;;;; ports.scm --- R6RS port API                    -*- coding: utf-8 -*-

;;;;	Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Author: Ludovic Courtès <ludo@gnu.org>

;;; Commentary:
;;;
;;; The I/O port API of the R6RS is provided by this module.  In many areas
;;; it complements or refines Guile's own historical port API.  For instance,
;;; it allows for binary I/O with bytevectors.
;;;
;;; Code:

(library (rnrs io ports (6))
  (export eof-object eof-object?

          ;; auxiliary types
          file-options buffer-mode buffer-mode?
          eol-style native-eol-style error-handling-mode
          make-transcoder transcoder-codec native-transcoder
          latin-1-codec utf-8-codec utf-16-codec
           
          ;; input & output ports
          port? input-port? output-port?
          port-transcoder binary-port? transcoded-port
          port-position set-port-position!
          port-has-port-position? port-has-set-port-position!?
          call-with-port close-port

          ;; input ports
          open-bytevector-input-port
          open-string-input-port
          open-file-input-port
          make-custom-binary-input-port

          ;; binary input
          get-u8 lookahead-u8
          get-bytevector-n get-bytevector-n!
          get-bytevector-some get-bytevector-all

          ;; output ports
          open-bytevector-output-port
          open-string-output-port
          open-file-output-port
          make-custom-binary-output-port
          call-with-bytevector-output-port
          call-with-string-output-port
          make-custom-textual-output-port
          flush-output-port
           
          ;; binary output
          put-u8 put-bytevector

          ;; textual input
          get-char get-datum get-line get-string-all lookahead-char
           
          ;; textual output
          put-char put-datum put-string

          ;; standard ports
          standard-input-port standard-output-port standard-error-port

          ;; condition types
          &i/o i/o-error? make-i/o-error
          &i/o-read i/o-read-error? make-i/o-read-error
          &i/o-write i/o-write-error? make-i/o-write-error
          &i/o-invalid-position i/o-invalid-position-error?
          make-i/o-invalid-position-error
          &i/o-filename i/o-filename-error? make-i/o-filename-error
          i/o-error-filename
          &i/o-file-protection i/o-file-protection-error?
          make-i/o-file-protection-error
          &i/o-file-is-read-only i/o-file-is-read-only-error?
          make-i/o-file-is-read-only-error
          &i/o-file-already-exists i/o-file-already-exists-error?
          make-i/o-file-already-exists-error
          &i/o-file-does-not-exist i/o-file-does-not-exist-error?
          make-i/o-file-does-not-exist-error
          &i/o-port i/o-port-error? make-i/o-port-error
          i/o-error-port)
  (import (only (rnrs base) assertion-violation)
          (rnrs enums)
          (rnrs records syntactic)
          (rnrs exceptions)
          (rnrs conditions)
          (rnrs files) ;for the condition types
          (srfi srfi-8)
          (ice-9 rdelim)
          (except (guile) raise))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_r6rs_ports")



;;;
;;; Auxiliary types
;;;

(define-enumeration file-option
  (no-create no-fail no-truncate)
  file-options)

(define-enumeration buffer-mode
  (none line block)
  buffer-modes)

(define (buffer-mode? symbol)
  (enum-set-member? symbol (enum-set-universe (buffer-modes))))

(define-enumeration eol-style
  (lf cr crlf nel crnel ls)
  eol-styles)

(define (native-eol-style)
  (eol-style lf))

(define-enumeration error-handling-mode
  (ignore raise replace)
  error-handling-modes)

(define-record-type (transcoder %make-transcoder transcoder?)
  (fields codec eol-style error-handling-mode))

(define* (make-transcoder codec
                          #:optional
                          (eol-style (native-eol-style))
                          (handling-mode (error-handling-mode replace)))
  (%make-transcoder codec eol-style handling-mode))

(define (native-transcoder)
  (make-transcoder (or (fluid-ref %default-port-encoding)
                       (latin-1-codec))))

(define (latin-1-codec)
  "ISO-8859-1")

(define (utf-8-codec)
  "UTF-8")

(define (utf-16-codec)
  "UTF-16")


;;;
;;; Internal helpers
;;;

(define (with-i/o-filename-conditions filename thunk)
  (catch 'system-error
         thunk
         (lambda args
           (let ((errno (system-error-errno args)))
             (let ((construct-condition
                    (cond ((= errno EACCES)
                           make-i/o-file-protection-error)
                          ((= errno EEXIST)
                           make-i/o-file-already-exists-error)
                          ((= errno ENOENT)
                           make-i/o-file-does-not-exist-error)
                          ((= errno EROFS)
                           make-i/o-file-is-read-only-error)
                          (else
                           make-i/o-filename-error))))
               (raise (construct-condition filename)))))))


;;;
;;; Input and output ports.
;;;

(define (port-transcoder port)
  (error "port transcoders are not supported" port))

(define (binary-port? port)
  ;; So far, we don't support transcoders other than the binary transcoder.
  #t)

(define (transcoded-port port transcoder)
  "Return a new textual port based on @var{port}, using
@var{transcoder} to encode and decode data written to or
read from its underlying binary port @var{port}."
  (let ((result (%make-transcoded-port port)))
    (set-port-encoding! result (transcoder-codec transcoder))
    result))

(define (port-position port)
  "Return the offset (an integer) indicating where the next octet will be
read from/written to in @var{port}."

  ;; FIXME: We should raise an `&assertion' error when not supported.
  (seek port 0 SEEK_CUR))

(define (set-port-position! port offset)
  "Set the position where the next octet will be read from/written to
@var{port}."

  ;; FIXME: We should raise an `&assertion' error when not supported.
  (seek port offset SEEK_SET))

(define (port-has-port-position? port)
  "Return @code{#t} is @var{port} supports @code{port-position}."
  (and (false-if-exception (port-position port)) #t))

(define (port-has-set-port-position!? port)
  "Return @code{#t} is @var{port} supports @code{set-port-position!}."
  (and (false-if-exception (set-port-position! port (port-position port)))
       #t))

(define (call-with-port port proc)
  "Call @var{proc}, passing it @var{port} and closing @var{port} upon exit of
@var{proc}.  Return the return values of @var{proc}."
  (call-with-values
      (lambda () (proc port))
    (lambda vals
      (close-port port)
      (apply values vals))))

(define* (call-with-bytevector-output-port proc #:optional (transcoder #f))
  (receive (port extract) (open-bytevector-output-port transcoder)
    (call-with-port port proc)
    (extract)))

(define (open-string-input-port str)
  "Open an input port that will read from @var{str}."
  (with-fluids ((%default-port-encoding "UTF-8"))
    (open-input-string str)))

(define* (open-file-input-port filename
                               #:optional
                               (file-options (file-options))
                               (buffer-mode (buffer-mode block))
                               maybe-transcoder)
  (let ((port (with-i/o-filename-conditions filename
                (lambda () (open filename O_RDONLY)))))
    (cond (maybe-transcoder
           (set-port-encoding! port (transcoder-codec maybe-transcoder))))
    port))

(define (open-string-output-port)
  "Return two values: an output port that will collect characters written to it
as a string, and a thunk to retrieve the characters associated with that port."
  (let ((port (with-fluids ((%default-port-encoding "UTF-8"))
                (open-output-string))))
    (values port
            (lambda () (get-output-string port)))))

(define* (open-file-output-port filename
                                #:optional
                                (file-options (file-options))
                                (buffer-mode (buffer-mode block))
                                maybe-transcoder)
  (let* ((flags (logior O_WRONLY
                        (if (enum-set-member? 'no-create file-options)
                            0
                            O_CREAT)
                        (if (enum-set-member? 'no-truncate file-options)
                            0
                            O_TRUNC)))
         (port (with-i/o-filename-conditions filename
                 (lambda () (open filename flags)))))
    (cond (maybe-transcoder
           (set-port-encoding! port (transcoder-codec maybe-transcoder))))
    port))

(define (call-with-string-output-port proc)
  "Call @var{proc}, passing it a string output port. When @var{proc} returns,
return the characters accumulated in that port."
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))

(define (make-custom-textual-output-port id
                                         write!
                                         get-position
                                         set-position!
                                         close)
  (make-soft-port (vector (lambda (c) (write! (string c) 0 1))
                          (lambda (s) (write! s 0 (string-length s)))
                          #f ;flush
                          #f ;read character
                          close)
                  "w"))

(define (flush-output-port port)
  (force-output port))

(define (put-char port char)
  (write-char char port))

(define (put-datum port datum)
  (write datum port))

(define* (put-string port s #:optional start count)
  (cond ((not (string? s))
         (assertion-violation 'put-string "expected string" s))
        ((and start count)
         (display (substring/shared s start (+ start count)) port))
        (start
         (display (substring/shared s start (string-length s)) port))
        (else
         (display s port))))

(define (get-char port)
  (read-char port))

(define (get-datum port)
  (read port))

(define (get-line port)
  (read-line port 'trim))

(define (get-string-all port)
  (read-delimited "" port 'concat))

(define (lookahead-char port)
  (peek-char port))



(define (standard-input-port)
  (dup->inport 0))

(define (standard-output-port)
  (dup->outport 1))

(define (standard-error-port)
  (dup->outport 2))

)

;;; ports.scm ends here
