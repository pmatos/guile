;;; Compilation targets

;; Copyright (C) 2011-2014,2017-2019 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(define-module (system base target)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 regex)
  #:export (target-type with-target with-native-target

            target-cpu target-vendor target-os

            target-endianness target-word-size

            target-max-size-t
            target-max-size-t/scm
            target-max-vector-length

            target-most-negative-fixnum
            target-most-positive-fixnum
            target-fixnum?

            target-fixnum-tag
            target-fixnum-tag-mask
            target-fixnum-tag-bits

            target-pair-tag
            target-pair-tag-mask
            target-pair-tag-bits))



;;;
;;; Target types
;;;

(define %native-word-size
  ((@ (system foreign) sizeof) '*))

(define %target-type (make-fluid %host-type))
(define %target-endianness (make-fluid (native-endianness)))
(define %target-word-size (make-fluid %native-word-size))

(define (validate-target target)
  (if (or (not (string? target))
          (let ((parts (string-split target #\-)))
            (or (< (length parts) 3)
                (or-map string-null? parts))))
      (error "invalid target" target)))

(define (with-target target thunk)
  (validate-target target)
  (let ((cpu (triplet-cpu target)))
    (with-fluids ((%target-type target)
                  (%target-endianness (cpu-endianness cpu))
                  (%target-word-size (triplet-pointer-size target)))
      (thunk))))

(define (with-native-target thunk)
  (with-fluids ((%target-type %host-type)
                (%target-endianness (native-endianness))
                (%target-word-size %native-word-size))
    (thunk)))

(define (cpu-endianness cpu)
  "Return the endianness for CPU."
  (if (string=? cpu (triplet-cpu %host-type))
      (native-endianness)
      (cond ((string-match "^i[0-9]86$" cpu)
             (endianness little))
            ((member cpu '("x86_64" "ia64"
                           "powerpcle" "powerpc64le" "mipsel" "mips64el" "nios2" "sh3" "sh4" "alpha"))
             (endianness little))
            ((member cpu '("sparc" "sparc64" "powerpc" "powerpc64" "spu"
                           "mips" "mips64" "m68k" "s390x"))
             (endianness big))
            ((string-match "^arm.*el" cpu)
             (endianness little))
            ((string-match "^arm.*eb" cpu)
             (endianness big))
            ((string-prefix? "arm" cpu)          ;ARMs are LE by default
             (endianness little))
            ((string-match "^aarch64.*be" cpu)
             (endianness big))
            ((string=? "aarch64" cpu)
             (endianness little))
            ((string-match "riscv[1-9][0-9]*" cpu)
             (endianness little))
            (else
             (error "unknown CPU endianness" cpu)))))

(define (triplet-pointer-size triplet)
  "Return the size of pointers in bytes for TRIPLET."
  (let ((cpu (triplet-cpu triplet)))
    (cond ((and (string=? cpu (triplet-cpu %host-type))
                (string=? (triplet-os triplet) (triplet-os %host-type)))
           %native-word-size)

          ((string-match "^i[0-9]86$" cpu) 4)

          ;; Although GNU config.guess doesn't yet recognize them,
          ;; Debian (ab)uses the OS part to denote the specific ABI
          ;; being used: <http://wiki.debian.org/Multiarch/Tuples>.
          ;; See <http://www.linux-mips.org/wiki/WhatsWrongWithO32N32N64>
          ;; for details on the MIPS ABIs.
          ((string-match "^mips64.*-gnuabi64" triplet) 8) ; n64 ABI
          ((string-match "^mips64" cpu) 4)                ; n32 or o32

          ((string-match "^x86_64-.*-gnux32" triplet) 4)  ; x32

          ((string-match "64$" cpu) 8)
          ((string-match "64_?[lbe][lbe]$" cpu) 8)
          ((member cpu '("sparc" "powerpc" "mips" "mipsel" "nios2" "m68k" "sh3" "sh4")) 4)
          ((member cpu '("s390x" "alpha")) 8)
          ((string-match "^arm.*" cpu) 4)
          (else (error "unknown CPU word size" cpu)))))

(define (triplet-cpu t)
  (substring t 0 (string-index t #\-)))

(define (triplet-vendor t)
  (let ((start (1+ (string-index t #\-))))
    (substring t start (string-index t #\- start))))

(define (triplet-os t)
  (let ((start (1+ (string-index t #\- (1+ (string-index t #\-))))))
    (substring t start)))


(define (target-type)
  "Return the GNU configuration triplet of the target platform."
  (fluid-ref %target-type))

(define (target-cpu)
  "Return the CPU name of the target platform."
  (triplet-cpu (target-type)))

(define (target-vendor)
  "Return the vendor name of the target platform."
  (triplet-vendor (target-type)))

(define (target-os)
  "Return the operating system name of the target platform."
  (triplet-os (target-type)))

(define (target-endianness)
  "Return the endianness object of the target platform."
  (fluid-ref %target-endianness))

(define (target-word-size)
  "Return the word size, in bytes, of the target platform."
  (fluid-ref %target-word-size))

(define (target-max-size-t)
  "Return the maximum size_t value of the target platform, in bytes."
  ;; Apply the currently-universal restriction of a maximum 48-bit
  ;; address space.
  (1- (ash 1 (min (* (target-word-size) 8) 48))))

(define (target-max-size-t/scm)
  "Return the maximum size_t value of the target platform, in units of
SCM words."
  ;; Apply the currently-universal restriction of a maximum 48-bit
  ;; address space.
  (/ (target-max-size-t) (target-word-size)))

;; TAGS-SENSITIVE
(define (target-max-vector-length)
  "Return the maximum vector length of the target platform, in units of
SCM words."
  ;; Vector size fits in first word; the low 8 bits are taken by the
  ;; type tag.  Additionally, restrict to 48-bit address space.
  (1- (ash 1 (min (- (* (target-word-size) 8) 8) 48))))

;; TAGS-SENSITIVE
(define (target-most-negative-fixnum)
  "Return the most negative integer representable as a fixnum on the
target platform."
  (case (target-word-size)
    ((4) #x-40000000)
    ((8) #x-800000000000000)
    (else (error "unexpected word size"))))

;; TAGS-SENSITIVE
(define (target-most-positive-fixnum)
  "Return the most positive integer representable as a fixnum on the
target platform."
  (case (target-word-size)
    ((4) #x3fffffff)
    ((8) #x7ffffffFFFFFFFF)
    (else (error "unexpected word size"))))

;; TAGS-SENSITIVE
(define (target-fixnum? n)
  (and (exact-integer? n)
       (<= (target-most-negative-fixnum)
           n
           (target-most-positive-fixnum))))

;; TAGS-SENSITIVE
(define (target-fixnum-tag)
  "Return the fixnum tag on the target platform."
  (case (target-word-size)
    ((4) 1)
    ((8) 15)
    (else (error "unexpected word size"))))

;; TAGS-SENSITIVE
(define (target-fixnum-tag-mask)
  "Return the fixnum tag mask on the target platform."
  (case (target-word-size)
    ((4) 1)
    ((8) 15)
    (else (error "unexpected word size"))))

;; TAGS-SENSITIVE
(define (target-fixnum-tag-bits)
  "Return the number of bits in the fixnum tag mask on the target platform."
  (case (target-word-size)
    ((4) 1)
    ((8) 4)
    (else (error "unexpected word size"))))

;; TAGS-SENSITIVE
(define (target-pair-tag)
  "Return the pair tag on the target platform."
  (case (target-word-size)
    ((4) 4)
    ((8) 6)
    (else (error "unexpected word size"))))

;; TAGS-SENSITIVE
(define (target-pair-tag-mask)
  "Return the pair tag mask on the target platform."
  (case (target-word-size)
    ((4) 7)
    ((8) 15)
    (else (error "unexpected word size"))))

;; TAGS-SENSITIVE
(define (target-pair-tag-bits)
  "Return the number of bits in the pair tag mask on the target platform."
  (case (target-word-size)
    ((4) 3)
    ((8) 4)
    (else (error "unexpected word size"))))
