;;; Details on internal value representation.
;;; Copyright (C) 2014, 2015, 2017-2019 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (system base types internal)
  #:export (;; Immediate tags.
            %fixnum-tag
            %fixnum-tag-mask
            %fixnum-tag-size
            %tc3-heap-object
            %tc8-char
            %tc16-false
            %tc16-nil
            %tc16-null
            %tc16-true
            %tc16-unspecified
            %tc16-undefined
            %tc16-eof
            visit-immediate-tags

            ;; Heap object tags (cell types).
            %tc4-non-pair-heap-object
            %tc5-struct
            %tc7-smob
            %tc11-symbol
            %tc11-variable
            %tc11-vector
            %tc12-immutable-vector
            %tc12-mutable-vector
            %tc11-weak-vector
            %tc11-string
            %tc11-heap-number
            %tc11-hash-table
            %tc11-pointer
            %tc11-fluid
            %tc11-stringbuf
            %tc11-dynamic-state
            %tc11-frame
            %tc11-keyword
            %tc11-atomic-box
            %tc11-syntax
            %tc11-program
            %tc11-vm-continuation
            %tc11-bytevector
            %tc11-weak-set
            %tc11-weak-table
            %tc11-array
            %tc11-bitvector
            %tc11-port
            %tc16-bignum
            %tc16-flonum
            %tc16-complex
            %tc16-fraction
            visit-heap-tags))

;;; Commentary:
;;;
;;; Tag values used to represent Scheme values, internally to Guile.
;;;
;;; Code:


;;;
;;; Tags---keep in sync with libguile/scm.h!
;;;

(define-syntax define-tags
  (lambda (x)
    (define (id-append ctx a b)
      (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b))))
    (syntax-case x ()
      ((_ tag-set (name pred mask tag) ...)
       #`(define-syntax #,(id-append #'tag-set #'visit- #'tag-set)
           (lambda (x)
             (define (introduce ctx id)
               (datum->syntax ctx (syntax->datum id)))
             (syntax-case x ()
               ((_ f)
                #`(begin
                    (f #,(introduce #'f #'name)
                       #,(introduce #'f #'pred)
                       mask
                       tag)
                    ...)))))))))

;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;; For now, this file defines tags for 64-bit word size.  TODO: support
;; tags that vary depending on the target word size.
(define-tags immediate-tags
  ;;                                    321076543210    321076543210
  (heap-object      heap-object?               #b111           #b000)
  (fixnum           fixnum?                   #b1111          #b1111)
  ;;(fixrat         fixrat?                   #b1111          #b0111)
  (char             char?                 #b11111111      #b00010110)
  (false            eq-false?         #b111111111111  #b000000000110)
  (nil              eq-nil?           #b111111111111  #b000100000110)
  (null             eq-null?          #b111111111111  #b001100000110)
  (true             eq-true?          #b111111111111  #b010000000110)
  (unspecified      unspecified?      #b111111111111  #b100000000110)
  (undefined        undefined?        #b111111111111  #b100100000110)
  (eof              eof-object?       #b111111111111  #b101000000110)

  ;;(false          eq-false?         #b111111111111  #b000000000110)
  ;;(nil            eq-nil?           #b111111111111  #b000100000110)
  ;;(null           eq-null?          #b111111111111  #b001100000110)
  (null+nil         null?             #b110111111111  #b000100000110)
  (false+nil        false?            #b111011111111  #b000000000110)
  (null+false+nil   nil?              #b110011111111  #b000000000110))

(define-tags heap-tags
  ;;                                        321076543210        321076543210
  (non-pair-heap-object
                    non-pair-heap-object?         #b1111              #b1110)
  (struct           struct?                      #b11111             #b11110)
  (smob             smob?                      #b1111111           #b1001110)
  (symbol           symbol?                #b11111111111       #b00000101110)
  (variable         variable?              #b11111111111       #b00001101110)
  (vector           vector?                #b11111111111       #b00010101110)
  (immutable-vector immutable-vector?     #b111111111111      #b100010101110)
  (mutable-vector   mutable-vector?       #b111111111111      #b000010101110)
  (weak-vector      weak-vector?           #b11111111111       #b00011101110)
  (string           string?                #b11111111111       #b00100101110)
  (heap-number      heap-number?           #b11111111111       #b00101101110)
  (hash-table       hash-table?            #b11111111111       #b00110101110)
  (pointer          pointer?               #b11111111111       #b00111101110)
  (fluid            fluid?                 #b11111111111       #b01000101110)
  (stringbuf        stringbuf?             #b11111111111       #b01001101110)
  (dynamic-state    dynamic-state?         #b11111111111       #b01010101110)
  (frame            frame?                 #b11111111111       #b01011101110)
  (keyword          keyword?               #b11111111111       #b01100101110)
  (atomic-box       atomic-box?            #b11111111111       #b01101101110)
  (syntax           syntax?                #b11111111111       #b01110101110)
  ;;(values         values?                #b11111111111       #b01111101110)
  (program          program?               #b11111111111       #b10000101110)
  (vm-continuation  vm-continuation?       #b11111111111       #b10001101110)
  (bytevector       bytevector?            #b11111111111       #b10010101110)
  (weak-set         weak-set?              #b11111111111       #b10011101110)
  (weak-table       weak-table?            #b11111111111       #b10100101110)
  (array            array?                 #b11111111111       #b10101101110)
  (bitvector        bitvector?             #b11111111111       #b10110101110)
  (port             port?                  #b11111111111       #b10111101110)
  ;;(unused         unused                 #b11111111111       #b11000101110)
  ;;(unused         unused                 #b11111111111       #b11001101110)
  ;;(unused         unused                 #b11111111111       #b11010101110)
  ;;(unused         unused                 #b11111111111       #b11011101110)
  ;;(unused         unused                 #b11111111111       #b11100101110)
  ;;(unused         unused                 #b11111111111       #b11101101110)
  ;;(unused         unused                 #b11111111111       #b11110101110)
  ;;(unused         unused                 #b11111111111       #b11111101110)

  ;(heap-number     heap-number?           #b11111111111       #b00101101110)
  (bignum           bignum?           #b1111111111111111  #b0001000101101110)
  (flonum           flonum?           #b1111111111111111  #b0010000101101110)
  (complex          compnum?          #b1111111111111111  #b0011000101101110)
  (fraction         fracnum?          #b1111111111111111  #b0100000101101110))

(eval-when (expand)
  (define configurable-width-tag-names
    '(fixnum #;fixrat #;heap-object #;struct))
  (define historic-tc16-names
    '(false nil null true unspecified undefined eof)))

(define-syntax define-tag
  (lambda (x)
    (define (id-append ctx . ids)
      (datum->syntax ctx (apply symbol-append (map syntax->datum ids))))
    (define (def prefix name tag)
      #`(define #,(id-append name prefix name) #,tag))
    (define (def* name mask tag)
      #`(begin
          (define #,(id-append name #'% name #'-tag-mask) #,mask)
          (define #,(id-append name #'% name #'-tag-size) (logcount #,mask))
          (define #,(id-append name #'% name #'-tag) #,tag)))
    (syntax-case x ()
      ((_ name pred mask tag)
       (member (syntax->datum #'name) configurable-width-tag-names)
       (def* #'name #'mask #'tag))
      ((_ name pred #b111 tag)           (def #'%tc3- #'name #'tag))
      ((_ name pred #b1111 tag)          (def #'%tc4- #'name #'tag))
      ((_ name pred #b11111 tag)         (def #'%tc5- #'name #'tag))
      ((_ name pred #b1111111 tag)       (def #'%tc7- #'name #'tag))
      ((_ name pred #b11111111 tag)      (def #'%tc8- #'name #'tag))
      ((_ name pred #b11111111111 tag)   (def #'%tc11- #'name #'tag))
      ;; Only 12 bits of mask but for historic reasons these are called
      ;; tc16 values.
      ((_ name pred #b111111111111 tag)
       (member (syntax->datum #'name) historic-tc16-names)
       (def #'%tc16- #'name #'tag))
      ((_ name pred #b111111111111 tag)      (def #'%tc12- #'name #'tag))
      ((_ name pred #b1111111111111111 tag)  (def #'%tc16- #'name #'tag))
      ((_ name pred mask tag)
       (def* #'name #'mask #'tag)))))

(visit-immediate-tags define-tag)
(visit-heap-tags define-tag)

;; See discussion in tags.h and boolean.h.
(eval-when (expand)
  (let ()
    (visit-immediate-tags define-tag)
    (define (exactly-one-bit-set? x)
      (and (not (zero? x)) (zero? (logand x (1- x)))))
    (define (exactly-two-bits-set? x)
      (exactly-one-bit-set? (logand x (1- x))))
    (define (bits-differ-in-exactly-one-bit-position? a b)
      (exactly-one-bit-set? (logxor a b)))
    (define (bits-differ-in-exactly-two-bit-positions? a b)
      (exactly-two-bits-set? (logxor a b)))
    (define (common-bits a b)
      (values (logand #xfff (lognot (logxor a b))) (logand a b)))

    (unless (bits-differ-in-exactly-one-bit-position? %tc16-null %tc16-nil)
      (error "expected #nil and '() to differ in exactly one bit position"))
    (unless (bits-differ-in-exactly-one-bit-position? %tc16-false %tc16-nil)
      (error "expected #f and '() to differ in exactly one bit position"))
    (unless (bits-differ-in-exactly-two-bit-positions? %tc16-false %tc16-null)
      (error "expected #f and '() to differ in exactly two bit positions"))
    (call-with-values (lambda () (common-bits %tc16-null %tc16-nil))
      (lambda (mask tag)
        (unless (= mask %null+nil-tag-mask) (error "unexpected mask for null?"))
        (unless (= tag %null+nil-tag) (error "unexpected tag for null?"))))
    (call-with-values (lambda () (common-bits %tc16-false %tc16-nil))
      (lambda (mask tag)
        (unless (= mask %false+nil-tag-mask) (error "unexpected mask for false?"))
        (unless (= tag %false+nil-tag) (error "unexpected tag for false?"))))
    (call-with-values (lambda () (common-bits %tc16-false %tc16-null))
      (lambda (mask tag)
        (unless (= mask %null+false+nil-tag-mask) (error "unexpected mask for nil?"))
        (unless (= tag %null+false+nil-tag) (error "unexpected tag for nil?"))))))
