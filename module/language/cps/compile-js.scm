;;; Continuation-passing style (CPS) to JS-IL compiler

;; Copyright (C) 2015, 2017 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language cps compile-js)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps utils)
  #:use-module ((language js-il)
                #:renamer (lambda (x) (if (eqv? x 'make-prompt) 'make-prompt* x)))
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:export (compile-js))

(define intmap-select (@@ (language cps compile-bytecode) intmap-select))
(define lower-cps (@@ (language cps optimize) lower-cps))

(define (compile-js exp env opts)
  ;; TODO: I should special case the compilation for the initial fun,
  ;; as this is the entry point for the program, and shouldn't get a
  ;; "self" argument, for now, I add "undefined" as the first
  ;; argument in the call to it.
  ;; see compile-exp in (language js-il compile-javascript)
  (define (intmap->program map)
    (intmap-fold-right (lambda (kfun body accum)
                         (acons (make-kid kfun)
                                (compile-fun (intmap-select map body) kfun)
                                accum))
                       (compute-reachable-functions map 0)
                       '()))
  (values (make-program (intmap->program (lower-cps exp opts))) env env))


(define (compile-fun cps kfun)
  (define doms (compute-dom-edges (compute-idoms cps kfun)))
  (match (intmap-ref cps kfun)
    (($ $kfun src meta self tail clause)
     (make-function
      (make-id self)
      (make-kid tail)
      (compile-clauses cps doms clause self)))))


(define (extract-and-compile-conts cps)
  (define (step id body accum)
    (match body
      ;; The term in a $kargs is always a $continue
      (($ $kargs names syms ($ $continue k src exp))
       (acons (make-kid id)
              (make-continuation (map make-id syms) (compile-exp exp k))
              accum))
      (($ $kreceive ($ $arity req _ (? symbol? rest) _ _) k2)
       (let ((ids (map make-id (append req (list rest)))))
         (acons (make-kid id)
                (make-continuation ids (make-continue (make-kid k2) ids))
                accum)))
      (($ $kreceive ($ $arity req _ #f _ _) k2)
       (let ((ids (map make-id req)))
         (acons (make-kid id)
                (make-continuation ids (make-continue (make-kid k2) ids))
                accum)))
      (else accum)))
  (intmap-fold step cps '()))


(define (compile-clauses cps doms clause self)
  ;; FIXME: This duplicates all the conts in each clause, and requires
  ;; the inliner to remove them. A better solution is to change the
  ;; function type to contain a separate map of conts, but this requires
  ;; more code changes, and is should constitute a separate commit.
  (let loop ((clause clause))
   (match (intmap-ref cps clause)
     (($ $kclause arity body #f)
      `((,(make-kid clause)
         ,(arity->params arity self)
         ,(compile-clause cps doms arity body self))))
     (($ $kclause arity body next)
      `((,(make-kid clause)
         ,(arity->params arity self)
         ,(compile-clause cps doms arity body self))
        . ,(loop next))))))


(define (arity->params arity self)
  (match arity
    (($ $arity req opts rest ((kws names kw-syms) ...) allow-other-keys?)
     (make-params (make-id self)
                  (map make-id req)
                  (map make-id opts)
                  (and rest (make-id rest))
                  (map (lambda (kw name kw-sym)
                         (list kw (make-id name) (make-id kw-sym)))
                       kws
                       names
                       kw-syms)
                  allow-other-keys?))))


(define (compile-clause cps doms arity body self)
  (match arity
    (($ $arity req opt rest ((_ _ kw-syms) ...) _)
     (let ((ids (map make-id
                     (append req opt kw-syms (if rest (list rest) '())))))
       (make-continuation
        (cons (make-id self) ids)
        (make-local (list (cons (make-kid body) (compile-cont cps doms body)))
                    (make-continue (make-kid body) ids)))))))

(define (compile-cont cps doms cont)
  (define (redominate label exp)
    ;; This ensures that functions which are dominated by a $kargs [e.g.
    ;; because they need its arguments] are moved into its body, and so
    ;; we get correct scoping.
    (define (find&compile-dominated label)
      (append-map (lambda (label)
                    (match (intmap-ref cps label)
                      (($ $ktail) '()) ; ignore tails
                      (($ $kargs)
                       ;; kargs may bind more arguments
                       (list (cons (make-kid label) (compile label))))
                      (else
                       ;; otherwise, even if it dominates other conts,
                       ;; it doesn't need to contain them
                       (cons (cons (make-kid label) (compile label))
                             (find&compile-dominated label)))))
                  (intmap-ref doms label)))
    (make-local (find&compile-dominated label) exp))
  (define (compile cont)
    (match (intmap-ref cps cont)
      ;; The term in a $kargs is always a $continue
      (($ $kargs names syms ($ $continue k src exp))
       (make-continuation (map make-id syms)
                          (redominate cont (compile-exp exp k))))
      (($ $kreceive ($ $arity req _ (? symbol? rest) _ _) k2)
       (let ((ids (map make-id (append req (list rest)))))
         (make-continuation ids (make-continue (make-kid k2) ids))))
      (($ $kreceive ($ $arity req _ #f _ _) k2)
       (let ((ids (map make-id req)))
         (make-continuation ids (make-continue (make-kid k2) ids))))))
  (compile cont))

(define (compile-exp exp k)
 (match exp
    (($ $branch kt exp)
     (compile-test exp (make-kid kt) (make-kid k)))
    (($ $primcall 'return (arg))
     (make-continue (make-kid k) (list (make-id arg))))
    (($ $call name args)
     (make-call (make-id name) (make-kid k) (map make-id args)))
    (($ $callk label proc args)
     (make-continue (make-kid label)
                    (cons* (make-id proc)
                           (make-kid k)
                           (map make-id args))))
    (($ $values values)
     (make-continue (make-kid k) (map make-id values)))
    (($ $prompt escape? tag handler)
     (make-seq
      (list
       (make-prompt* escape? (make-id tag) (make-kid handler))
       (make-continue (make-kid k) '()))))
    (_
     (make-continue (make-kid k) (list (compile-exp* exp))))))

(define (compile-exp* exp)
  (match exp
    (($ $const val)
     (make-const val))
    (($ $primcall name args)
     (make-primcall name (map make-id args)))
    (($ $closure label nfree)
     (make-closure (make-kid label) nfree))
    (($ $values (val))
     ;; FIXME:
     ;; may happen if a test branch of a conditional compiles to values
     ;; placeholder till I learn if multiple values could be returned.
     (make-id val))))

(define (compile-test exp kt kf)
  ;; TODO: find out if the expression is always simple enough that I
  ;; don't need to create a new continuation (which will require extra
  ;; arguments being passed through)
  (make-branch (compile-exp* exp)
               (make-continue kt '())
               (make-continue kf '())))
