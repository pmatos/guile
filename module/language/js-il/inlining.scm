;;; JavaScript Intermediate Language (JS-IL) Inliner

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

;; FIXME: It is currently wrong to think of inlining as an optimisation
;; since in the cps-soup world we need inlining to rebuild the scope
;; tree for variables.
;; FIXME: since *all* conts are passed to each clause, there can be
;; "dead" conts thare are included in a clause

(define-module (language js-il inlining)
  #:use-module ((srfi srfi-1) #:select (partition))
  #:use-module (ice-9 match)
  #:use-module (language js-il)
  #:export (count-calls
            inline-single-calls
            ))

(define (count-calls exp)
  (define counts (make-hash-table))
  (define (count-inc! key)
    (hashv-set! counts key (+ 1 (hashv-ref counts key 0))))
  (define (count-inf! key)
    (hashv-set! counts key +inf.0))
  (define (analyse-args arg-list)
    (for-each (match-lambda
               (($ kid name)
                (count-inf! name))
               (($ id name) #f))
              arg-list))
  (define (analyse exp)
    (match exp
      (($ program ((ids . funs) ...))
       (for-each analyse funs))

      (($ function self tail ((($ kid ids) _ bodies) ...))
       (for-each count-inc! ids) ;; count-inf! ?
       (for-each analyse bodies))

      (($ continuation params body)
       (analyse body))

      (($ local bindings body)
       (for-each (match-lambda
                  ((i . b) (analyse b)))
                 bindings)
       (analyse body))

      (($ continue ($ kid cont) args)
       (count-inc! cont)
       (for-each analyse args))

      (($ primcall name args)
       (analyse-args args))

      (($ call name ($ kid k) args)
       (count-inf! k)
       (analyse-args args))

      (($ closure ($ kid label) num-free)
       (count-inf! label))

      (($ branch test consequence alternate)
       (analyse test)
       (analyse consequence)
       (analyse alternate))

      (($ kid name)
       (count-inf! name))

      (($ seq body)
       (for-each analyse body))

      (($ prompt escape? tag ($ kid handler))
       (count-inf! handler))

      (else #f)))
  (analyse exp)
  counts)

(define no-values-primitives
  '(
    cache-current-module!
    set-cdr!
    set-car!
    vector-set!
    free-set!
    vector-set!/immediate
    box-set!
    struct-set!
    struct-set!/immediate
    wind
    unwind
    push-fluid
    pop-fluid
    handle-interrupts
    push-dynamic-state
    pop-dynamic-state
    fluid-set!
    ))

(define no-values-primitive?
  (let ((h (make-hash-table)))
    (for-each (lambda (prim)
                (hashv-set! h prim #t))
              no-values-primitives)
    (lambda (prim)
      (hashv-ref h prim))))


(define (inline-single-calls exp)
  (define (handle-function fun)
    (match fun
      (($ function self tail ((ids params bodies) ...))
       (make-function self
                      tail
                      (map (lambda (id param body)
                             (list id param (inline-clause body)))
                           ids
                           params
                           bodies)))))
  (match exp
    (($ program ((ids . funs) ...))
     (make-program (map (lambda (id fun)
                          (cons id (handle-function fun)))
                        ids
                        funs)))))

(define (inline-clause exp)

  (define calls (count-calls exp))

  (define (inlinable? k)
    (eqv? 1 (hashv-ref calls k)))

  (define (split-inlinable bindings)
    (partition (match-lambda
                ((($ kid id) . _) (inlinable? id)))
               bindings))

  (define (lookup kont substs)
    (match substs
      (((($ kid id) . exp) . rest)
       (if (= id kont)
           exp
           (lookup kont rest)))
      (() kont)
      (else
       (throw 'lookup-failed kont))))

  (define (inline exp substs)
    (match exp

      ;; FIXME: This hacks around the fact that define doesn't return
      ;; arguments to the continuation. This should be handled when
      ;; converting to js-il, not here.
      (($ continue
          ($ kid (? inlinable? cont))
          (($ primcall (? no-values-primitive? prim) args)))
       (match (lookup cont substs)
         (($ continuation () body)
          (make-seq
           (list
            (make-primcall prim args)
            (inline body substs))))
         (else
          ;; inlinable but not locally bound
          exp)))

      (($ continue ($ kid (? inlinable? cont)) args)
       (match (lookup cont substs)
         (($ continuation kargs body)
          (if (not (= (length args) (length kargs)))
              (throw 'args-dont-match cont args kargs)
              (make-local (map cons kargs args)
                          ;; gah, this doesn't work
                          ;; identifiers need to be separated earlier
                          ;; not just as part of compilation
                          (inline body substs))))
         (else
          ;; inlinable but not locally bound
          ;; FIXME: This handles tail continuations, but only by accident
          exp)))

      (($ continue cont args)
       exp)

      (($ continuation params body)
       (make-continuation params (inline body substs)))

      (($ local bindings body)
       (call-with-values
           (lambda ()
             (split-inlinable bindings))
         (lambda (new-substs uninlinable-bindings)
           (define substs* (append new-substs substs))
           (make-local (map (match-lambda
                             ((id . val)
                              `(,id . ,(inline val substs*))))
                            uninlinable-bindings)
                       (inline body substs*)))))

      (($ seq body)
       (make-seq (map (lambda (x) (inline x substs))
                      body)))

      (($ branch test consequence alternate)
       (make-branch test
                    (inline consequence substs)
                    (inline alternate substs)))

      (exp exp)))

  (inline exp '()))
