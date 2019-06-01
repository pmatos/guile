;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2009-2015, 2018, 2019 Free Software Foundation, Inc.
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
;;;;



;;; Commentary:

;;; Scheme eval, written in Scheme.
;;;
;;; Expressions are first expanded, by the syntax expander (i.e.
;;; psyntax), then memoized into internal forms. The evaluator itself
;;; only operates on the internal forms ("memoized expressions").
;;;
;;; Environments are represented as a chain of vectors, linked through
;;; their first elements.  The terminal element of an environment is the
;;; module that was current when the outer lexical environment was
;;; entered.
;;;

;;; Code:



(define (primitive-eval exp)
  "Evaluate @var{exp} in the current module."
  (define-syntax env-toplevel
    (syntax-rules ()
      ((_ env)
       (let lp ((e env))
         (if (vector? e)
             (lp (vector-ref e 0))
             e)))))

  (define-syntax make-env
    (syntax-rules ()
      ((_ n init next)
       (let ((v (make-vector (1+ n) init)))
         (vector-set! v 0 next)
         v))))

  (define-syntax make-env*
    (syntax-rules ()
      ((_ next init ...)
       (vector next init ...))))

  (define-syntax env-ref
    (syntax-rules ()
      ((_ env depth width)
       (let lp ((e env) (d depth))
         (if (zero? d)
             (vector-ref e (1+ width))
             (lp (vector-ref e 0) (1- d)))))))

  (define-syntax env-set!
    (syntax-rules ()
      ((_ env depth width val)
       (let lp ((e env) (d depth))
         (if (zero? d)
             (vector-set! e (1+ width) val)
             (lp (vector-ref e 0) (1- d)))))))

  ;; This is a modified version of Oleg Kiselyov's "pmatch".
  (define-syntax-rule (match e cs ...)
    (let ((v e)) (expand-clauses v cs ...)))

  (define-syntax expand-clauses
    (syntax-rules ()
      ((_ v) ((error "unreachable")))
      ((_ v (pat e0 e ...) cs ...)
       (let ((fk (lambda () (expand-clauses v cs ...))))
         (expand-pattern v pat (let () e0 e ...) (fk))))))

  (define-syntax expand-pattern
    (syntax-rules (_ quote unquote ?)
      ((_ v _ kt kf) kt)
      ((_ v () kt kf) (if (null? v) kt kf))
      ((_ v (quote lit) kt kf)
       (if (equal? v (quote lit)) kt kf))
      ((_ v (unquote exp) kt kf)
       (if (equal? v exp) kt kf))
      ((_ v (x . y) kt kf)
       (if (pair? v)
           (let ((vx (car v)) (vy (cdr v)))
             (expand-pattern vx x (expand-pattern vy y kt kf) kf))
           kf))
      ((_ v (? pred var) kt kf)
       (if (pred v) (let ((var v)) kt) kf))
      ((_ v #f kt kf) (if (eqv? v #f) kt kf))
      ((_ v var kt kf) (let ((var v)) kt))))

  (define-syntax typecode
    (lambda (x)
      (syntax-case x ()
        ((_ type)
         (or (memoized-typecode (syntax->datum #'type))
             (error "not a typecode" (syntax->datum #'type)))))))

  (define (annotate src proc)
    (set-procedure-property! proc 'source-override src)
    proc)

  (define-syntax-rule (lambda@ src formals body bodies ...)
    (annotate src (lambda formals body bodies ...)))

  (define-syntax-rule (lazy src (arg ...) exp)
    (letrec ((proc (lambda (arg ...)
                     (set! proc exp)
                     (proc arg ...))))
      (lambda@ src (arg ...)
        (proc arg ...))))

  (define (compile-lexical-ref src depth width)
    (case depth
      ((0) (lambda@ src (env) (env-ref env 0 width)))
      ((1) (lambda@ src (env) (env-ref env 1 width)))
      ((2) (lambda@ src (env) (env-ref env 2 width)))
      (else (lambda@ src (env) (env-ref env depth width)))))

  (define (primitive=? name loc module var)
    "Return true if VAR is the same as the primitive bound to NAME."
    (match loc
      ((mode . loc)
       (and (match loc
              ((mod name* . public?) (eq? name* name))
              (_ (eq? loc name)))
            ;; `module' can be #f if the module system was not yet
            ;; booted when the environment was captured.
            (or (not module)
                (eq? var (module-local-variable the-root-module name)))))))

  (define (compile-top-call src cenv loc args)
    (let* ((module (env-toplevel cenv))
           (var (%resolve-variable loc module)))
      (define-syntax-rule (maybe-primcall (prim ...) arg ...)
        (let ((arg (compile arg))
              ...)
          (cond
           ((primitive=? 'prim loc module var)
            (lambda@ src (env) (prim (arg env) ...)))
           ...
           (else (lambda@ src (env) ((variable-ref var) (arg env) ...))))))
      (match args
        (()
         (lambda@ src (env) ((variable-ref var))))
        ((a)
         (maybe-primcall (1+ 1- car cdr lognot vector-length
                          variable-ref string-length struct-vtable)
                         a))
        ((a b)
         (maybe-primcall (+ - * / ash logand logior logxor
                          cons vector-ref struct-ref variable-set!)
                         a b))
        ((a b c)
         (maybe-primcall (vector-set! struct-set!) a b c))
        ((a b c . args)
         (let ((a (compile a))
               (b (compile b))
               (c (compile c))
               (args (let lp ((args args))
                       (if (null? args)
                           '()
                           (cons (compile (car args)) (lp (cdr args)))))))
           (lambda@ src (env)
             (apply (variable-ref var) (a env) (b env) (c env)
                    (let lp ((args args))
                      (if (null? args)
                          '()
                          (cons ((car args) env) (lp (cdr args))))))))))))

  (define (compile-call src f args)
    (match f
      ((,(typecode box-ref) _ . (,(typecode resolve) _ . loc))
       (lazy src (env) (compile-top-call src env loc args)))
      (_
       (match args
         (()
          (let ((f (compile f)))
            (lambda@ src (env) ((f env)))))
         ((a)
          (let ((f (compile f))
                (a (compile a)))
            (lambda@ src (env) ((f env) (a env)))))
         ((a b)
          (let ((f (compile f))
                (a (compile a))
                (b (compile b)))
            (lambda@ src (env) ((f env) (a env) (b env)))))
         ((a b c)
          (let ((f (compile f))
                (a (compile a))
                (b (compile b))
                (c (compile c)))
            (lambda@ src (env) ((f env) (a env) (b env) (c env)))))
         ((a b c . args)
          (let ((f (compile f))
                (a (compile a))
                (b (compile b))
                (c (compile c))
                (args (let lp ((args args))
                        (if (null? args)
                            '()
                            (cons (compile (car args)) (lp (cdr args)))))))
            (lambda@ src (env)
              (apply (f env) (a env) (b env) (c env)
                     (let lp ((args args))
                       (if (null? args)
                           '()
                           (cons ((car args) env) (lp (cdr args)))))))))))))

  (define (compile-box-ref src box)
    (match box
      ((,(typecode resolve) _ . loc)
       (lazy src (cenv)
             (let ((var (%resolve-variable loc (env-toplevel cenv))))
               (lambda@ src (env) (variable-ref var)))))
      ((,(typecode lexical-ref) _ depth . width)
       (lambda@ src (env)
         (variable-ref (env-ref env depth width))))
      (_
       (let ((box (compile box)))
         (lambda@ src (env)
           (variable-ref (box env)))))))

  (define (compile-resolve src cenv loc)
    (let ((var (%resolve-variable loc (env-toplevel cenv))))
      (lambda@ src (env) var)))

  (define (compile-top-branch src cenv loc args consequent alternate)
    (let* ((module (env-toplevel cenv))
           (var (%resolve-variable loc module))
           (consequent (compile consequent))
           (alternate (compile alternate)))
      (define (generic-top-branch)
        (let ((test (compile-top-call src cenv loc args)))
          (lambda@ src (env)
            (if (test env) (consequent env) (alternate env)))))
      (define-syntax-rule (maybe-primcall (prim ...) arg ...)
        (cond
         ((primitive=? 'prim loc module var)
          (let ((arg (compile arg))
                ...)
            (lambda@ src (env)
              (if (prim (arg env) ...)
                  (consequent env)
                  (alternate env)))))
         ...
         (else (generic-top-branch))))
      (match args
        ((a)
         (maybe-primcall (null? nil? pair? struct? string? vector? symbol?
                          keyword? variable? bitvector? char? zero? not)
                         a))
        ((a b)
         (maybe-primcall (eq? eqv? equal? = < > <= >= logtest logbit?)
                         a b))
        (_
         (generic-top-branch)))))

  (define (compile-if src test consequent alternate)
    (match test
      ((,(typecode call) _
        (,(typecode box-ref) _ . (,(typecode resolve) _ . loc))
        . args)
       (lazy src (env) (compile-top-branch src env loc args consequent alternate)))
      (_
       (let ((test (compile test))
             (consequent (compile consequent))
             (alternate (compile alternate)))
         (lambda@ src (env)
           (if (test env) (consequent env) (alternate env)))))))

  (define (compile-quote src x)
    (lambda@ src (env) x))

  (define (compile-let src inits body)
    (let ((body (compile body))
          (width (vector-length inits)))
      (case width
        ((0) (lambda@ src (env)
               (body (make-env* env))))
        ((1)
         (let ((a (compile (vector-ref inits 0))))
           (lambda@ src (env)
             (body (make-env* env (a env))))))
        ((2)
         (let ((a (compile (vector-ref inits 0)))
               (b (compile (vector-ref inits 1))))
           (lambda@ src (env)
             (body (make-env* env (a env) (b env))))))
        ((3)
         (let ((a (compile (vector-ref inits 0)))
               (b (compile (vector-ref inits 1)))
               (c (compile (vector-ref inits 2))))
           (lambda@ src (env)
             (body (make-env* env (a env) (b env) (c env))))))
        ((4)
         (let ((a (compile (vector-ref inits 0)))
               (b (compile (vector-ref inits 1)))
               (c (compile (vector-ref inits 2)))
               (d (compile (vector-ref inits 3))))
           (lambda@ src (env)
             (body (make-env* env (a env) (b env) (c env) (d env))))))
        (else
         (let lp ((n width)
                  (k (lambda@ src (env)
                       (make-env width #f env))))
           (if (zero? n)
               (lambda@ src (env)
                 (body (k env)))
               (lp (1- n)
                   (let ((init (compile (vector-ref inits (1- n)))))
                     (lambda@ src (env)
                       (let* ((x (init env))
                              (new-env (k env)))
                         (env-set! new-env 0 (1- n) x)
                         new-env))))))))))

  (define (compile-fixed-lambda src body nreq)
    (case nreq
      ((0) (lambda@ src (env)
             (lambda@ src ()
               (body (make-env* env)))))
      ((1) (lambda@ src (env)
             (lambda@ src (a)
               (body (make-env* env a)))))
      ((2) (lambda@ src (env)
             (lambda@ src (a b)
               (body (make-env* env a b)))))
      ((3) (lambda@ src (env)
             (lambda@ src (a b c)
               (body (make-env* env a b c)))))
      ((4) (lambda@ src (env)
             (lambda@ src (a b c d)
               (body (make-env* env a b c d)))))
      ((5) (lambda@ src (env)
             (lambda@ src (a b c d e)
               (body (make-env* env a b c d e)))))
      ((6) (lambda@ src (env)
             (lambda@ src (a b c d e f)
               (body (make-env* env a b c d e f)))))
      ((7) (lambda@ src (env)
             (lambda@ src (a b c d e f g)
               (body (make-env* env a b c d e f g)))))
      (else
       (lambda@ src (env)
         (lambda@ src (a b c d e f g . more)
           (let ((env (make-env nreq #f env)))
             (env-set! env 0 0 a)
             (env-set! env 0 1 b)
             (env-set! env 0 2 c)
             (env-set! env 0 3 d)
             (env-set! env 0 4 e)
             (env-set! env 0 5 f)
             (env-set! env 0 6 g)
             (let lp ((n 7) (args more))
               (cond
                ((= n nreq)
                 (unless (null? args)
                   (scm-error 'wrong-number-of-args
                              "eval" "Wrong number of arguments"
                              '() #f))
                 (body env))
                ((null? args)
                 (scm-error 'wrong-number-of-args
                            "eval" "Wrong number of arguments"
                            '() #f))
                (else
                 (env-set! env 0 n (car args))
                 (lp (1+ n) (cdr args)))))))))))

  (define (compile-rest-lambda src body nreq rest?)
    (case nreq
      ((0) (lambda@ src (env)
             (lambda@ src rest
               (body (make-env* env rest)))))
      ((1) (lambda@ src (env)
             (lambda@ src (a . rest)
               (body (make-env* env a rest)))))
      ((2) (lambda@ src (env)
             (lambda@ src (a b . rest)
               (body (make-env* env a b rest)))))
      ((3) (lambda@ src (env)
             (lambda@ src (a b c . rest)
               (body (make-env* env a b c rest)))))
      (else
       (lambda@ src (env)
         (lambda@ src (a b c . more)
           (let ((env (make-env (1+ nreq) #f env)))
             (env-set! env 0 0 a)
             (env-set! env 0 1 b)
             (env-set! env 0 2 c)
             (let lp ((n 3) (args more))
               (cond
                ((= n nreq)
                 (env-set! env 0 n args)
                 (body env))
                ((null? args)
                 (scm-error 'wrong-number-of-args
                            "eval" "Wrong number of arguments"
                            '() #f))
                (else
                 (env-set! env 0 n (car args))
                 (lp (1+ n) (cdr args)))))))))))

  (define (compile-opt-lambda src body nreq rest? nopt ninits unbound make-alt)
    (lambda@ src (env)
      (define alt (and make-alt (make-alt env)))
      (lambda@ src args
        (let ((nargs (length args)))
          (cond
           ((or (< nargs nreq) (and (not rest?) (> nargs (+ nreq nopt))))
            (if alt
                (apply alt args)
                ((scm-error 'wrong-number-of-args
                            "eval" "Wrong number of arguments"
                            '() #f))))
           (else
            (let* ((nvals (+ nreq (if rest? 1 0) ninits))
                   (env (make-env nvals unbound env)))
              (define (bind-req args)
                (let lp ((i 0) (args args))
                  (cond
                   ((< i nreq)
                    ;; Bind required arguments.
                    (env-set! env 0 i (car args))
                    (lp (1+ i) (cdr args)))
                   (else
                    (bind-opt args)))))
              (define (bind-opt args)
                (let lp ((i nreq) (args args))
                  (cond
                   ((and (< i (+ nreq nopt)) (< i nargs))
                    (env-set! env 0 i (car args))
                    (lp (1+ i) (cdr args)))
                   (else
                    (bind-rest args)))))
              (define (bind-rest args)
                (when rest?
                  (env-set! env 0 (+ nreq nopt) args))
                (body env))
              (bind-req args))))))))

  (define (compile-kw-lambda src body nreq rest? nopt kw ninits unbound make-alt)
    (define allow-other-keys? (car kw))
    (define keywords (cdr kw))
    (lambda@ src (env)
      (define alt (and make-alt (make-alt env)))
      (lambda@ src args
        (define (npositional args)
          (let lp ((n 0) (args args))
            (if (or (null? args)
                    (and (>= n nreq) (keyword? (car args))))
                n
                (lp (1+ n) (cdr args)))))
        (let ((nargs (length args)))
          (cond
           ((or (< nargs nreq)
                (and alt (not rest?) (> (npositional args) (+ nreq nopt))))
            (if alt
                (apply alt args)
                ((scm-error 'wrong-number-of-args
                            "eval" "Wrong number of arguments"
                            '() #f))))
           (else
            (let* ((nvals (+ nreq (if rest? 1 0) ninits))
                   (env (make-env nvals unbound env)))
              (define (bind-req args)
                (let lp ((i 0) (args args))
                  (cond
                   ((< i nreq)
                    ;; Bind required arguments.
                    (env-set! env 0 i (car args))
                    (lp (1+ i) (cdr args)))
                   (else
                    (bind-opt args)))))
              (define (bind-opt args)
                (let lp ((i nreq) (args args))
                  (cond
                   ((and (< i (+ nreq nopt)) (< i nargs)
                         (not (keyword? (car args))))
                    (env-set! env 0 i (car args))
                    (lp (1+ i) (cdr args)))
                   (else
                    (bind-rest args)))))
              (define (bind-rest args)
                (when rest?
                  (env-set! env 0 (+ nreq nopt) args))
                (bind-kw args))
              (define (bind-kw args)
                (let lp ((args args))
                  (cond
                   ((pair? args)
                    (cond
                     ((keyword? (car args))
                      (let ((k (car args))
                            (args (cdr args)))
                        (cond
                         ((assq k keywords)
                          => (lambda (kw-pair)
                               ;; Found a known keyword; set its value.
                               (if (pair? args)
                                   (let ((v (car args))
                                         (args (cdr args)))
                                     (env-set! env 0 (cdr kw-pair) v)
                                     (lp args))
                                   ((scm-error 'keyword-argument-error
                                               "eval"
                                               "Keyword argument has no value"
                                               '() (list k))))))
                         ;; Otherwise unknown keyword.
                         (allow-other-keys?
                          (lp (if (pair? args) (cdr args) args)))
                         (else
                          ((scm-error 'keyword-argument-error
                                      "eval" "Unrecognized keyword"
                                      '() (list k)))))))
                     (rest?
                      ;; Be lenient parsing rest args.
                      (lp (cdr args)))
                     (else
                      ((scm-error 'keyword-argument-error
                                  "eval" "Invalid keyword"
                                  '() (list (car args)))))))
                   (else
                    (body env)))))
              (bind-req args))))))))

  (define (compute-arity alt nreq rest? nopt kw)
    (let lp ((alt alt) (nreq nreq) (nopt nopt) (rest? rest?))
      (if (not alt)
          (let ((arglist (list nreq
                               nopt
                               (if kw (cdr kw) '())
                               (and kw (car kw))
                               (and rest? '_))))
            (values arglist nreq nopt rest?))
          (let* ((spec (cddr alt))
                 (nreq* (car spec))
                 (rest?* (if (null? (cdr spec)) #f (cadr spec)))
                 (tail (and (pair? (cdr spec)) (pair? (cddr spec)) (cddr spec)))
                 (nopt* (if tail (car tail) 0))
                 (alt* (and tail (car (cddddr tail)))))
            (if (or (< nreq* nreq)
                    (and (= nreq* nreq)
                         (if rest?
                             (and rest?* (> nopt* nopt))
                             (or rest?* (> nopt* nopt)))))
                (lp alt* nreq* nopt* rest?*)
                (lp alt* nreq nopt rest?))))))

  (define (compile-general-lambda src body nreq rest? nopt kw ninits unbound alt)
    (call-with-values
        (lambda ()
          (compute-arity alt nreq rest? nopt kw))
      (lambda (arglist min-nreq min-nopt min-rest?)
        (define make-alt
          (match alt
            (#f #f)
            ((body meta nreq . tail)
             (compile-lambda src body meta nreq tail))))
        (define make-closure
          (if kw
              (compile-kw-lambda src body nreq rest? nopt kw ninits unbound make-alt)
              (compile-opt-lambda src body nreq rest? nopt ninits unbound make-alt)))
        (lambda@ src (env)
          (let ((proc (make-closure env)))
            (set-procedure-property! proc 'arglist arglist)
            (set-procedure-minimum-arity! proc min-nreq min-nopt min-rest?)
            proc)))))

  (define (compile-lambda src body meta nreq tail)
    (define (set-procedure-meta meta proc)
      (match meta
        (() proc)
        (((prop . val) . meta)
         (set-procedure-meta meta
                             (lambda@ src (env)
                               (let ((proc (proc env)))
                                 (set-procedure-property! proc prop val)
                                 proc))))))
    (let ((body (lazy src (env) (compile body))))
      (set-procedure-meta
       meta
       (match tail
         (() (compile-fixed-lambda src body nreq))
         ((rest? . tail)
          (match tail
            (() (compile-rest-lambda src body nreq rest?))
            ((nopt kw ninits unbound alt)
             (compile-general-lambda src body nreq rest? nopt kw
                                     ninits unbound alt))))))))

  (define (compile-capture-env src locs body)
    (let ((body (compile body)))
      (lambda@ src (env)
        (let* ((len (vector-length locs))
               (new-env (make-env len #f (env-toplevel env))))
          (let lp ((n 0))
            (when (< n len)
              (match (vector-ref locs n)
                ((depth . width)
                 (env-set! new-env 0 n (env-ref env depth width))))
              (lp (1+ n))))
          (body new-env)))))

  (define (compile-seq src head tail)
    (let ((head (compile head))
          (tail (compile tail)))
      (lambda@ src (env)
        (head env)
        (tail env))))

  (define (compile-box-set! src box val)
    (let ((box (compile box))
          (val (compile val)))
      (lambda@ src (env)
        (let ((val (val env)))
          (variable-set! (box env) val)))))

  (define (compile-lexical-set! src depth width x)
    (let ((x (compile x)))
      (lambda@ src (env)
        (env-set! env depth width (x env)))))

  (define (compile-call-with-values src producer consumer)
    (let ((producer (compile producer))
          (consumer (compile consumer)))
      (lambda@ src (env)
        (call-with-values (producer env)
          (consumer env)))))

  (define (compile-apply src f args)
    (let ((f (compile f))
          (args (compile args)))
      (lambda@ src (env)
        (apply (f env) (args env)))))

  (define (compile-capture-module src x)
    (let ((x (compile x)))
      (lambda@ src (env)
        (x (current-module)))))

  (define (compile-call-with-prompt src tag thunk handler)
    (let ((tag (compile tag))
          (thunk (compile thunk))
          (handler (compile handler)))
      (lambda@ src (env)
        (call-with-prompt (tag env) (thunk env) (handler env)))))

  (define (compile-call/cc src proc)
    (let ((proc (compile proc)))
      (lambda@ src (env)
        (call/cc (proc env)))))

  (define (compile exp)
    (match exp
      ((,(typecode lexical-ref) src depth . width)
       (compile-lexical-ref src depth width))
      
      ((,(typecode call) src f . args)
       (compile-call src f args))
      
      ((,(typecode box-ref) src . box)
       (compile-box-ref src box))

      ((,(typecode resolve) src . loc)
       (lazy src (env) (compile-resolve src env loc)))

      ((,(typecode if) src test consequent . alternate)
       (compile-if src test consequent alternate))

      ((,(typecode quote) src . x)
       (compile-quote src x))

      ((,(typecode let) src inits . body)
       (compile-let src inits body))

      ((,(typecode lambda) src body meta nreq . tail)
       (compile-lambda src body meta nreq tail))

      ((,(typecode capture-env) src locs . body)
       (compile-capture-env src locs body))

      ((,(typecode seq) src head . tail)
       (compile-seq src head tail))
      
      ((,(typecode box-set!) src box . val)
       (compile-box-set! src box val))

      ((,(typecode lexical-set!) src (depth . width) . x)
       (compile-lexical-set! src depth width x))
      
      ((,(typecode call-with-values) src producer . consumer)
       (compile-call-with-values src producer consumer))

      ((,(typecode apply) src f args)
       (compile-apply src f args))

      ((,(typecode capture-module) src . x)
       (compile-capture-module src x))

      ((,(typecode call-with-prompt) src tag thunk . handler)
       (compile-call-with-prompt src tag thunk handler))
      
      ((,(typecode call/cc) src . proc)
       (compile-call/cc src proc))))

  (let ((eval (compile
               (memoize-expression
                (if (macroexpanded? exp)
                    exp
                    ((module-transformer (current-module)) exp)))))
        (env #f))
    (eval env)))

;;; Local Variables:
;;; eval: (put 'lambda@ 'scheme-indent-function 2)
;;; End:
