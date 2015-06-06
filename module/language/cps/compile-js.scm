(define-module (language cps compile-js)
  #:use-module (language cps)
  #:use-module (language js-il)
  #:use-module (ice-9 match)
  #:export (compile-js))

(define optimize (@@ (language cps compile-bytecode) optimize))
(define convert-closures (@@ (language cps compile-bytecode) convert-closures))
(define reify-primitives (@@ (language cps compile-bytecode) reify-primitives))
(define renumber (@@ (language cps compile-bytecode) renumber))

(define (compile-js exp env opts)
  ;; See comment in `optimize' about the use of set!.
  (set! exp (optimize exp opts))
  (set! exp (convert-closures exp))
  ;; first-order optimization should go here
  (set! exp (reify-primitives exp))
  (set! exp (renumber exp))
  ;; (values exp env env)
  (match exp
    (($ $program funs)
     ;; TODO: I should special case the compilation for the initial fun,
     ;; as this is the entry point for the program, and shouldn't get a
     ;; "self" argument, for now, I add "undefined" as the first
     ;; argument in the call to it.
     ;; see compile-exp in (language js-il compile-javascript)
     (values (make-program (compile-fun (car funs))
                           (map compile-fun (cdr funs)))
             env
             env)))
  )

(define (compile-fun fun)
  ;; meta
  (match fun
    (($ $cont k ($ $kfun src meta self ($ $cont tail ($ $ktail)) clause))
     (make-var k (compile-clause clause self tail)))
    (_
     `(fun:todo: ,fun))))

(define (compile-clause clause self tail)
  (match clause
    (($ $cont k ($ $kclause ($ $arity req opt rest kw allow-other-keys?)
         body alternate))
     ;; add function argument prelude
     (unless (null? opt)
       (not-supported "optional arguments are not supported" clause))
     (when rest
       (not-supported "rest arguments are not supported" clause))
     (unless (or (null? kw) allow-other-keys?)
       (not-supported "keyword arguments are not supported" clause))
     (when alternate
       (not-supported "alternate continuations are not supported" clause))
     (make-function self ;; didn't think this js pattern would come in handy
                    (cons tail req)
                    (match body
                      (($ $cont k ($ $kargs () () exp))
                       (compile-term exp))
                      (($ $cont k _)
                       (make-local (list (compile-cont body))
                                   (make-jscall k req))))))
    (_
     `(clause:todo: ,clause))))

(define (not-supported msg clause)
  (error 'not-supported msg clause))

(define (compile-term term)
  (match term
    (($ $letk conts body)
     (make-local (map compile-cont conts) (compile-term body)))
    (($ $continue k src exp)
     (compile-exp exp k))))

(define (compile-cont cont)
  (match cont
    (($ $cont k ($ $kargs names syms body))
     ;; use the name part?
     (make-var k (make-function syms (compile-term body))))
    (($ $cont k ($ $kreceive ($ $arity (arg) _ (? symbol? rest) _ _) k2))
     (make-var k (make-function (list arg rest) (make-jscall k2 (list arg rest)))))
    (($ $cont k ($ $kreceive ($ $arity (arg) _ #f _ _) k2))
     (make-var k (make-function (list arg) (make-jscall k2 (list arg)))))
    (_
     `(cont:todo: ,cont))
    ))

(define (compile-exp exp k)
 (match exp
    (($ $branch kt exp)
     (compile-test exp kt k))
    (($ $primcall 'return (arg))
     (make-continue k (list (make-id arg))))
    (($ $call name args)
     (make-call name (cons k args)))
    (($ $callk label proc args)
     ;; eh?
     ;; (pk 'callk label proc args k)
     (make-jscall label (cons* proc k args)))
    (($ $values values)
     (make-continue k (map make-id values)))
    (_
     (make-continue k (list (compile-exp* exp))))))

(define (compile-exp* exp)
  (match exp
    (($ $const val)
     (make-const val))
    (($ $primcall name args)
     (make-primcall name args))
    (($ $closure label nfree)
     (make-closure label nfree))
    (_
     `(exp:todo: ,exp))))

(define (compile-test exp kt kf)
  ;; TODO: find out if the expression is always simple enough that I
  ;; don't need to create a new continuation (which will require extra
  ;; arguments being passed through)
  (make-branch (compile-exp* exp)
               (make-continue kt '())
               (make-continue kf '())))
