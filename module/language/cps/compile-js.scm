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
             env))))

(define (compile-fun fun)
  (match fun
    (($ $cont k ($ $kfun src meta self ($ $cont tail ($ $ktail)) clause))
     (make-var k (compile-clause clause self tail)))))

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
     (make-function self
                    (cons tail req)
                    (match body
                      (($ $cont k ($ $kargs () () exp))
                       (compile-term exp))
                      (($ $cont k _)
                       (make-local (list (compile-cont body))
                                   (make-continue k (map make-id req)))))))))

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
     (make-var k (make-continuation syms (compile-term body))))
    (($ $cont k ($ $kreceive ($ $arity req _ (? symbol? rest) _ _) k2))
     (make-var k
       (make-continuation (append req (list rest))
                          (make-continue k2
                                         (append (map make-id req) (list (make-id rest)))))))
    (($ $cont k ($ $kreceive ($ $arity req _ #f _ _) k2))
     (make-var k (make-continuation req (make-continue k2 (map make-id req)))))
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
     (make-continue label (map make-id (cons* proc k args))))
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
