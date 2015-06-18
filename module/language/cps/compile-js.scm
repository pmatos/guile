(define-module (language cps compile-js)
  #:use-module (language cps)
  #:use-module ((language js-il)
                #:renamer (lambda (x) (if (eqv? x 'make-prompt) 'make-prompt* x)))
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
    (($ $cont k ($ $kfun _ _ self ($ $cont tail ($ $ktail)) clause))
     (call-with-values
         (lambda ()
           (extract-clauses self clause))
       (lambda (jump-table clauses)
         (make-var
          (make-kid k)
          (make-function
           (make-id self) (make-kid tail)
           (make-local (map (lambda (clause)
                              (compile-clause clause self tail))
                            clauses)
                       (make-jump-table jump-table)))))))))

(define (extract-clauses self clause)
  (define (make-params* self req opts rest kw allow-other-keys?)
    (make-params (make-id self)
                  (map make-id req)
                  (map make-id opts)
                  (and rest (make-id rest))
                  (map make-id kw)
                  allow-other-keys?))
  (let loop ((clause clause) (specs '()) (clauses '()))
    (match clause
      (($ $cont k ($ $kclause ($ $arity req opts rest kw allow-other-keys?) _ #f))
       (values (reverse (acons (make-params* self req opts rest kw allow-other-keys?)
                               (make-kid k)
                               specs))
               (reverse (cons clause clauses))))
      (($ $cont k ($ $kclause ($ $arity req opts rest kw allow-other-keys?) _ alternate))
       (loop alternate
             (acons (make-params* self req opts rest kw allow-other-keys?)
                    (make-kid k)
                    specs)
             (cons clause clauses))))))

(define (compile-clause clause self tail)
  (match clause
    (($ $cont k ($ $kclause ($ $arity req opt rest ((_ _ kw-syms) ...) _) body _))
     (make-var
      (make-kid k)
      (make-continuation
       (append (list (make-id self))
               (map make-id req)
               (map make-id opt)
               (map make-id kw-syms)
               (if rest (list (make-id rest)) '()))
       (match body
         (($ $cont k ($ $kargs () () exp))
          (compile-term exp))
         (($ $cont k _)
          (make-local (list (compile-cont body))
                      (make-continue
                       (make-kid k)
                       (map make-id (append req opt kw-syms (if rest (list rest) '()))))))))))))

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
     (make-var (make-kid k)
               (make-continuation (map make-id syms)
                                  (compile-term body))))
    (($ $cont k ($ $kreceive ($ $arity req _ (? symbol? rest) _ _) k2))
     (make-var
      (make-kid k)
      (make-continuation (append (map make-id req) (list (make-id rest)))
                         (make-continue (make-kid k2)
                                        (append (map make-id req)
                                                (list (make-id rest)))))))
    (($ $cont k ($ $kreceive ($ $arity req _ #f _ _) k2))
     (make-var (make-kid k)
               (make-continuation (map make-id req)
                                  (make-continue (make-kid k2)
                                                 (map make-id req)))))))

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
