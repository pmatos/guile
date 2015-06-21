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
    (($ $program (($ $cont ks funs) ...))
     ;; TODO: I should special case the compilation for the initial fun,
     ;; as this is the entry point for the program, and shouldn't get a
     ;; "self" argument, for now, I add "undefined" as the first
     ;; argument in the call to it.
     ;; see compile-exp in (language js-il compile-javascript)
     (values (make-program
              (map (lambda (k fun)
                     (cons (make-kid k) (compile-fun fun)))
                   ks
                   funs))
             env
             env))))

(define (compile-fun fun)
  (match fun
    (($ $kfun _ _ self ($ $cont tail ($ $ktail)) clause)
     (make-function
      (make-id self)
      (make-kid tail)
      (compile-clauses clause self)))))

(define (compile-clauses clause self)
  (match clause
    (($ $cont k ($ $kclause arity body #f))
     `((,(make-kid k)
        ,(arity->params arity self)
        ,(compile-clause arity body self))))
    (($ $cont k ($ $kclause arity body next))
     `((,(make-kid k)
        ,(arity->params arity self)
        ,(compile-clause arity body self))
       . ,(compile-clauses next self)))))

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

(define (compile-clause arity body self)
  (match arity
    (($ $arity req opt rest ((_ _ kw-syms) ...) _)
     (let ((ids (map make-id
                     (append req opt kw-syms (if rest (list rest) '())))))
       (make-continuation
        (cons (make-id self) ids)
        (match body
          (($ $cont k cont)
           (make-local `((,(make-kid k) . ,(compile-cont cont)))
                       (make-continue (make-kid k) ids)))))))))

(define (compile-term term)
  (match term
    (($ $letk (($ $cont ks conts) ...) body)
     (make-local (map (lambda (k cont)
                        (cons (make-kid k)
                              (compile-cont cont)))
                      ks
                      conts)
                 (compile-term body)))
    (($ $continue k src exp)
     (compile-exp exp k))))

(define (compile-cont cont)
  (match cont
    (($ $kargs names syms body)
     (make-continuation (map make-id syms) (compile-term body)))
    (($ $kreceive ($ $arity req _ (? symbol? rest) _ _) k2)
     (let ((ids (map make-id (append req (list rest)))))
       (make-continuation ids (make-continue (make-kid k2) ids))))
    (($ $kreceive ($ $arity req _ #f _ _) k2)
     (let ((ids (map make-id req)))
       (make-continuation ids (make-continue (make-kid k2) ids))))))

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
