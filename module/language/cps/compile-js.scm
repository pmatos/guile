(define-module (language cps compile-js)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps utils)
  #:use-module ((language js-il)
                #:renamer (lambda (x) (if (eqv? x 'make-prompt) 'make-prompt* x)))
  #:use-module (ice-9 match)
  #:export (compile-js))

(define intmap-select (@@ (language cps compile-bytecode) intmap-select))
(define lower-cps (@@ (language cps compile-bytecode) lower-cps))

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
  (match (intmap-ref cps kfun)
    (($ $kfun src meta self tail clause)
     (make-function
      (make-id self)
      (make-kid tail)
      (compile-clauses cps clause self)))))


(define (compile-clauses cps clause self)
  (match (intmap-ref cps clause)
    (($ $kclause arity body #f)
     `((,(make-kid clause)
        ,(arity->params arity self)
        ,(compile-clause cps arity body self))))
    (($ $kclause arity body next)
     `((,(make-kid clause)
        ,(arity->params arity self)
        ,(compile-clause cps arity body self))
       . ,(compile-clauses cps next self)))))


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


(define (compile-clause cps arity body self)
  (match arity
    (($ $arity req opt rest ((_ _ kw-syms) ...) _)
     (let ((ids (map make-id
                     (append req opt kw-syms (if rest (list rest) '())))))
       (make-continuation
        (cons (make-id self) ids)
        (make-local `((,(make-kid body) . ,(compile-cont cps body)))
                    (make-continue (make-kid body) ids)))))))


(define (compile-cont cps cont)
  (match (intmap-ref cps cont)
    ;; The term in a $kargs is always a $continue
    (($ $kargs names syms ($ $continue k src exp))
     (make-continuation (map make-id syms) (compile-exp exp k)))
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
