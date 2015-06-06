(define-module (language js-il compile-javascript)
  #:use-module (ice-9 match)
  #:use-module ((language js-il) #:renamer (symbol-prefix-proc 'il:))
  #:use-module (language javascript)
  #:export (compile-javascript))

(define (compile-javascript exp env opts)
  (values (compile-exp exp) env env))

(define *scheme* (make-id "scheme"))

(define (name->id name)
  (make-id (rename name)))

(define (rename name)
  (format #f "kont_~a" name))

(define (compile-exp exp)
  ;; TODO: handle ids for js
  (match exp
    (($ il:program (and entry ($ il:var name _)) body)
     (let ((entry-call
            (make-return
             (make-call (name->id name)
                        (list
                         (make-id "undefined")
                         (make-refine *scheme* (make-const "initial_cont")))))))
       (make-call (make-function '() (append (map compile-exp body)
                                           (list (compile-exp entry) entry-call)))
                  '())))

    (($ il:function #f params body)
     (make-function (map rename params) (list (compile-exp body))))

    (($ il:function name params body)
     ;; TODO: split il:function into closure (with self) and cont types
     (make-function (map rename (cons name params)) (list (compile-exp body))))

    (($ il:local bindings body)
     (make-block (append (map compile-exp bindings) (list (compile-exp body)))))

    (($ il:var id exp)
     (make-var (rename id) (compile-exp exp)))

    (($ il:continue k exps)
     (make-return (make-call (name->id k) (map compile-exp exps))))

    (($ il:branch test then else)
     (make-conditional (make-call (make-refine *scheme* (make-const "is_true"))
                                  (list (compile-exp test)))
                       (list (compile-exp then))
                       (list (compile-exp else))))

    (($ il:const c)
     (compile-const c))

    (($ il:primcall name args)
     (make-call (make-refine (make-refine *scheme* (make-const "primitives"))
                             (make-const (symbol->string name)))
                (map name->id args)))

    (($ il:call name args)
     (make-return
      (make-call (make-refine (name->id name) (make-const "fun"))
                 (map name->id (cons name args)))))

    (($ il:jscall name args)
     (make-return (make-call (name->id name) (map name->id args))))

    (($ il:closure label nfree)
     (make-new
      (make-call (make-refine *scheme* (make-const "Closure"))
                 (list (name->id label) (make-const nfree)))))

    (($ il:id name)
     (name->id name))))

(define (compile-const c)
  (cond ((number? c)
         (make-const c))
        ((eqv? c #t)
         (make-refine *scheme* (make-const "TRUE")))
        ((eqv? c #f)
         (make-refine *scheme* (make-const "FALSE")))
        ((eqv? c '())
         (make-refine *scheme* (make-const "EMPTY")))
        ((unspecified? c)
         (make-refine *scheme* (make-const "UNSPECIFIED")))
        ((symbol? c)
         (make-new
          (make-call
           (make-refine *scheme* (make-const "Symbol"))
           (list (make-const (symbol->string c))))))
        ((list? c)
         (make-call
           (make-refine *scheme* (make-const "list"))
           (map compile-const c)))
        (else
         (throw 'uncompilable-const c))))
