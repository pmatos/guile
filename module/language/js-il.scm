(define-module (language js-il)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (make-program program
            make-function function
            make-continuation continuation
            make-local local
            make-var var
            make-continue continue
            make-const const
            make-primcall primcall
            make-call call
            make-closure closure
            make-branch branch
            make-return return
            make-id id
            ))

;; Copied from (language cps)
;; Should put in a srfi 99 module
(define-syntax define-record-type*
  (lambda (x)
    (define (id-append ctx . syms)
      (datum->syntax ctx (apply symbol-append (map syntax->datum syms))))
    (syntax-case x ()
      ((_ name field ...)
       (and (identifier? #'name) (and-map identifier? #'(field ...)))
       (with-syntax ((cons (id-append #'name #'make- #'name))
                     (pred (id-append #'name #'name #'?))
                     ((getter ...) (map (lambda (f)
                                          (id-append f #'name #'- f))
                                        #'(field ...))))
         #'(define-record-type name
             (cons field ...)
             pred
             (field getter)
             ...))))))

;; TODO: add type predicates to fields so I can only construct valid
;; objects
(define-syntax-rule (define-js-type name field ...)
  (begin
    (define-record-type* name field ...)
    (set-record-type-printer! name print-js)))

(define (print-js exp port)
  (format port "#<js-il ~S>" (unparse-js exp)))

(define-js-type program entry body)
(define-js-type function name params body)
(define-js-type continuation params body)
(define-js-type local bindings body) ; local scope
(define-js-type var id exp)
(define-js-type continue cont args)
(define-js-type const value)
(define-js-type primcall name args)
(define-js-type call name args)
(define-js-type closure label num-free)
(define-js-type branch test consequence alternate)
(define-js-type id name)
(define-js-type return val)

(define (unparse-js exp)
  (match exp
    (($ program entry body)
     `(program ,(unparse-js entry) . ,(map unparse-js body)))
    (($ continuation params body)
     `(continuation ,params ,(unparse-js body)))
    (($ function name params body)
     `(function ,name ,params ,(unparse-js body)))
    (($ local bindings body)
     `(local ,(map unparse-js bindings) ,(unparse-js body)))
    (($ var id exp)
     `(var ,id ,(unparse-js exp)))
    (($ continue k args)
     `(continue ,k ,(map unparse-js args)))
    (($ branch test then else)
     `(if ,(unparse-js test) ,(unparse-js then) ,(unparse-js else)))
    (($ const c)
     `(const ,c))
    (($ primcall name args)
     `(primcall ,name , args))
    (($ call name args)
     `(call ,name , args))
    (($ closure label nfree)
     `(closure ,label ,nfree))
    (($ return val)
     `(return . ,(unparse-js val)))
    (($ id name)
     `(id . ,name))))
