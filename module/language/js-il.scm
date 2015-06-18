(define-module (language js-il)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (make-program program
            make-function function
            make-jump-table jump-table
            make-params params
            make-continuation continuation
            make-local local
            make-var var
            make-continue continue
            make-const const
            make-primcall primcall
            make-call call
            make-closure closure
            make-branch branch
            make-id id
            make-kid kid
            make-seq seq
            make-prompt prompt
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
(define-js-type function self tail body)
(define-js-type jump-table spec)
(define-js-type params self req opt rest kw allow-other-keys?)
(define-js-type continuation params body)
(define-js-type local bindings body) ; local scope
(define-js-type var id exp)
(define-js-type continue cont args)
(define-js-type const value)
(define-js-type primcall name args)
(define-js-type call name k args)
(define-js-type closure label num-free)
(define-js-type branch test consequence alternate)
(define-js-type id name)
(define-js-type kid name)
(define-js-type seq body)
(define-js-type prompt escape? tag handler)

(define (unparse-js exp)
  (match exp
    (($ program entry body)
     `(program ,(unparse-js entry) . ,(map unparse-js body)))
    (($ continuation params body)
     `(continuation ,(map unparse-js params) ,(unparse-js body)))
    (($ function self tail body)
     `(function ,self ,tail ,(unparse-js body)))
    (($ jump-table body)
     `(jump-table ,@(map (lambda (p)
                           `(,(unparse-js (car p)) . ,(cdr p)))
                         body)))
    (($ params ($ id self) req opt rest kws allow-other-keys?)
     `(params ,self
              ,(map unparse-js req)
              ,(map unparse-js opt)
              ,(and rest (unparse-js rest))
              ,(map (match-lambda
                     ((kw ($ id name) ($ id sym))
                      (list kw name sym)))
                    kws)
              ,allow-other-keys?))
    (($ local bindings body)
     `(local ,(map unparse-js bindings) ,(unparse-js body)))
    (($ var id exp)
     `(var ,id ,(unparse-js exp)))
    (($ continue ($ kid k) args)
     `(continue ,k ,(map unparse-js args)))
    (($ branch test then else)
     `(if ,(unparse-js test) ,(unparse-js then) ,(unparse-js else)))
    (($ const c)
     `(const ,c))
    (($ primcall name args)
     `(primcall ,name ,(map unparse-js args)))
    (($ call ($ id name) ($ kid k) args)
     `(call ,name ,k ,(map unparse-js args)))
    (($ closure ($ kid label) nfree)
     `(closure ,label ,nfree))
    (($ id name)
     `(id . ,name))
    (($ kid name)
     `(kid . ,name))))
