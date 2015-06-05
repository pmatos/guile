;; Only has enough of the ecmascript language for compilation from cps
(define-module (language javascript)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (
            make-const const
            make-function function
            make-return return
            make-call call
            make-block block
            make-new new
            make-id id
            make-refine refine
            make-conditional conditional
            make-var var

            print-statement))

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
  (format port "#<js ~S>" (unparse-js exp)))

(define-js-type const c)
(define-js-type function args body)
(define-js-type return exp)
(define-js-type call function args)
(define-js-type block statements)
(define-js-type new expr)
(define-js-type id name)
(define-js-type refine id field)
(define-js-type conditional test then else)
(define-js-type var id exp)

(define (unparse-js exp)
  (match exp
    (($ const c)
     `(const ,c))
    (($ function args body)
     `(function ,args ,@(map unparse-js body)))
    (($ return exp)
     `(return ,(unparse-js exp)))
    (($ call function args)
     `(call ,(unparse-js function) ,@(map unparse-js args)))
    (($ block statements)
     `(block ,@(map unparse-js statements)))
    (($ new expr)
     `(new ,(unparse-js expr)))
    (($ id name)
     `(id ,name))
    (($ refine id field)
     `(refine ,(unparse-js id) ,(unparse-js field)))
    (($ conditional test then else)
     `(if ,(unparse-js test)
          (block ,@(map unparse-js then))
          (block ,@(map unparse-js else))))
    (($ var id exp)
     `(var ,id ,(unparse-js exp)))))

(define (print-exp exp port)
  (match exp

    (($ const c)
     (print-const c port))

    (($ id name)
     (print-id name port))

    (($ call (and ($ function _ _) fun) args)
     (format port "(")
     (print-exp fun port)
     (format port ")(")
     (print-separated args print-exp "," port)
     (format port ")"))

    (($ call fun args)
     (print-exp fun port)
     (format port "(")
     (print-separated args print-exp "," port)
     (format port ")"))


    (($ refine expr field)
     (print-exp expr port)
     (format port "[")
     (print-exp field port)
     (format port "]"))

    (($ function params body)
     (format port "function (")
     (print-separated params print-id "," port)
     (format port ")")
     (print-block body port))

    (($ block stmts)
     (print-block stmts port))

    (($ new expr)
     (format port "new ")
     (print-exp expr port))))

(define (print-statement stmt port)
  (match stmt
    (($ var id exp)
     (format port "var ")
     (print-id id port)
     (format port " = ")
     (print-exp exp port)
     (format port ";"))

    (($ conditional test then else)
     (format port "if (")
     (print-exp test port)
     (format port ") {")
     (print-block then port)
     (format port "} else {")
     (print-block else port)
     (format port "}"))

    (($ return expr)
     (format port "return ")
     (print-exp expr port)
     (format port ";"))

    (expr
     (print-exp expr port)
     (format port ";"))))

(define (print-id id port)
  (display id port))

(define (print-block stmts port)
  (format port "{")
  (print-statements stmts port)
  (format port "}"))

(define (print-statements stmts port)
  (for-each (lambda (stmt)
              (print-statement stmt port))
            stmts))

(define (print-const c port)
  (cond ((string? c)
         (write c port))
        ((number? c)
         (write c port))
        (else
         (throw 'unprintable-const c))))

(define (print-separated args printer separator port)
  (unless (null? args)
    (let ((first (car args))
          (rest  (cdr args)))
      (printer first port)
      (for-each (lambda (x)
                  (display separator port)
                  (printer x port))
                rest))))

(define (print-terminated args printer terminator port)
  (for-each (lambda (x)
              (printer x port)
              (display terminator port))
            args))
