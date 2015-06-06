(define-module (language js-il)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (make-program program
            (make-function* . make-function) function
            make-local local
            make-var var
            make-continue continue ; differ from conts
            make-const const
            make-primcall primcall
            make-call call
            make-jscall jscall
            make-closure closure
            make-branch branch
            ; print-js
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

(define make-function*
  (case-lambda
    ((name params body)
     (make-function name params body))
    ((params body)
     (make-function #f params body))))

(define-js-type local bindings body) ; local scope
(define-js-type var id exp)
(define-js-type continue cont args)
(define-js-type const value)
(define-js-type primcall name args)
(define-js-type call name args)
(define-js-type jscall name args) ;; TODO: shouldn't need this hack
(define-js-type closure label num-free)
(define-js-type branch test consequence alternate)
(define-js-type id name)
(define-js-type return val)

(define (unparse-js exp)
  (match exp
    (($ program entry body)
     `(program ,(unparse-js entry) . ,(map unparse-js body)))
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
    ;; values
    (($ const c)
     `(const ,c))
    (($ primcall name args)
     `(primcall ,name , args))
    (($ call name args)
     `(call ,name , args))
    (($ jscall name args)
     `(jscall ,name , args))
    (($ closure label nfree)
     `(closure ,label ,nfree))
    (($ return val)
     `(return . ,(unparse-js val)))
    (($ id name)
     `(id . ,name))
    (_
     ;(error "unexpected js" exp)
     (pk 'unexpected exp)
     exp)))
#|
(define (print-js exp port)
  ;; could be much nicer with foof's fmt
  (match exp
    (($ program (and entry ($ var name _)) body)
     ;; TODO: I should probably put call to entry in js-il
     (format port "(function(){\n")
     (print-js entry port) (display ";\n" port)
     (print-terminated body print-js ";\n" port)
     ;; call to entry point
     (format port "return ~a(scheme.initial_cont);" (lookup-cont name))
     (format port "})();\n"))
    (($ function #f params body)
     (format port "function(")
     (print-separated params print-var "," port)
     (format port "){\n")
     (print-js body port)(display ";" port)
     (format port "}"))
    ;; TODO: clean this code up
    (($ function name params body)
     (format port "function (~a," (lookup-cont name))
     (print-separated params print-var "," port)
     (format port "){\n")
     (print-js body port)(display ";" port)
     (format port "}"))
    (($ local bindings body)
     (display "{" port)
     (print-terminated bindings print-js ";\n" port)
     (print-js body port)
     (display ";\n")
     (display "}" port))
    (($ var id exp)
     (format port "var ~a = " (lookup-cont id))
     (print-js exp port))
    (($ continue k args)
     (format port "return ~a(" (lookup-cont k))
     (print-js exp port)
     (display ")" port))
    (($ branch test then else)
     (display "if (scheme.is_true(" port)
     (print-js test port)
     (display ")) {\n" port)
     (print-js then port)
     (display ";} else {\n" port)
     (print-js else port)
     (display ";}" port))
    ;; values
    (($ const c)
     (print-const c port))
    (($ primcall name args)
     (format port "scheme.primitives[\"~s\"](" name)
     (print-separated args print-var "," port)
     (format port ")"))
    (($ call name args)
     ;; TODO: need to also add closure env
     (format port "return ~a.fun(~a," (lookup-cont name) (lookup-cont name))
     (print-separated args print-var "," port)
     (format port ")"))
    (($ jscall name args)
     (format port "return ~a(" (lookup-cont name))
     (print-separated args print-var "," port)
     (format port ")"))
    (($ closure label nfree)
     (format port "new scheme.Closure(~a,~a)" (lookup-cont label) nfree))
    (($ values vals)
     (display "new scheme.Values(" port)
     (print-separated vals print-var "," port)
     (display ")" port))
    ;; (($ return val)
    ;;  (display "return " port)
    ;;  (print-js val port))
    (($ id name)
     (print-var name port))
    (_
     (error "print: unexpected js" exp))))

(define (print-var var port)
  (if (number? var)
      (display (lookup-cont var) port)
      (display var port)))

(define (lookup-cont k)
  (format #f "kont_~s" k))

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

(define (print-const c port)
  (cond ((number? c) (display c port))
        ((eqv? c #t) (display "scheme.TRUE" port))
        ((eqv? c #f) (display "scheme.FALSE" port))
        ((eqv? c '()) (display "scheme.EMPTY" port))
        ((unspecified? c) (display "scheme.UNSPECIFIED" port))
        ((symbol? c) (format port "new scheme.Symbol(\"~s\")" c))
        ((list? c)
         (display "scheme.list(" port)
         (print-separated c print-const "," port)
         (display ")" port))
        (else
         (throw 'not-implemented))))
|#
