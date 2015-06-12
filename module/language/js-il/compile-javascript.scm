(define-module (language js-il compile-javascript)
  #:use-module ((srfi srfi-1) #:select (fold-right))
  #:use-module (ice-9 match)
  #:use-module ((language js-il) #:renamer (symbol-prefix-proc 'il:))
  #:use-module (language javascript)
  #:use-module (language js-il direct)
  #:use-module (system foreign)
  #:export (compile-javascript))

(define (undefined? obj)
  (define tc8-iflag 4)
  (define unbound-val 9)
  (define unbound-bits (logior (ash unbound-val 8) tc8-iflag))
  (eqv? obj (pointer->scm (make-pointer unbound-bits))))

(define (compile-javascript exp env opts)
  (set! exp (remove-immediate-calls exp))
  (values (compile-exp exp) env env))

(define *scheme* (make-id "scheme"))

(define (name->id name)
  (make-id (rename name)))

(define (rename id)
  (cond ((and (integer? id) (>= id 0))
         (format #f "k_~a" id))
        ((symbol? id)
         (js-id (symbol->string id)))
        ((string? id)
         (js-id id))
        (else
         (throw 'bad-id id))))

(define (js-id name)
  (call-with-output-string
   (lambda (port)
     (display "k_" port)
     (string-for-each
      (lambda (c)
        (if (or (and (char<=? #\a c) (char<=? c #\z))
                (and (char<=? #\A c) (char<=? c #\Z))
                (and (char<=? #\0 c) (char<=? c #\9)))
            (display c port)
            (case c
              ((#\-) (display "_h" port))
              ((#\_) (display "_u" port))
              ((#\?) (display "_p" port))
              ((#\!) (display "_x" port))
              ((#\<) (display "_l" port))
              ((#\>) (display "_g" port))
              ((#\=) (display "_e" port))
              ((#\*) (display "_s" port))
              ((#\+) (display "_a" port))
              ((#\\) (display "_b" port))
              ((#\/) (display "_f" port))
              (else
               (throw 'bad-id-char  c)))))
      name))))

(define (bind-rest-args rest num-drop)
  (define (ref i l)
    (if (null? l)
        i
        (ref (make-refine i (make-const (car l)))
             (cdr l))))
  (define this (rename rest))
  (make-var this
            (make-call (ref *scheme* (list "list" "apply"))
                       (list
                        (ref *scheme* (list "list"))
                        (make-call (ref (make-id "Array") (list "prototype" "slice" "call"))
                                   (list (make-id "arguments") (make-const num-drop)))))))

(define (bind-opt-args opts num-drop)
  (map (lambda (opt idx)
         (make-var (rename opt)
                   (make-binop 'or
                               (make-refine (make-id "arguments")
                                            (make-const (+ num-drop idx)))
                               (make-refine *scheme* (make-const "UNDEFINED")))))
       opts
       (iota (length opts))))


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

    (($ il:continuation params body)
     (make-function (map rename params) (list (compile-exp body))))

    (($ il:function params body)
     (make-function (map rename params) (list (compile-exp body))))

    (($ il:jump-table specs)
     (compile-jump-table specs))

    (($ il:local bindings body)
     (make-block (append (map compile-exp bindings) (list (compile-exp body)))))

    (($ il:var id exp)
     (make-var (rename id) (compile-exp exp)))

    (($ il:continue k exps)
     (make-return (make-call (name->id k) (map compile-exp exps))))

    (($ il:branch test then else)
     (make-branch (make-call (make-refine *scheme* (make-const "is_true"))
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

    (($ il:closure label nfree)
     (make-new
      (make-call (make-refine *scheme* (make-const "Closure"))
                 (list (name->id label) (make-const nfree)))))

    (($ il:id name)
     (name->id name))))

(define (compile-jump-table specs)
  (define offset 2) ; closure & continuation
  (define (compile-test params)
    (match params
      (($ il:params self req '() #f)
       (make-binop '=
                   (make-refine (make-id "arguments")
                                (make-const "length"))
                   (make-const (+ offset (length req)))))
      (($ il:params self req '() rest)
       (make-binop '>=
                   (make-refine (make-id "arguments")
                                (make-const "length"))
                   (make-const (+ offset (length req)))))
      (($ il:params self req opts #f)
       (make-binop 'and
                   (make-binop '<=
                               (make-const (+ offset (length req)))
                               (make-refine (make-id "arguments")
                                            (make-const "length")))
                   (make-binop '<=
                               (make-refine (make-id "arguments")
                                            (make-const "length"))
                               (make-const (+ offset (length req) (length opts))))))
      ))
  (define (compile-jump params k)
    (match params
      (($ il:params self req '() #f)
       (list
        (make-return
         (make-call (name->id k)
                    (cons (name->id self)
                          (map (lambda (idx)
                                 (make-refine (make-id "arguments")
                                              (make-const (+ offset idx))))
                               (iota (length req))))))))
      (($ il:params self req '() rest)
       (list
        (bind-rest-args rest (+ offset (length req)))
        (make-return
         (make-call (name->id k)
                    (append (list (name->id self))
                            (map (lambda (idx)
                                   (make-refine (make-id "arguments")
                                                (make-const (+ offset idx))))
                                 (iota (length req)))
                            (list (name->id rest)))))))
      (($ il:params self req opts #f)
       (append
        (bind-opt-args opts (+ offset (length req)))
        (list
         (make-return
          (make-call (name->id k)
                     (append (list (name->id self))
                             (map (lambda (idx)
                                    (make-refine (make-id "arguments")
                                                 (make-const (+ offset idx))))
                                  (iota (length req)))
                             (map name->id opts)))))))
      ))
  (fold-right (lambda (a d)
                (make-branch (compile-test (car a))
                             (compile-jump (car a) (cdr a))
                             (list d)))
              ;; FIXME: should throw an error
              (make-return (make-id "undefined"))
              specs))

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
        ((string? c)
         (make-new
          (make-call
           (make-refine *scheme* (make-const "String"))
           (list (make-const c)))))
        (else
         (throw 'uncompilable-const c))))
