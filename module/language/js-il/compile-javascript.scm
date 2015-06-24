(define-module (language js-il compile-javascript)
  #:use-module ((srfi srfi-1) #:select (fold-right))
  #:use-module (ice-9 match)
  #:use-module ((language js-il) #:renamer (symbol-prefix-proc 'il:))
  #:use-module (language javascript)
  #:use-module (language javascript simplify)
  #:use-module (language js-il inlining)
  #:use-module (system foreign)
  #:export (compile-javascript))

(define (undefined? obj)
  (define tc8-iflag 4)
  (define unbound-val 9)
  (define unbound-bits (logior (ash unbound-val 8) tc8-iflag))
  (eqv? obj (pointer->scm (make-pointer unbound-bits))))

(define (compile-javascript exp env opts)
  (set! exp (inline-single-calls exp))
  (set! exp (compile-exp exp))
  (set! exp (flatten-blocks exp))
  (values exp env env))

(define *scheme* (make-id "scheme"))
(define *utils*  (make-refine *scheme* (make-const "utils")))

(define (rename-id i)
  (match i
    (($ il:id i)
     (rename i))
    (($ il:kid i)
     (rename-kont i))))

(define (compile-id i)
  (make-id (rename-id i)))

(define (kont->id name)
  (make-id (rename-kont name)))

(define (rename-kont name)
  (format #f "k_~a" name))

(define (name->id name)
  (make-id (rename name)))

(define (rename id)
  (cond ((and (integer? id) (>= id 0))
         (format #f "v_~a" id))
        ((symbol? id)
         (js-id (symbol->string id)))
        ((string? id)
         (js-id id))
        (else
         (throw 'bad-id id))))

(define (js-id name)
  (call-with-output-string
   (lambda (port)
     (display "v_" port)
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
              ((#\%) (display "_c" port))
              ((#\$) (display "_d" port))
              ((#\~) (display "_t" port))
              ((#\^) (display "_i" port))
              ((#\&) (display "_j" port))
              ((#\:) (display "_k" port))
              ((#\@) (display "_m" port))
              ;; unused: noqrvxy
              (else
               (display "_z" port)
               (display (char->integer c) port)))))
      name))))

(define (bind-rest-args rest num-drop)
  (define (ref i l)
    (if (null? l)
        i
        (ref (make-refine i (make-const (car l)))
             (cdr l))))
  (define this (rename-id rest))
  (make-var this
            (make-call (ref *scheme* (list "list" "apply"))
                       (list
                        (ref *scheme* (list "list"))
                        (make-call (ref (make-id "Array") (list "prototype" "slice" "call"))
                                   (list (make-id "arguments") (make-const num-drop)))))))

(define (bind-opt-args opts num-drop)
  (map (lambda (opt idx)
         (make-var (rename-id opt)
                   (let ((arg (make-refine (make-id "arguments")
                                            (make-const (+ num-drop idx)))))
                     (make-ternary (make-binop '===
                                               (make-prefix 'typeof arg)
                                               (make-id "undefined"))
                                   (make-refine *scheme* (make-const "UNDEFINED"))
                                   arg))))
       opts
       (iota (length opts))))

(define (bind-kw-args kws ids num-drop)
  (define lookup (make-refine *utils* (make-const "keyword_ref")))
  (map (lambda (kw id)
         (make-var (rename-id id)
                   (make-call lookup
                              (list (compile-const kw)
                                    (make-id "arguments")
                                    (compile-const num-drop)
                                    (make-refine *scheme* (make-const "UNDEFINED"))))))
       kws
       ids))


(define (compile-exp exp)
  ;; TODO: handle ids for js
  (match exp
    (($ il:program ((name . fun) (names . funs) ...))
     (let ((entry-call
            (make-return
             (make-call (compile-id name)
                        (list
                         (make-id "undefined")
                         (make-refine *scheme* (make-const "initial_cont")))))))
       (make-call (make-function
                   '()
                   (append
                    (map (lambda (id f)
                           (make-var (rename-id id)
                                     (compile-exp f)))
                         (cons name names)
                         (cons fun funs))

                    (list entry-call)))
                  '())))

    (($ il:continuation params body)
     (make-function (map rename-id params) (list (compile-exp body))))

    (($ il:function self tail clauses)
     (make-function (list (rename-id self) (rename-id tail))
                    (append
                     (map (match-lambda
                           ((id _ body)
                            (make-var (rename-id id) (compile-exp body))))
                          clauses)
                     (list (compile-jump-table clauses)))))

    (($ il:local ((ids . bindings) ...) body)
     (make-block
      (append (map (lambda (id binding)
                     (make-var (rename-id id) (compile-exp binding)))
                   ids
                   bindings)
              (list (compile-exp body)))))

    (($ il:continue k exps)
     (make-return (make-call (compile-id k) (map compile-exp exps))))

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
                (map compile-id args)))

    (($ il:call name k args)
     (make-return
      (make-call (make-refine (compile-id name) (make-const "fun"))
                 (cons* (compile-id name)
                        (compile-id k)
                        (map compile-id args)))))

    (($ il:closure label nfree)
     (make-new
      (make-call (make-refine *scheme* (make-const "Closure"))
                 (list (compile-id label) (make-const nfree)))))

    (($ il:prompt escape? tag handler)
     ;; never a tailcall
     (make-call (make-refine (make-refine *scheme* (make-const "primitives"))
                             (make-const "prompt"))
                (list (compile-const escape?) (compile-id tag) (compile-id handler))))

    (($ il:seq body)
     (make-block (map compile-exp body)))

    (($ il:id name)
     (name->id name))

    (($ il:kid name)
     (kont->id name))))

(define (compile-jump-table specs)
  (define offset 2) ; closure & continuation
  (define (compile-test params)
    (match params
      (($ il:params self req '() #f '() #f)
       (make-binop '=
                   (make-refine (make-id "arguments")
                                (make-const "length"))
                   (make-const (+ offset (length req)))))
      (($ il:params self req '() rest '() #f)
       (make-binop '>=
                   (make-refine (make-id "arguments")
                                (make-const "length"))
                   (make-const (+ offset (length req)))))
      (($ il:params self req opts #f '() #f)
       (make-binop 'and
                   (make-binop '<=
                               (make-const (+ offset (length req)))
                               (make-refine (make-id "arguments")
                                            (make-const "length")))
                   (make-binop '<=
                               (make-refine (make-id "arguments")
                                            (make-const "length"))
                               (make-const (+ offset (length req) (length opts))))))
      ;; FIXME: need to handle allow-other-keys? and testing for actual keywords
      (($ il:params self req opts #f kwargs _)
       (make-binop '<=
                   (make-const (+ offset (length req)))
                   (make-refine (make-id "arguments")
                                (make-const "length"))))
      ))
  (define (compile-jump params k)
    (match params
      (($ il:params self req '() #f '() #f)
       (list
        (make-return
         (make-call (compile-id k)
                    (cons (compile-id self)
                          (map (lambda (idx)
                                 (make-refine (make-id "arguments")
                                              (make-const (+ offset idx))))
                               (iota (length req))))))))
      (($ il:params self req '() rest '() #f)
       (list
        (bind-rest-args rest (+ offset (length req)))
        (make-return
         (make-call (compile-id k)
                    (append (list (compile-id self))
                            (map (lambda (idx)
                                   (make-refine (make-id "arguments")
                                                (make-const (+ offset idx))))
                                 (iota (length req)))
                            (list (compile-id rest)))))))
      (($ il:params self req opts #f '() #f)
       (append
        (bind-opt-args opts (+ offset (length req)))
        (list
         (make-return
          (make-call (compile-id k)
                     (append (list (compile-id self))
                             (map (lambda (idx)
                                    (make-refine (make-id "arguments")
                                                 (make-const (+ offset idx))))
                                  (iota (length req)))
                             (map compile-id opts)))))))
      (($ il:params self req opts #f ((kws names ids) ...) _)
       (append
        (bind-opt-args opts (+ offset (length req)))
        (bind-kw-args kws names (+ offset (length req)))
        (list
         (make-return
          (make-call (compile-id k)
                     (append (list (compile-id self))
                             (map (lambda (idx)
                                    (make-refine (make-id "arguments")
                                                 (make-const (+ offset idx))))
                                  (iota (length req)))
                             (map compile-id opts)
                             (map compile-id names)))))))
      ))
  (fold-right (lambda (a d)
                (match a
                  ((id params _)
                   (make-branch (compile-test params)
                                (compile-jump params id)
                                (list d)))))
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
        ((pair? c)
         (make-new
          (make-call
           (make-refine *scheme* (make-const "Pair"))
           (list (compile-const (car c))
                 (compile-const (cdr c))))))
        ((vector? c)
         (make-new
          (make-call
           (make-refine *scheme* (make-const "Vector"))
           (map compile-const (vector->list c)))))
        ((char? c)
         (make-new
          (make-call
           (make-refine *scheme* (make-const "Char"))
           (list (make-const (string c))))))
        ((keyword? c)
         (make-new
          (make-call
           (make-refine *scheme* (make-const "Keyword"))
           (list (make-const (symbol->string (keyword->symbol c)))))))
        ((undefined? c)
         (make-refine *scheme* (make-const "UNDEFINED")))
        (else
         (throw 'uncompilable-const c))))
