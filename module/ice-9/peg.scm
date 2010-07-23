(define-module (ice-9 peg)
  :export (peg-sexp-compile peg-string-compile context-flatten peg-parse define-nonterm define-nonterm-f peg-match get-code define-grammar define-grammar-f)
  :export-syntax (until-works string-collapse single? push-not-null! single-filter push!)
  :use-module (ice-9 pretty-print))
(define (eeval exp)
  (eval exp (interaction-environment)))

(use-modules (ice-9 pretty-print))
(use-modules (language tree-il))

(eval-when (compile load eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; CONVENIENCE MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eeval exp)
  (eval exp (interaction-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; MACRO BUILDERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Safe-bind helps to bind macros safely.
;; e.g.:
;; (safe-bind
;;  (a b)
;;  `(,a ,b))
;; gives:
;; (#<uninterned-symbol a cc608d0> #<uninterned-symbol b cc608a0>)
(define-macro (safe-bind vals . actions)
  (apply safe-bind-f (cons vals actions)))
(define (safe-bind-f vals . actions)
  `(let ,(map (lambda (val) `(,val (make-symbol ,(symbol->string val)))) vals)
     ,@actions))

;; Unsafe-bind is like safe-bind but uses symbols that are easier to read while
;; debugging rather than safe ones.
(define-macro (unsafe-bind vals . actions)
  (apply unsafe-bind-f (cons vals actions)))
(define (unsafe-bind-f vals . actions)
  `(let ,(map (lambda (val) `(,val ',val)) vals)
     ,@actions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; LOOPING CONSTRUCTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If TST is true, evaluate BODY and try again.
;; (turns out this is built-in, so I don't export it)
;;;;;Old Def:
;; (define-macro (while tst . body)
;;   `(do () ((not ,tst))
;;      ,@body))


;; perform ACTION
;; if it succeeded, return its return value
;; if it failed, run IF_FAILS and try again
;;;;;Old Def:
;; (define-macro (until-works action if-fails)
;;   (safe-bind
;;    (retval)
;;    `(let ((,retval ,action))
;;       (while (not ,retval)
;;              ,if-fails
;;              (set! ,retval ,action))
;;       ,retval)))
(define-syntax until-works
  (lambda (x)
    (syntax-case x ()
      ((_ action if-fails)
       #'(let ((retval action))
           (while (not retval)
                  if-fails
                  (set! retval action)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GENERIC LIST-PROCESSING MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return #t if the list has only one element
;; (calling length all the time on potentially long lists was really slow)
;;;;;Old Def:
;; (define-macro (single? lst)
;;   `(and (list? ,lst) (not (null? ,lst)) (null? (cdr ,lst))))
(define-syntax single?
  (lambda (x)
    (syntax-case x ()
      ((_ lst)
       #'(and (list? lst) (not (null? lst)) (null? (cdr lst)))))))

;; push an object onto a list
;;;;;Old Def:
;; (define-macro (push! lst obj)
;;   `(set! ,lst (cons ,obj ,lst)))
(define-syntax push!
  (lambda (x)
    (syntax-case x ()
      ((_ lst obj)
       #'(set! lst (cons obj lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; CODE GENERATORS
;; These functions generate scheme code for parsing PEGs.
;; Conventions:
;;   accum: (all name body none)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code we generate will be defined in a function, and always has to test
;; whether it's beyond the bounds of the string before it executes.
(define (cg-generic-lambda str strlen at code)
  `(lambda (,str ,strlen ,at)
     (if (>= ,at ,strlen)
         #f
         ,code)))
(define cggl cg-generic-lambda)

;; Optimizations for CG-GENERIC-RET below...
(define *op-known-single-body* '(cg-string cg-peg-any cg-range))
;; ...done with optimizations.

;; Code we generate will have a certain return structure depending on how we're
;; accumulating (the ACCUM variable).
(define (cg-generic-ret accum name body-uneval at)
  (safe-bind
   (body)
   `(let ((,body ,body-uneval))
      ,(cond
        ((and (eq? accum 'all) name body)
         `(list ,at
                (cond
                 ((not (list? ,body)) (list ',name ,body))
                 ((null? ,body) ',name)
                 ((symbol? (car ,body)) (list ',name ,body))
                 (#t (cons ',name ,body)))))
        ((and (eq? accum 'name) name)
         `(list ,at ',name))
        ((and (eq? accum 'body) body)
         (cond
          ((member name *op-known-single-body*)
           `(list ,at ,body))
          (#t `(list ,at
                     (cond
                      ((single? ,body) (car ,body))
                      (#t ,body))))))
        ((eq? accum 'none)
         `(list ,at '()))
        (#t
         (begin
           (pretty-print `(cg-generic-ret-error ,accum ,name ,body-uneval ,at))
           (pretty-print "Defaulting to accum of none.\n")
           `(list ,at '())))))))
(define cggr cg-generic-ret)

;; Generates code that matches a particular string.
;; E.g.: (cg-string "abc" 'body)
(define (cg-string match accum)
  (safe-bind
   (str strlen at)
   (let ((len (string-length match)))
     (cggl str strlen at
           `(if (string=? (substring ,str ,at (min (+ ,at ,len) ,strlen))
                          ,match)
                ,(cggr accum 'cg-string match `(+ ,at ,len))
                #f)))))

;; Generates code for matching any character.
;; E.g.: (cg-peg-any 'body)
(define (cg-peg-any accum)
  (safe-bind
   (str strlen at)
   (cggl str strlen at
         (cggr accum 'cg-peg-any `(substring ,str ,at (+ ,at 1)) `(+ ,at 1)))))

;; Generates code for matching a range of characters between start and end.
;; E.g.: (cg-range #\a #\z 'body)
(define (cg-range start end accum)
  (safe-bind
   (str strlen at c)
   (cggl str strlen at
         `(let ((,c (string-ref ,str ,at)))
            (if (and
                 (char>=? ,c ,start)
                 (char<=? ,c ,end))
                ,(cggr accum 'cg-range `(string ,c) `(+ ,at 1))
                #f)))))

;; Filters the accum argument to cg-match-func four buildings like string
;; literals (since we don't want to tag them with their name if we're doing an
;; "all" accum).
(define (builtin-accum-filter accum)
  (cond
   ((eq? accum 'all) 'body)
   ((eq? accum 'name) 'name)
   ((eq? accum 'body) 'body)
   ((eq? accum 'none) 'none)))
(define baf builtin-accum-filter)

;; Takes a value, prints some debug output, and returns it.
(define (error-val val)
  (begin
    (pretty-print val)
    (pretty-print "Inserting into code for debugging.\n")
    val))

;; Takes an arbitrary expressions and accumulation variable, then parses it.
;; E.g.: (cg-match-func '(and "abc" (or "-" (range #\a #\z))) 'all)
(define (cg-match-func match accum)
   (cond
    ((string? match) (cg-string match (baf accum)))
    ((symbol? match) ;; either peg-any or a nonterminal
     (cond
      ((eq? match 'peg-any) (cg-peg-any (baf accum)))
      ;; if match is any other symbol it's a nonterminal, so just return it
      (#t match)))
    ((or (not (list? match)) (null? match))
     ;; anything besides a string, symbol, or list is an error
     (error-val `(cg-match-func-error-1 ,match ,accum)))
    
    ((eq? (car match) 'range) ;; range of characters (e.g. [a-z])
     (cg-range (cadr match) (caddr match) (baf accum)))
    ((eq? (car match) 'ignore) ;; match but don't parse
     (cg-match-func (cadr match) 'none))
    ((eq? (car match) 'capture) ;; parse
     (cg-match-func (cadr match) 'body))
    ((eq? (car match) 'peg)
     (pattern-builder (cadr match) (baf accum)))
    ((eq? (car match) 'and) (cg-and (cdr match) (baf accum)))
    ((eq? (car match) 'or) (cg-or (cdr match) (baf accum)))
    ((eq? (car match) 'body)
     (if (not (= (length match) 4))
         (error-val `(cg-match-func-error-2 ,match ,accum))
         (apply cg-body (cons (baf accum) (cdr match)))))
    (#t (error-val `(cg-match-func-error-3 ,match ,accum)))))

;;;;; Convenience macros for making sure things come out in a readable form.
(define-macro (single-filter sym) `(if (single? ,sym) (car ,sym) ,sym))
(define-macro (push-not-null! lst obj)
  `(if (not (null? ,obj)) (push! ,lst ,obj)))

;; Top-level function builder for AND.
(define (cg-and arglst accum)
  (safe-bind
   (str strlen at body)
   `(lambda (,str ,strlen ,at)
      (let ((,body '()))
        ,(cg-and-int arglst accum str strlen at body)))))

;; Internal function builder for AND (calls itself).
(define (cg-and-int arglst accum str strlen at body)
  (safe-bind
   (res newat newbody)
   (if (null? arglst)
       (cggr accum 'cg-and `(reverse ,body) at) ;; base case
       (let ((mf (cg-match-func (car arglst) accum))) ;; match function
         `(let ((,res (,mf ,str ,strlen ,at)))
            (if (not ,res) 
                #f ;; if the match failed, the and failed
                ;; otherwise update AT and BODY then recurse
                (let ((,newat (car ,res))
                      (,newbody (cadr ,res)))
                  (set! ,at ,newat)
                  (push-not-null! ,body (single-filter ,newbody))
                  ,(cg-and-int (cdr arglst) accum str strlen at body))))))))

;; Top-level function builder for OR.
(define (cg-or arglst accum)
  (safe-bind
   (str strlen at body)
   `(lambda (,str ,strlen ,at)
      ,(cg-or-int arglst accum str strlen at body))))

;; Internal function builder for OR (calls itself).
(define (cg-or-int arglst accum str strlen at body)
  (safe-bind
   (res)
   (if (null? arglst)
       #f ;; base case
       (let ((mf (cg-match-func (car arglst) accum)))
         `(let ((,res (,mf ,str ,strlen ,at)))
            (if ,res ;; if the match succeeds, we're done
                ,(cggr accum 'cg-or `(cadr ,res) `(car ,res))
                ,(cg-or-int (cdr arglst) accum str strlen at body)))))))

;; Returns a block of code that tries to match MATCH, and on success updates AT
;; and BODY, return #f on failure and #t on success.
(define (cg-body-test match accum str strlen at body)
  (safe-bind
   (at2-body2 at2 body2)
   (let ((mf (cg-match-func match accum)))
     `(let ((,at2-body2 (,mf ,str ,strlen ,at)))
        (if (or (not ,at2-body2) (= ,at (car ,at2-body2)))
            #f
            (let ((,at2 (car ,at2-body2))
                  (,body2 (cadr ,at2-body2)))
              (set! ,at ,at2)
              (push-not-null! ,body (single-filter ,body2))
              #t))))))

;; Returns a block of code that sees whether NUM wants us to try and match more
;; given that we've already matched COUNT.
(define (cg-body-more num count)
  (cond ((number? num) `(< ,count ,num))
        ((eq? num '+) #t)
        ((eq? num '*) #t)
        ((eq? num '?) `(< ,count 1))
        (#t (error-val `(cg-body-more-error ,num ,count)))))


;; Returns a function that takes a paramter indicating whether or not the match
;; was succesful and returns what the body expression should return.
(define (cg-body-ret accum type name body at at2)
  (safe-bind
   (success)
   `(lambda (,success)
      ,(cond ((eq? type '!) `(if ,success #f ,(cggr accum name ''() at)))
             ((eq? type '&) `(if ,success ,(cggr accum name ''() at) #f))
             ((eq? type 'lit)
              `(if ,success ,(cggr accum name `(reverse ,body) at2) #f))
             (#t (error-val
                  `(cg-body-ret-error ,type ,accum ,name ,body ,at ,at2)))))))

;; Returns a block of code that sees whether COUNT satisfies the constraints of
;; NUM.
(define (cg-body-success num count)
  (cond ((number? num) `(= ,count ,num))
        ((eq? num '+) `(>= ,count 1))
        ((eq? num '*) #t)
        ((eq? num '?) `(<= ,count 1))
        (#t `(cg-body-success-error ,num))))

;; Returns a function that parses a BODY element.
(define (cg-body accum type match num)
  (safe-bind
   (str strlen at at2 count body)
   `(lambda (,str ,strlen ,at)
      (let ((,at2 ,at) (,count 0) (,body '()))
        (while (and ,(cg-body-test match accum str strlen at2 body)
                    (set! ,count (+ ,count 1))
                    ,(cg-body-more num count)))
        (,(cg-body-ret accum type 'cg-body body at at2)
         ,(cg-body-success num count))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; FOR DEFINING AND USING NONTERMINALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defines a new nonterminal symbol accumulating with ACCUM.
(define-macro (define-nonterm sym accum match)
  (define-nonterm-f sym accum match))
(define (define-nonterm-f sym accum match)
  (safe-bind
   (res str strlen at body)
   
   ;; (let ((match (if (string? match)
   ;;                  (pattern-builder match accum)
   ;;                  (cg-match-func match accum))))

     (let ((match (cg-match-func match accum)))
     
     (let ((code
            `(lambda (,str ,strlen ,at)
               (let ((,res (,match ,str ,strlen ,at)))
                 (if ,res
                     (let ((,at (car ,res))
                           (,body (cadr ,res)))
                       ,(cond
                         ((eq? accum 'name)
                          `(list ,at ',sym))
                         ((eq? accum 'all)
                          `(list (car ,res)
                                 (cond
                                  ((not (list? ,body)) (list ',sym ,body))
                                  ((null? ,body) ',sym)
                                  ((symbol? (car ,body)) (list ',sym ,body))
                                  (#t (cons ',sym ,body)))))
                         ((eq? accum 'none) `(list (car ,res) '()))
                         (#t (begin `,res))))
                     #f)))))
       (set-symbol-property! sym 'code code)
       `(define ,sym ,code)))))

;; Gets the code corresponding to NONTERM
(define-macro (get-code nonterm)
  `(pretty-print (symbol-property ',nonterm 'code)))

;; Parses STRING using NONTERM
(define (parse nonterm string)
  (string-collapse (nonterm string (string-length string) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; POST-PROCESSING FUNCTIONS (TO CANONICALIZE MATCH TREES)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Is everything in LST true?
(define (andlst lst)
  (or (null? lst)
      (and (car lst) (andlst (cdr lst)))))

;; Is LST a list of strings?
(define (string-list? lst)
  (and (list? lst) (not (null? lst))
       (andlst (map string? lst))))

;; Groups all strings that are next to each other in LST.
(define (string-group lst)
  (if (not (list? lst))
      lst
      (if (null? lst)
          '()
          (let ((next (string-group (cdr lst))))
            (if (not (string? (car lst)))
                (cons (car lst) next)
                (if (and (not (null? next))
                         (list? (car next))
                         (string? (caar next)))
                    (cons (cons (car lst) (car next)) (cdr next))
                    (cons (list (car lst)) next)))))))


;; Collapses all the string in LST.
;; ("a" "b" (c d) "e" "f") -> ("ab" (c d) "ef")
(define (string-collapse lst)
  (if (list? lst)
      (let ((res (map (lambda (x) (if (string-list? x)
                                      (apply string-append x)
                                      x))
                      (string-group (map string-collapse lst)))))
        (if (single? res) (car res) res))
      lst))

;; Makes sure LST is a list.
(define (mklst lst)
  (if (not (list? lst)) (list lst) lst))

;; Takes a list and "flattens" it, using tst to know when to stop instead of
;; terminating on atoms.
(define (flatmaster tst lst)
  (if (or (not (list? lst)) (null? lst))
      lst
      (if (tst lst)
          (list lst)
          (apply append (map (lambda (x) (mklst (flatmaster tst x))) lst)))))

;; Gets the left-hand depth of a list.
(define (depth lst)
  (if (or (not (list? lst)) (null? lst))
      0
      (+ 1 (depth (car lst)))))

;; Trims characters off the front and end of STR.
;; (trim-1chars "'ab'") -> "ab"
(define (trim-1chars str) (substring str 1 (- (string-length str) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; parse string PEGs using sexp PEGs
;; grammar <- (nonterminal '<-' sp pattern)+
;; pattern <- alternative ('/' sp alternative)*
;; alternative <- ([!&]? sp suffix)+
;; suffix <- primary ([*+?] sp)*
;; primary <- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<-'
;; literal <- ['] (!['] .)* ['] sp
;; charclass <- '[' (!']' (charclass-range / charclass-single))* ']' sp
;; charclass-range <- . '-' .
;; charclass-single <- .
;; nonterminal <- [a-zA-Z]+ sp
;; sp <- [ \t\n]*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-nonterm peg-grammar all
  (body lit (and peg-nonterminal (or "<--" "<-" "<") peg-sp peg-pattern) +))
(define-nonterm peg-pattern all
  (and peg-alternative
       (body lit (and (ignore "/") peg-sp peg-alternative) *)))
(define-nonterm peg-alternative all
  (body lit (and (body lit (or "!" "&") ?) peg-sp peg-suffix) +))
(define-nonterm peg-suffix all
  (and peg-primary (body lit (and (or "*" "+" "?") peg-sp) *)))
(define-nonterm peg-primary all
  (or (and "(" peg-sp peg-pattern ")" peg-sp)
      (and "." peg-sp)
      peg-literal
      peg-charclass
      (and peg-nonterminal (body ! "<" 1))))
(define-nonterm peg-literal all
  (and "'" (body lit (and (body ! "'" 1) peg-any) *) "'" peg-sp))
(define-nonterm peg-charclass all
  (and (ignore "[")
       (body lit (and (body ! "]" 1)
                      (or charclass-range charclass-single)) *)
       (ignore "]")
       peg-sp))
(define-nonterm charclass-range all (and peg-any "-" peg-any))
(define-nonterm charclass-single all peg-any)
;; (define-nonterm peg-nonterminal all
;;   (and (body lit (or peg-az peg-AZ) +) peg-sp))
(define-nonterm peg-nonterminal all
  (and (body lit (or (range #\a #\z) (range #\A #\Z)) +) peg-sp))
(define-nonterm peg-sp none
  (body lit (or " " "\t" "\n") *))

;;;;; Testing Code
;; (define-nonterm tst all (body lit "a" +))
;; (pretty-print (parse peg-grammar "as <- 'a'+ !.
;; bs <- 'b'+ !.
;; asorbs <- as / bs
;; tst <- [a-z]+"
;; ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; PARSE STRING PEGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pakes a string representing a PEG grammar and defines all the nonterminals in
;; it as the associated PEGs.
(define (peg-parser str)
  (let ((parsed (parse peg-grammar str)))
    (if (not parsed)
        (begin
          (pretty-print "Invalid PEG grammar!\n")
          #f)
        (let ((lst (cadr parsed)))
          (cond
           ((or (not (list? lst)) (null? lst))
            lst)
           ((eq? (car lst) 'peg-grammar)
            (cons 'begin (map (lambda (x) (peg-parse-nonterm x))
                              (flatmaster (lambda (lst) (<= (depth lst) 2))
                                          (cdr lst))))))))))

;; Parse a nonterminal and pattern listed in LST.
(define (peg-parse-nonterm lst)
  (let ((nonterm (car lst))
        (grabber (cadr lst))
        (pattern (caddr lst)))
    `(define-nonterm ,(string->symbol (cadr nonterm))
       ,(cond
         ((string=? grabber "<--") 'all)
         ((string=? grabber "<-") 'body)
         (#t 'none))
       ,(compressor (peg-parse-pattern pattern)))))

;; Parse a pattern.
(define (peg-parse-pattern lst)
  (cons 'or (map peg-parse-alternative
                 (flatmaster (lambda (x) (eq? (car x) 'peg-alternative))
                             (cdr lst)))))

;; Parse an alternative.
(define (peg-parse-alternative lst)
  (cons 'and (map peg-parse-body (cdr lst))))

;; Parse a body.
(define (peg-parse-body lst)
  (let ((suffix '())
        (front 'lit))
    (cond
     ((eq? (car lst) 'peg-suffix)
      (set! suffix lst))
     ((string? (car lst))
      (begin (set! front (string->symbol (car lst)))
             (set! suffix (cadr lst))))
     (#t `(peg-parse-body-fail ,lst)))
    `(body ,front ,@(peg-parse-suffix suffix))))

;; Parse a suffix.
(define (peg-parse-suffix lst)
  (list (peg-parse-primary (cadr lst))
        (if (null? (cddr lst))
            1
            (string->symbol (caddr lst)))))

;; Parse a primary.
(define (peg-parse-primary lst)
  (let ((el (cadr lst)))
  (cond
   ((list? el)
    (cond
     ((eq? (car el) 'peg-literal)
      (peg-parse-literal el))
     ((eq? (car el) 'peg-charclass)
      (peg-parse-charclass el))
     ((eq? (car el) 'peg-nonterminal)
      (string->symbol (cadr el)))))
   ((string? el)
    (cond
     ((equal? el "(")
      (peg-parse-pattern (caddr lst)))
     ((equal? el ".")
      'peg-any)
     (#t `(peg-parse-any unknown-string ,lst))))
   (#t `(peg-parse-any unknown-el ,lst)))))

;; Parses a literal.
(define (peg-parse-literal lst) (trim-1chars (cadr lst)))

;; Parses a charclass.
(define (peg-parse-charclass lst)
  (cons 'or
        (map
         (lambda (cc)
           (cond
            ((eq? (car cc) 'charclass-range)
             `(range ,(string-ref (cadr cc) 0) ,(string-ref (cadr cc) 2)))
            ((eq? (car cc) 'charclass-single)
             (cadr cc))))
         (flatmaster
          (lambda (x) (or (eq? (car x) 'charclass-range)
                          (eq? (car x) 'charclass-single)))
          (cdr lst)))))

;; Compresses a list to save the optimizer work.
;; e.g. (or (and a)) -> a
(define (compressor lst)
  (if (or (not (list? lst)) (null? lst))
      lst
      (cond
       ((and (or (eq? (car lst) 'or) (eq? (car lst) 'and))
             (null? (cddr lst)))
        (compressor (cadr lst)))
       ((and (eq? (car lst) 'body)
             (eq? (cadr lst) 'lit)
             (eq? (cadddr lst) 1))
        (compressor (caddr lst)))
       (#t (map compressor lst)))))

;; Tests
;; (define-nonterm t1 all
;;   (body lit (and (ignore "/") peg-sp peg-alternative) *))
;; (define-nonterm t2 all (and (ignore "/") peg-sp peg-alternative))

;; Grammar for PEGs in PEG grammar.
(define peg-as-peg
"grammar <- (nonterminal '<-' sp pattern)+
pattern <- alternative ('/' sp alternative)*
alternative <- ([!&]? sp suffix)+
suffix <- primary ([*+?] sp)*
primary <- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<-'
literal <- ['] (!['] .)* ['] sp
charclass <- '[' (!']' (CCrange / CCsingle))* ']' sp
CCrange <- . '-' .
CCsingle <- .
nonterminal <- [a-zA-Z]+ sp
sp <- [ \t\n]*
")

;; Convenience shortcut
(define (pppp x) (pretty-print (peg-parser x)))

;; Builds a lambda-expressions for the pattern STR using accum.
(define (pattern-builder str accum)
  (cg-match-func
   (compressor (peg-parse-pattern (cadr (parse peg-pattern str))))
   accum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; USER-VISIBLE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define peg-sexp-compile cg-match-func)
(define peg-string-compile pattern-builder)

(define context-flatten flatmaster)
(define peg-parse parse)

;; define-nonterm
;; define-nonterm-f

(define-macro (peg-find peg-matcher pattern)
  (peg-find-f peg-matcher pattern))
(define-macro (peg-match peg-matcher pattern)
  (peg-find-f peg-matcher pattern))
(define (peg-find-f peg-matcher pattern)
  (safe-bind
   (at strlen ret end match)
   (let ((cg-match-func
          (if (string? peg-matcher)
              (pattern-builder peg-matcher 'body)
              (cg-match-func peg-matcher 'body))))
     `(let ((,strlen (string-length ,pattern))
            (,at 0))
        (let ((,ret (until-works (or (>= ,at ,strlen)
                                     (,cg-match-func ,pattern ,strlen ,at))
                                 (set! ,at (+ ,at 1)))))
          (if (eq? ,ret #t)
              #f
              (let ((,end (car ,ret))
                    (,match (cadr ,ret)))
                (list ,at ,end (string-collapse ,match)))))))))


(define-macro (define-grammar str)
  (peg-parser str))
(define define-grammar-f peg-parser)

(define-macro (tst x)
  (compile (macroexpand (* x 2)) #:from 'tree-il))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; OLD CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (match-func match accum)
;;   (cond ((string? match)
;;          (let ((len (string-length match)))
;;            `(lambda (str strlen at)
;;               (if (>= at strlen)
;;                   #f
;;                   (if (string=? (substring str at (min (+ at ,len) strlen))
;;                                 ,match)
;;                       (list (+ at ,len) ,match)
;;                       #f)))))
;;         ((symbol? match)
;;          (cond
;;           ((eq? match 'peg-any)
;;            `(lambda (str strlen at)
;;               (if (>= at strlen)
;;                   #f
;;                   (list (+ at 1) (substring str at (+ at 1))))))
;;           ((eq? match 'peg-az)
;;            (match-func '(range #\a #\z) accum))
;;           ((eq? match 'peg-AZ)
;;            (match-func '(range #\A #\Z) accum))
;;           (#t match)))
;;         ((or (not (list? match)) (null? match))
;;          (begin
;;            (pretty-print `(fail-match-func ,match ,accum))
;;            `fail-match-func))
;;         ((eq? (car match) 'range)
;;          `(lambda (str strlen at)
;;             (if (>= at strlen)
;;                 #f
;;                 (let ((c (string-ref str at)))
;;                   (if (and (char>=? c ,(cadr match))
;;                            (char<=? c ,(caddr match)))
;;                       (list (+ at 1) (string c))
;;                       #f)))))
;;         ((eq? (car match) 'ignore)
;;          `(lambda (str strlen at)
;;             (let ((res (,(match-func (cadr match) accum) str strlen at)))
;;               (if res
;;                   (list (car res) '())
;;                   #f))))
;;         ((eq? (car match) 'and)
;;          (build-and-top (cdr match)))
;;         ((eq? (car match) 'or)
;;          (build-or-top (cdr match)))
;;         ((eq? (car match) 'body)
;;          (if (not (= (length match) 4))
;;              (begin
;;                (pretty-print `(fail-match-func-2 ,match ,accum))
;;                `fail-match-func-2)
;;              (apply build-lambda (cdr match))))
;;         (#t (begin
;;               (pretty-print `(fail-match-func-3 ,match ,accum))
;;               `fail-match-func-3))))

;; (define (tst-func match strsym strlensym atsym bodysym)
;;   (let ((at2-body2 (gensym))
;;         (at2 (gensym))
;;         (body2 (gensym)))
;;     `(let ((,at2-body2 (,(match-func match #f) ,strsym ,strlensym ,atsym)))
;;        (if ,at2-body2
;;            (let ((,at2 (car ,at2-body2))
;;                  (,body2 (cadr ,at2-body2)))
;;              (begin
;;                (set! ,atsym ,at2)
;;                (if (not (null? ,body2))
;;                    (push! ,bodysym
;;                           (if (single? ,body2) (car ,body2) ,body2)))
;;                #t))
;;              #f))))

;; (define (check-func num countsym)
;;   (cond ((number? num) `(< ,countsym ,num))
;;         ((eq? num '+) #t)
;;         ((eq? num '*) #t)
;;         ((eq? num '?) `(< ,countsym 1))
;;         (#t `(check-func-error ,num ,countsym))))

;; (define (success-check-func num countsym)
;;   (cond ((number? num) `(= ,countsym ,num))
;;         ((eq? num '+) `(>= ,countsym 1))
;;         ((eq? num '*) #t)
;;         ((eq? num '?) `(<= ,countsym 1))
;;         (#t `(success-check-func-error ,num))))

;; (define (ret-func type atsym at2sym bodysym)
;;   `(lambda (success)
;;      ,(cond ((eq? type '!)
;;              `(if success #f (list ,atsym '())))
;;             ((eq? type '&)
;;              `(if success (list ,atsym '()) #f))
;;             ((eq? type 'lit)
;;              `(if success (list ,at2sym (reverse ,bodysym)) #f))
;;             (#t `(ret-func-error ,type ,atsym ,at2sym ,bodysym)))))

;; (define (build-lambda type match num)
;;   (let ((tst (tst-func match 'str 'strlen 'at2 'body))
;;         (check (check-func num 'count))
;;         (ret (ret-func type 'at 'at2 'body))
;;         (success-check (success-check-func num 'count)))
;;     `(lambda (str strlen at)
;;        (let ((at2 at) (count 0) (body '()))
;;          (while (and ,tst
;;                      (set! count (+ count 1))
;;                      ,check))
;;          (,ret ,success-check)))))

;; (define (build-and-top arglst)
;;   `(lambda (str strlen at)
;;      (let ((body '()))
;;        ,(build-and arglst 'str 'strlen 'at 'body))))

;; (define (build-and arglst strsym strlensym atsym bodysym)
;;   (if (null? arglst)
;;       `(list ,atsym (reverse ,bodysym))
;;       (let ((mf (match-func (car arglst) #f)))
;;         `(let ((res (,mf ,strsym ,strlensym ,atsym)))
;;            (if (not res)
;;                #f
;;                (begin
;;                  (set! ,atsym (car res))
;;                  (if (not (null? (cadr res)))
;;                      (push! ,bodysym
;;                             (if (single? (cadr res)) (caadr res) (cadr res))))
;;                  ,(build-and (cdr arglst) strsym strlensym atsym bodysym)))))))

;; (define (build-or-top arglst)
;;   `(lambda (str strlen at)
;;      (let ((body '()))
;;        ,(build-or arglst 'str 'strlen 'at 'body))))

;; (define (build-or arglst strsym strlensym atsym bodysym)
;;   (if (null? arglst)
;;       #f
;;       (let ((mf (match-func (car arglst) #f)))
;;         `(let ((res (,mf ,strsym ,strlensym ,atsym)))
;;            (if res
;;                (list (car res) (cadr res))
;;                ,(build-or (cdr arglst) strsym strlensym atsym bodysym))))))

;; (define-macro (quoter q) `',q)

;; (define-nonterm abc (body lit "abc" 1))
;; (define-nonterm def (body lit "def" 1))
;; (define-nonterm abcplus (body lit abc +))
;; ;; (pretty-print (match-func '(and (body lit "abc" 1) (body lit "def" 1))))
;; (define-nonterm abcdef (and abc def))
;; (define-nonterm abcordef (or abc def))

;; (define-nonterm c-begin "(*")
;; (define-nonterm c-end "*)")
;; (define-nonterm c-c (and c-begin (body lit c-n *) c-end))
;; (define-nonterm c-n (or c-c (and (body ! c-begin 1) (body ! c-end 1) c-z)))
;; (define-nonterm c-z "a")

;; (define-nonterm c-s (and (body & (and c-a "c") 1)
;;                          (body lit "a" +)
;;                          c-b
;;                          (body ! (or "a" "b" "c") 1)))
;; (define-nonterm c-a (and "a" (body lit c-a ?) "b"))
;; (define-nonterm c-b (and "b" (body lit c-b ?) "c"))

)

(peg-match "'a'+" "bbaa")

(define-syntax inc-sc
  (lambda (x)
    (syntax-case x ()
      ((_ a)
       #`(lambda (y) #,(datum->syntax x (inc-builder 'y 'a)))))))

(define-syntax inc-sc
  (lambda (x)
    (syntax-case x ()
      ((_ a)
       (with-syntax ((inc-body (datum->syntax x (inc-builder 'y 'a))))
                    #`(lambda (y) #,inc-body))))))

(define-syntax inc-sc
  (lambda (x)
    (syntax-case x ()
      ((_ a)
       (with-syntax ((y (datum->syntax x 'y)))
                    #`(lambda (y) #,(inc-body 'a 'y)))))))

(define-macro (inc-dm num)
  (let ((arg (gensym "arg")))
    `(lambda (,arg) ,(inc-builder arg num))))

(define (inc-builder varsym incsym)
  `(+ ,varsym ,incsym))