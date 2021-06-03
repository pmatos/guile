;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2021 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; Tailification converts a program so that all calls are tail calls.
;;; It is a minimal form of global CPS conversion that stack-allocates
;;; "return continuations" -- minimal in the sense that the only
;;; additionally residualized continuations are the ones necessary to
;;; preserve the all-tail-calls property.  Notably, loops, conditionals,
;;; and similar features in the source program are left as is unless
;;; it's necessary to split them.
;;;
;;; The first step of tailification computes the set of "tails" in a
;;; function.  The function entry starts a tail, as does each return
;;; point from non-tail calls.  Join points between different tails
;;; also start tails.
;;;
;;; In the residual program, there are four ways that a continuation
;;; exits:
;;;
;;;   - Tail calls in the source program are tail calls in the residual
;;;     program; no change.
;;;
;;;   - For non-tail calls in the source program, the caller saves the
;;;     state of the continuation (the live variables flowing into the
;;;     continuation) on an explicit stack, and saves the label of the
;;;     continuation.  The return continuation will be converted into a
;;;     arity-checking function entry, to handle multi-value returns;
;;;     when it is invoked, it will pop its incoming live variables from
;;;     the continuation stack.
;;;
;;;   - Terms that continue to a join continuation are converted to
;;;     label calls in tail position, passing the state of the
;;;     continuation as arguments.
;;;
;;;   - Returning values from a continuation pops the return label from
;;;     the stack and does an indirect tail label call on that label,
;;;     with the given return values.
;;;
;;; Additionally, the abort-to-prompt run-time routine may unwind the
;;; explicit stack and tail-call a handler continuation.  If the
;;; continuation is not escape-only, then the slice of the continuation
;;; that would be popped off is captured before unwinding.  Resuming a
;;; continuation splats the saved continuation back on the stack and
;;; returns to the top continuation, just as in the tail return case
;;; above.
;;;
;;; We expect that a tailified program will probably be slower than a
;;; non-tailified program.  However a tailified program has a few
;;; interesting properties: the stack is packed and only contains live
;;; data; the stack can be traversed in a portable way, allowing for
;;; implementation of prompts on systems that don't support them
;;; natively; and as all calls are tail calls, the whole system can be
;;; implemented naturally with a driver trampoline on targets that don't
;;; support tail calls (e.g. JavaScript and WebAssembly).
;;;
;;; Code:

(define-module (language cps tailify)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps graphs)
  #:use-module (language cps utils)
  #:use-module (language cps renumber)
  #:use-module (language cps with-cps)
  #:export (tailify))

(define (trivial-intmap x)
  (let ((next (intmap-next x)))
    (and (eqv? next (intmap-prev x))
         next)))

(define (live-constants live-in constants head)
  (intmap-select constants
                 (intset-intersect (intmap-ref live-in head)
                                   (intmap-keys constants))))
(define (live-vars live-in constants head)
  (intset-subtract (intmap-ref live-in head)
                   (intmap-keys constants)))

(define (rename-var* fresh-names var)
  (intmap-ref fresh-names var (lambda (var) var)))
(define (rename-vars* fresh-names vars)
  (match vars
    (() '())
    ((var . vars)
     (cons (rename-var* fresh-names var)
           (rename-vars* fresh-names vars)))))

(define (compute-saved-vars* fresh-names live-in constants reprs k)
  (intset-fold-right
   (lambda (var reprs* vars)
     (values (cons (intmap-ref reprs var) reprs*)
             (cons (rename-var* fresh-names var) vars)))
   (live-vars live-in constants k) '() '()))

(define (tailify-tail cps head body fresh-names winds live-in constants
                      reprs entries original-ktail)
  "Rewrite the conts with labels in the intset BODY, forming the body of
the tail which begins at HEAD in the source program.  The entry to the
tail was already rewritten, with ENTRIES containing an intmap of tail
heads to $kfun labels.  WINDS associates 'unwind primcalls with the
corresponding conts that pushes on the dynamic stack.  LIVE-IN indicates
the variables that are live at tail heads, and CONSTANTS is an intmap
associating vars known to be constant with their values.  REPRS holds
the representation of each var.  ORIGINAL-KTAIL is the tail cont of the
source function; terms in the tail that continue to ORIGINAL-KTAIL will
be rewritten to continue to the tail's ktail."

  ;; HEAD will have been given a corresponding entry $kfun by
  ;; tailify-tails.  Here we find the tail-label for the current tail.
  (define local-ktail
    (match (intmap-ref cps head)
      (($ $kfun src meta self ktail kentry)
       ktail)))

  (pk 'tailify-tail head body fresh-names original-ktail local-ktail)

  (define (rename-var var)   (rename-var* fresh-names var))
  (define (rename-vars vars) (rename-vars* fresh-names vars))
  (define (rename-exp exp)
    (pk 'rename exp
     (rewrite-exp exp
       ((or ($ $const) ($ $prim) ($ $const-fun) ($ $code)) ,exp)
       (($ $call proc args)
        ($call (rename-var proc) ,(rename-vars args)))
       (($ $callk k proc args)
        ($callk k (and proc (rename-var proc)) ,(rename-vars args)))
       (($ $primcall name param args)
        ($primcall name param ,(rename-vars args)))
       (($ $values args)
        ($values ,(rename-vars args))))))

  (define (compute-saved-vars fresh-names k)
    (compute-saved-vars* fresh-names live-in constants reprs k))

  ;; Return a $callk to the join tail with head K.  To allow for
  ;; tail-local names for values bound by K, JOIN-VARS is an alist of
  ;; mappings to add to FRESH-NAMES.
  (define (compute-join-call join-vars k)
    (let ((fresh-names (fold (lambda (pair fresh-names)
                               (match pair
                                 ((old . new)
                                  (intmap-add fresh-names old new))))
                             fresh-names join-vars)))
      (call-with-values (lambda () (compute-saved-vars fresh-names k))
        (lambda (reprs vars)
          (build-exp
            ($callk (intmap-ref entries k) #f vars))))))

  ;; A branch target can either be in the current tail, or it starts a
  ;; join continuation.  It can't be $ktail, it can't be $kreceive, and
  ;; it takes no values, hence we pass () to compute-join-call.
  (define (rewrite-branch-target cps src k)
    (cond
     ((intset-ref body k)
      (with-cps cps k))
     (else
      (when (eqv? k original-ktail) (error "what!!"))
      (with-cps cps
        (letk kcall
              ($kargs () ()
                ($continue local-ktail src ,(compute-join-call '() k))))
        kcall))))
  (define (rewrite-branch-targets cps src k*)
    (match k*
      (()
       (with-cps cps '()))
      ((k . k*)
       (with-cps cps
         (let$ k* (rewrite-branch-targets src k*))
         (let$ k (rewrite-branch-target src k))
         (cons k k*)))))

  ;; Rewrite TERM.  Generally speaking we just rename variable uses.
  ;; However if TERM continues to another tail, we have to generate the
  ;; appropriate call for the continuation tail kind.
  (define (rewrite-term cps term)
    (match term
      (($ $continue k src exp)
       (let ((exp (rename-exp exp)))
         (cond
          ((eqv? k original-ktail)
           (pk 'original-tail-call k exp)
           (match exp
             (($ $values args)
              ;; The original term is a $values in tail position.
              ;; Transform to pop the continuation stack and tail call
              ;; it.
              (with-cps cps
                (letv ret)
                (letk kcall ($kargs ('ret) (ret)
                              ($continue local-ktail src
                                ($calli args ret))))
                (build-term ($continue kcall src
                              ($primcall 'restore1 'ptr ())))))
             ((or ($ $call) ($ $callk) ($ $calli))
              ;; Otherwise the original term was a tail call.
              (with-cps cps
                (build-term ($continue local-ktail src ,exp))))))
          ((intset-ref body k)
           ;; Continuation within current tail.
           (with-cps cps
             (build-term ($continue k src ,exp))))
          (else
           (match (intmap-ref cps k)
             (($ $kreceive)
              ;; A non-tail-call: push the pending continuation and tail
              ;; call instead.
              (pk 'non-tail-call head k exp)
              (match exp
                ((or ($ $call) ($ $callk) ($ $calli))
                 (call-with-values (lambda ()
                                     (compute-saved-vars fresh-names k))
                   (lambda (reprs vars)
                     (pk 'saved-vars reprs vars)
                     (with-cps cps
                       (letk kexp ($kargs () ()
                                    ($continue local-ktail src ,exp)))
                       (letv cont)
                       (letk kcont ($kargs ('cont) (cont)
                                     ($continue kexp src
                                       ($primcall 'save
                                                  (append reprs (list 'ptr))
                                                  ,(append vars (list cont))))))
                       (build-term ($continue kcont src
                                     ($code (intmap-ref entries k))))))))))
             (($ $kargs names vars)
              ;; Calling a join continuation.  This is one of those
              ;; cases where it might be nice in CPS to have names for
              ;; phi predecessor values.  Ah well.
              (let ((vars' (map (lambda (_) (fresh-var)) vars)))
                (with-cps cps
                  (letk kvals
                        ($kargs names vars'
                          ($continue local-ktail src
                            ,(compute-join-call (map cons vars vars') k))))
                  (build-term
                    ($continue kvals src ,exp))))))))))
      (($ $branch kf kt src op param args)
       (with-cps cps
         (let$ kf (rewrite-branch-target src kf))
         (let$ kt (rewrite-branch-target src kt))
         (build-term
           ($branch kf kt src op param ,(rename-vars args)))))
      (($ $switch kf kt* src arg)
       (with-cps cps
         (let$ kf (rewrite-branch-target src kf))
         (let$ kt* (rewrite-branch-targets src kt*))
         (build-term ($switch kf kt* src (rename-var arg)))))
      (($ $prompt k kh src escape? tag)
       (call-with-values (lambda () (compute-saved-vars fresh-names kh))
         (lambda (reprs vars)
           (with-cps cps
             (letv handler)
             (let$ k (rewrite-branch-target src k))
             (letk kpush ($kargs ('handler) (handler)
                           ($continue k src
                             ($primcall 'push-prompt escape?
                                        ((rename-var tag) handler)))))
             (letk kcode ($kargs () ()
                           ($continue kpush src ($code (intmap-ref entries kh)))))
             (build-term ($continue kpush src
                           ($primcall 'save reprs vars)))))))
      (($ $throw src op param args)
       (with-cps cps
         (build-term ($throw src op param ,(rename-vars args)))))))

  ;; A prompt body begins with a $prompt, may contain nested prompt
  ;; bodies, and continues until a corresponding 'unwind primcall.
  ;; Leaving a prompt body may or may not correspond to leaving the
  ;; current tail.  Leaving the prompt body must remove the handler from
  ;; the stack.  Removing the handler must happen before leaving the
  ;; tail, and notably must happen before pushing saved state for a
  ;; non-tail-call continuation.
  (define (maybe-unwind-prompt cps label term)
    (define (not-a-prompt-unwind) (with-cps cps term))
    (define (pop-prompt kh)
      (call-with-values (lambda () (compute-saved-vars fresh-names kh))
        (lambda (reprs vars)
          (with-cps cps
            (letk kterm ($kargs () () ,term))
            (build-term ($continue kterm #f
                          ($primcall 'drop reprs ())))))))
    (cond
     ((intmap-ref winds label (lambda (_) #f))
      => (lambda (wind)
           (match (intmap-ref cps wind)
             (($ $prompt k kh) (pop-prompt kh))
             (_ (not-a-prompt-unwind)))))
     (else (not-a-prompt-unwind))))

  ;; The entry for the current tail has already been rewritten, so here
  ;; we just rewrite all the body conts.
  (intset-fold
   (lambda (label cps)
     (match (pk 'tailify-tail1 head label (intmap-ref cps label))
       ((or ($ $kfun) ($ $kclause) ($ $ktail)) cps) ;; Unchanged.
       (($ $kargs names vals term)
        (with-cps cps
          (let$ term (rewrite-term term))
          (let$ term (maybe-unwind-prompt label term))
          (setk label ($kargs names vals ,(pk 'setting label term)))))))
   body cps))

(define (tailify-tails cps winds live-in constants reprs tails)
  "Given that the conts in a function were partitioned into tails in the
intmap TAILS, mapping tail entries to tail bodies, return a new CPS
program in which the tails have been split to separate functions in
which all calls are tail calls.

WINDS associates 'unwind primcalls with the corresponding conts that
pushes on the dynamic stack.

LIVE-IN indicates the variables that are live at tail heads.

CONSTANTS is an intmap associating vars known to be constant with their
values.

REPRS holds the representation of each var."

  (define (cont-source label)
    (match (intmap-ref cps label)
      (($ $kargs _ _ term)
       (match term
         (($ $continue k src) src)
         (($ $branch k kt src) src)
         (($ $switch k kt* src) src)
         (($ $prompt k kh src) src)
         (($ $throw src) src)))))

  ;; For live values that flow into a tail, each tail will need to give
  ;; them unique names.
  (define fresh-names-per-tail
    (intmap-map (lambda (head body)
                  (intset-fold (lambda (var fresh)
                                 (intmap-add fresh var (pk 'live-in head var
                                                           (fresh-var))))
                               (intmap-ref live-in head)
                               empty-intmap))
                tails))

  (define (compute-saved-vars head)
    (compute-saved-vars* (intmap-ref fresh-names-per-tail head)
                         live-in constants reprs head))

  ;; For a tail whose head in the source program is HEAD, rewrite to be
  ;; a $kfun.  For the "main" tail, no change needed.  For join tails,
  ;; we make an unchecked $kfun-to-$kargs function to which live
  ;; variables are received directly as arguments.  For return tails,
  ;; the live vars are restored from the stack.  In all cases, adjoin a
  ;; HEAD->ENTRY mapping to ENTRIES, where ENTRY is the $kfun label for
  ;; the tail.
  (define (add-entry head body cps entries tails)
    (define fresh-names (intmap-ref fresh-names-per-tail head))
    ;; Constants don't need to be passed from tail to tail; rather they
    ;; are rebound locally.
    (define (restore-constants cps body term)
      (intmap-fold (lambda (var exp cps body term)
                     (define var' (intmap-ref fresh-names var))
                     (with-cps cps
                       (letk k ($kargs ('const) (var') ,term))
                       ($ (values (intset-add body k)
                                  (build-term ($continue k #f ,exp))))))
                   (live-constants live-in constants head)
                   cps body term))
    (define (restore-saved cps body term)
      (call-with-values (lambda () (compute-saved-vars head))
        (lambda (reprs vars)
          (pk 'restoring head reprs vars)
          (define names (map (lambda (_) 'restored) vars))
          (if (null? names)
              (with-cps cps ($ (values body term)))
              (with-cps cps
                (letk krestore ($kargs names vars ,term))
                ($ (values (intset-add body krestore)
                           (build-term ($continue krestore #f
                                         ($primcall 'restore reprs ()))))))))))
    (match (intmap-ref cps head)
      (($ $kfun)
       ;; The main entry.
       (values cps (intmap-add entries head head) tails))
      (($ $kreceive ($ $arity req () rest () #f) kargs)
       ;; The continuation of a non-tail call, or a prompt handler.
       (match (intmap-ref cps kargs)
         (($ $kargs names vars)
          (let ((vars' (map (lambda (_) (fresh-var)) vars))
                (src (cont-source kargs)))
            (let*-values (((cps body term)
                           (restore-constants
                            cps
                            body
                            (build-term
                              ($continue kargs src ($values vars')))))
                          ((cps body term) (restore-saved cps body term)))
              (with-cps cps
                (letk ktail ($ktail))
                (letk krestore ($kargs names vars' ,term))
                (letk kclause ($kclause (req '() rest '() #f) krestore #f))
                (letk kfun ($kfun src '() #f ktail kclause))
                ($ (values
                    (intmap-add entries head kfun)
                    (let ((added (intset kfun kclause krestore ktail))
                          (removed (intset head)))
                      (intmap-add (intmap-remove tails head)
                                  kfun
                                  (intset-subtract (intset-union body added)
                                                   removed)))))))))))
      (($ $kargs names vars term)
       ;; A join point.
       (call-with-values (lambda () (compute-saved-vars head))
         (lambda (reprs vars')
           (define names'
             (let ((names (map cons vars names)))
               (map (lambda (var) (assq-ref names var))
                    vars')))
           (define meta `((arg-representations . ,reprs)))
           (let*-values (((cps body term)
                          (restore-constants cps body term)))
             (with-cps cps
               (letk ktail ($ktail))
               (letk kargs ($kargs names' vars' ,term))
               (letk kfun ($kfun (cont-source head) meta #f ktail kargs))
               ($ (values
                   (intmap-add entries head kfun)
                   (let ((added (intset kfun kargs ktail))
                         (removed (intset head)))
                     (intmap-add (intmap-remove tails head)
                                 kfun
                                 (intset-subtract (intset-union body added)
                                                  removed))))))))))))

  (define original-ktail
    (match (intmap-ref cps (intmap-next tails))
      (($ $kfun src meta self ktail kentry)
       ktail)))
  (call-with-values (lambda ()
                      (intmap-fold (lambda (head body cps entries tails)
                                     (add-entry head body cps entries tails))
                                   tails cps empty-intmap tails))
    (lambda (cps entries tails)
      (intmap-fold
       (lambda (old-head head cps)
         (define fresh-names (intmap-ref fresh-names-per-tail old-head))
         (define body (intmap-ref tails head))
         (tailify-tail cps head body fresh-names winds live-in constants
                       reprs entries original-ktail))
       entries cps))))

(define (compute-tails kfun body preds cps)
  "Compute the set of tails in the function with entry KFUN and body
BODY. Return as an intset mapping the head label for each tail to its
body, as an intset."
  ;; Initially, we start with the requirement that kfun and kreceive
  ;; labels are split heads.
  (define (initial-split label splits)
    (match (intmap-ref cps label)
      ((or ($ $kfun) ($ $kreceive))
       (intmap-add splits label label))
      (_
       splits)))
  ;; Then we build tails by propagating splits forward in the CFG,
  ;; possibly creating new split heads at the dominance frontier.
  (define (compute-split label splits)
    (define (split-head? label)
      (eqv? label (intmap-ref splits label (lambda (_) #f))))
    (define (ktail? label)
      (match (intmap-ref cps label)
        (($ $ktail) #t)
        (_ #f)))
    (cond
     ((split-head? label)
      ;; Once a label is a split head, it stays a split head.
      splits)
     ((ktail? label)
      ;; ktail always part of root tail.
      (intmap-add splits label kfun))
     (else
      (match (intset-fold
              (lambda (pred pred-splits)
                (define split
                  (intmap-ref splits pred (lambda (_) #f)))
                (if (and split (not (memv split pred-splits)))
                    (cons split pred-splits)
                    pred-splits))
              (intmap-ref preds label) '())
        ((split)
         ;; If all predecessors in same split, label is too.
         (intmap-add splits label split (lambda (old new) new)))
        ((_ _ . _)
         ;; Otherwise this is a new split.
         (pk 'join-split label)
         (intmap-add splits label label (lambda (old new) new)))))))
  ;; label -> split head
  (define initial-splits
    (pk (intset-fold initial-split body empty-intmap)))
  (cond
   ((trivial-intmap initial-splits)
    ;; There's only one split head, so only one tail.
    (intmap-add empty-intmap kfun body))
   (else
    ;; Otherwise, assign each label to a tail, identified by the split
    ;; head, then collect the tails by split head.
    (let ((splits (fixpoint
                   (lambda (splits)
                     (pk 'fixpoint splits)
                     (intset-fold compute-split body splits))
                   initial-splits)))
      (intmap-fold
       (lambda (label head split-bodies)
         (intmap-add split-bodies head (intset label) intset-union))
       splits
       empty-intmap)))))

(define (intset-pop set)
  "Return two values: all values in intset SET except the first one, and
first value in SET, or #f if SET was empty."
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define (identify-winds cps kfun body succs)
  "For each unwind primcall in BODY, adjoin an entry mapping it to the
corresponding wind expression."
  (define (visit-label label exits bodies)
    (define wind (intmap-ref bodies label))
    (match (intmap-ref cps label)
      (($ $kargs _ _ ($ $prompt k kh))
       (let* ((bodies (intmap-add bodies k label))
              (bodies (intmap-add bodies kh wind)))
         (values exits bodies)))
      (($ $kargs _ _ ($ $continue k _ ($ $primcall 'wind)))
       (let ((bodies (intmap-add bodies k wind)))
         (values exits bodies)))
      (($ $kargs _ _ ($ $continue k _ ($ $primcall 'unwind)))
       (let* ((exits (intmap-add exits label wind))
              (bodies (intmap-add bodies k (intmap-ref bodies wind))))
         (values exits bodies)))
      (else
       (let ((bodies (intset-fold (lambda (succ bodies)
                                    (intmap-add bodies succ wind))
                                  (intmap-ref succs label)
                                  bodies)))
         (values exits bodies)))))
  (values
   (worklist-fold
    (lambda (to-visit exits bodies)
      (call-with-values (lambda () (intset-pop to-visit))
        (lambda (to-visit label)
          (call-with-values (lambda () (visit-label label exits bodies))
            (lambda (exits* bodies*)
              (if (and (eq? exits exits*) (eq? bodies bodies*))
                  (values to-visit exits bodies)
                  (values (intset-union to-visit (intmap-ref succs label))
                          exits* bodies*)))))))
    (intset kfun)
    empty-intmap
    (intmap-add empty-intmap kfun #f))))

(define (compute-live-in cps body preds)
  "Return an intmap associating each label in BODY with an intset of
live variables flowing into the label."
  (let ((function (intmap-select cps body)))
    (call-with-values
        (lambda ()
          (call-with-values (lambda () (compute-defs-and-uses function))
            (lambda (defs uses)
              ;; Unlike the use of compute-live-variables in
              ;; slot-allocation.scm, we don't need to add prompt
              ;; control-flow edges, as the prompt handler is in its own
              ;; tail and therefore $prompt will push the handler
              ;; continuation (including its needed live vars) before
              ;; entering the prompt body.
              (compute-live-variables preds defs uses))))
      (lambda (live-in live-out)
        live-in))))

(define (compute-constants cps preds)
  "Return an intmap associating each variables BODY to their defining
expression, for all variables binding constant expressions."
  (define (constant? exp)
    (match exp
      ((or ($ $const) ($ $prim) ($ $const-fun) ($ $code)) #t)
      (_ #f)))
  (intmap-fold
   (lambda (label preds constants)
     (cond
      ((trivial-intset preds)
       => (lambda (pred)
            (match (intmap-ref cps pred)
              (($ $continue _ _ (? constant? exp))
               (match (intmap-ref cps label)
                 (($ $kargs (_) (var) _)
                  (intmap-add constants var exp))))
              (_
               constants))))
      (else constants)))
   preds empty-intmap))

(define (tailify-trivial-tail body cps)
  "For the function with body BODY and only one tail, rewrite any return
to tail-call the saved continuation."
  (define (ktail? k)
    (match (intmap-ref cps k)
      (($ $ktail) #t)
      (_ #f)))
  (define (rewrite-return-to-pop-and-calli label cps)
    (match (intmap-ref cps label)
      (($ $kargs names vars
          ($ $continue (? ktail? k) src ($ $values args)))
       ;; The original term is a $values in tail position.
       ;; Transform to pop the continuation stack and tail
       ;; call it.
       (with-cps cps
         (letv ret)
         (letk kcall ($kargs ('ret) (ret)
                       ($continue k src ($calli args ret))))
         (setk label ($kargs names vars
                       ($continue kcall src
                         ($primcall 'restore1 'ptr ()))))))
      (_ cps)))
  (intset-fold rewrite-return-to-pop-and-calli body cps))

(define (tailify-function kfun body cps)
  "Partition the function with entry of KFUN into tails.  Rewrite all
tails in such a way that they enter via a $kfun and leave only via tail
calls."
  (define succs (compute-successors cps kfun))
  (define preds (invert-graph succs))
  (define tails (pk 'tails (compute-tails kfun body preds cps)))
  (cond
   ((trivial-intmap tails)
    (tailify-trivial-tail body cps))
   (else
    ;; Otherwise we apply tailification.
    (let ((winds (identify-winds cps kfun body succs))
          (live-in (compute-live-in cps body preds))
          (constants (compute-constants cps preds))
          (reprs (compute-var-representations cps)))
      (tailify-tails cps winds live-in constants reprs tails)))))

(define (dump* map)
  (intmap-fold (lambda (label cont) (pk label cont) (values)) map)
  map)

(define (tailify cps)
  ;; Renumber so that label order is topological order.
  (let ((cps (renumber cps)))
    (with-fresh-name-state cps
      (dump* (intmap-fold tailify-function
                          (compute-reachable-functions cps)
                          cps)))))
