(define-module (language js-il direct)
  #:use-module (ice-9 match)
  #:use-module (language js-il)
  #:export (remove-immediate-calls))

(define (remove-immediate-calls exp)
  (match exp
    (($ program entry body)
     (make-program (remove-immediate-calls entry)
                   (map remove-immediate-calls body)))

    (($ continuation params body)
     (make-continuation params (remove-immediate-calls body)))

    (($ function name params body)
     (make-function name params (remove-immediate-calls body)))

    (($ local
        (($ var id ($ continuation () body)))
        ($ continue id ()))
     (remove-immediate-calls body))

    (($ local
        (($ var id ($ continuation (arg) body)))
        ($ continue id (val)))
     (make-local (list (make-var arg val))
                 (remove-immediate-calls body)))

    (($ local bindings body)
     (make-local (map remove-immediate-calls bindings)
                 (remove-immediate-calls body)))

    (($ var id exp)
     (make-var id (remove-immediate-calls exp)))

    (exp exp)))
