;; in future, this should be merged with ecmacript

(define-module (language javascript spec)
  #:use-module (system base language)
  #:use-module (language javascript)
  #:export (javascript))

(define-language javascript
  #:title	"Javascript"
  #:reader      #f
  #:printer	print-statement
  #:for-humans? #f
  )
