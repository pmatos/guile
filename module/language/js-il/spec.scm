(define-module (language js-il spec)
  #:use-module (system base language)
  ; #:use-module (language js-il)
  #:use-module (language js-il compile-javascript)
  #:export (js-il))

(define-language js-il
  #:title	"Javascript Intermediate Language"
  #:reader      #f
  #:compilers   `((javascript . ,compile-javascript))
  #:printer	#f ; print-js
  #:for-humans? #f)
