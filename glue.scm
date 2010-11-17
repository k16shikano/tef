(use output-loop)
(use num-dimen)
(use internal-value)
(use parser-combinator.parser-combinator)

(define (glue env)
  (parser-or
   (parser-cont extra-sign (internal-glue env))
   (parser-cont (tex-dimen env) (stretch env) (shrink env))))

(define (stretch env)
  (parser-or
   (parser-cont (make-string-parser "plus") (tex-dimen env))
   (parser-cont (make-string-parser "plus") fil-dimen)
   extra-space))

(define (shrink env)
  (parser-or
   (parser-cont (make-string-parser "minus") (tex-dimen env))
   (parser-cont (make-string-parser "minus") fil-dimen)
   extra-space))

(define fil-dimen
  (parser-cont
   extra-sign
   tex-factor

(define fil-unit
  (parser-cont
   (make-string-parser "fi")
   (parser-many1 (make-string-parser "l"))))

(define (mu-glue env)
  (parser-or
   (parser-cont extra-sign (internal-mu-glue env))
   (parser-cont (mu-dimen env) (mu-stretch env) (mu-shirink env))))

(define (mu-stretch env)
  (parser-or
   (parser-cont (make-string-parser "plus") (mu-dimen env))
   (parser-cont (make-string-parser "plus") fil-dimen)
   extra-space))

(define (mu-shrink env)
  (parser-or
   (parser-cont (make-string-parser "minus") (mu-dimen env))
   (parser-cont (make-string-parser "minus") fil-dimen)
   extra-space))

