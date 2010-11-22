(define-module glue
  (use num-dimen)
  (use tokenlist-utils)
  (use internal-value)
  (use parser-combinator.parser-combinator)
  (export-all)
  )

(select-module glue)

(define fil-dimen
  (parser-cont
   extra-sign
   tex-factor
   fil-unit
   extra-space))

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
   (parser-do return dim 
	      in void <- (make-string-parser "plus")
	         dim  <- (mu-dimen env))
   (parser-do return dim
	      in void <- (make-string-parser "plus")
	         dim  <- fil-dimen)
   extra-space))

(define (mu-shrink env)
  (parser-or
   (parser-do return dim 
	      in void <- (make-string-parser "minue")
	         dim  <- (mu-dimen env))
   (parser-do return dim
	      in void <- (make-string-parser "minue")
	         dim  <- fil-dimen)
   extra-space))

(provide "glue")
