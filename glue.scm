(define-module glue
  (use num-dimen)
  (use register)
  (use tokenlist-utils)
  (use internal-value)
  (use parser-combinator.parser-combinator)
  (export hskip? get-hskip)
  )

(select-module glue)

(defpred hskip? "hskip")

(define (get-hskip env mode)
  (lambda (ts)
    (receive (glue rest)
	     ((glue env) ts)
	     (cond ((eq? mode 'H) (values `((HG ,@glue)) rest))
		   ((eq? mode 'M) (values `((MG ,@glue)) rest))
		   (else (error "inhibite mode" (perror ts)))))))

(define (glue env)
  (parser-or
   (parser-cont extra-sign (internal-glue env))
   (parser-do 
    return (list a b c)
    in a <- (get-tex-dimen env) 
       b <- (stretch env)
       c <- (shrink env))))

(define (stretch env)
  (parser-or
   (parser-do return dim 
	      in void <- (make-string-parser "plus")
	         dim  <- (get-tex-dimen env))
   (parser-do return dim
	      in void <- (make-string-parser "plus")
	         dim  <- fil-dimen)
   extra-space))

(define (shrink env)
  (parser-or
   (parser-do return dim 
	      in void <- (make-string-parser "minus")
	         dim  <- (get-tex-dimen env))
   (parser-do return dim
	      in void <- (make-string-parser "minus")
	         dim  <- fil-dimen)
   extra-space))

(define fil-dimen
  (parser-cont
   extra-sign
   tex-factor
   fil-unit
   extra-space1))

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
