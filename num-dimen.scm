(define-module num-dimen
  (use srfi-1)
  (use srfi-13)
  (use read)
  (use show)
  (use internal-value)
  (use tokenlist-utils)
  (use parser-combinator.parser-combinator)
  (use eqtb)
  (export-all)
)

(select-module num-dimen)

(define tex-number 
  (parser-cont extra-sign tex-int-num))

(define tex-int-num
  (parser-cont
   extra-sign
   (parser-or (parser-cont tex-int-const)
	      (parser-cont (tex-other-char #\') tex-oct-const)
	      (parser-cont (tex-other-char #\") tex-hex-const)
	      (parser-cont (tex-other-char #\`) char-token)
	      (error "it's not number"))
   (skip extra-space1)))
			
(define (tex-int->integer ts)
  (define (p radix ts)
    (string->number (list->string (map cdr ts)) radix))
  (cond ((char=? #\- (cdar ts)) (* -1 (tex-int->integer (cdr ts))))
	((char=? #\` (cdar ts)) (char->integer (cdadr ts)))
	((char=? #\" (cdar ts)) (p 16 (cdr ts)))
	((char=? #\' (cdar ts)) (p 8  (cdr ts)))
	(else                   (p 10 ts))))

(define tex-oct-digit
  (orothers "octal digit" 0 1 2 3 4 5 6 7))
(define tex-digit
  (orothers "digit" 0 1 2 3 4 5 6 7 8 9))
(define tex-hex-digit
  (parser-or
   (orothers "" 0 1 2 3 4 5 6 7 8 9 #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)
   (oralpha  "" #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)
   (error "it's not hexadecimal digit")))

(define tex-int-const
  (parser-many1 tex-digit))
(define tex-oct-const
  (parser-many1 tex-oct-digit))
(define tex-hex-const
  (parser-many1 tex-hex-digit))
(define char-token
  (make-token-parser
   "character"
   'any
   char?))

(define sign1
  (parser-or (make-token-parser "plus  sign" 12 (cut char=? #\+ <>))
	     (make-token-parser "minus sign" 12 (cut char=? #\- <>))
	     tex-space1
	     (error "there would be sign characters")))

(define extra-sign
  (parser-or 
   (parser-many sign1)
   tex-null))


(define tex-digit-marker
  (parser-or
   (make-token-parser "dot" 12 (cut char=? #\. <>))
   (make-token-parser "dot" 12 (cut char=? #\, <>))))

(define tex-digit-const
  (parser-or
   (parser-cont tex-int-const tex-digit-marker tex-int-const)
   (parser-cont tex-int-const tex-digit-marker)
   (parser-cont tex-digit-marker tex-int-const)
   (parser-cont tex-int-const)
   (error "it's not tex digit constant")))

(define tex-factor
  (parser-or
   tex-digit-const
   tex-int-num))

(define physical-unit
  (parser-cont
   (orstring
    "pt" "pc" "in" "bp" "cm" "mm" "dd" "cc" "sp"   
    (error "it's not a physical unit"))
   extra-space1))

(define (internal-unit env)
  (parser-or (parser-cont (make-string-parser "em") extra-space1)
	     (parser-cont (make-string-parser "ex") extra-space1)
	     (internal-int env)
	     (internal-dimen env)
	     (internal-glue env)
	     (error "it's not an internal unit")))

(define (dimen-unit env)
  (parser-or
   (parser-cont (parser-many (make-string-parser "true"))
		physical-unit)
   (parser-cont (skip extra-space1) (internal-unit env))
   (error "it's not a valid dimension unit")))

(define (tex-dimen env)
  (parser-cont (skip extra-space1) extra-sign tex-factor (dimen-unit env)))

(define (mu-dimen env)
  (parser-cont (skip extra-space1) extra-sign (unsigned-mu-dimen env)))

(define (unsigned-mu-dimen env)
  (parser-or (normal-mu-dimen env) (coerced-mu-dimen env)))

(define (normal-mu-dimen env)
  (parser-cont tex-factor (mu-unit env)))

(define (coerced-mu-dimen env)
  (internal-mu-glue env))

(define (mu-unit env)
  (parser-or
   (parser-cont (skip extra-space1) (internal-mu-glue env))
   (parser-cont (make-text-parser "mu") extra-space1)))

(define tex-register tex-factor)

(define (token->number ts)
  (string->number (tokenlist->string ts)))

(define (dimen->sp num-unit)
  (let* ((dimstr (tokenlist->string num-unit))
	 (true   (string-scan dimstr "true"))
	 (ratio  (car 
		  (filter 
		   car
		   (map 
		    (lambda (x)
		      (cons (string-scan dimstr (car x)) (cdr x)))
		    (zip 
		     '("pt" "in" "pc" "cm" "mm" "bp" "dd" "cc" "sp")
		     `(1 7227/100 12/1 7227/254 7227/2540 
			 7227/7200 1238/1157 14856/1157 65536)))))))
    (* (tex-int->integer 
	(string->tokenlist (string-take dimstr (or true (car ratio)))))
       (/ 1 (cadr ratio))
       65536)))

(provide "num-dimen")
