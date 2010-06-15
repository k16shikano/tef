(load "parser-utils.scm")
(load "parser-combinator.scm")

(define (get-tex-dimen ts)
  (receive (num-unit rest)
	   (tex-dimen ts)
	   (values `((-101 . ,(tokenlist->string num-unit))) rest)))

(define tex-number 
  (parser-cont extra-sign tex-int-num))

(define tex-int-num
  (parser-cont
   (parser-or (parser-cont tex-int-const)
	      (parser-cont (tex-other-char #\') tex-oct-const)
	      (parser-cont (tex-other-char #\") tex-hex-const)
	      (parser-cont (tex-other-char #\`) char-token)
	      (error "it's not number"))
   extra-space1))

(define tex-oct-digit
  (orothers "octal digit" 0 1 2 3 4 5 6 7))
(define tex-digit
  (orothers "digit" 0 1 2 3 4 5 6 7 8 9))
(define tex-hex-digit
  (parser-or
   (orothers "" 0 1 2 3 4 5 6 7 8 9 #\A #\B #\C #\D #\E #\F)
   (oralpha  "" #\A #\B #\C #\D #\E #\F)
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

(define extra-space1
  (parser-or tex-space1 tex-null))
(define extra-space
  (parser-or tex-null (parser-cont tex-space1 extra-space)))

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

(define inner-unit
  (parser-or (parser-cont (make-string-parser "em") extra-space1)
	     (parser-cont (make-string-parser "ex") extra-space1)
	     (error "it's not an inner unit")))

(define dimen-unit
  (parser-or
   (parser-cont (parser-many (make-string-parser "true"))
		physical-unit)
   (parser-cont (parser-many tex-space1) inner-unit)
   (error "it's not a valid dimension unit")))

(define tex-dimen
  (parser-cont extra-sign tex-factor dimen-unit))

(define tex-register tex-factor)

(define (get-tex-dimen-after str ts)
  (let ((dimen (match-head ts (string->tokenlist str))))
    (if dimen
	(get-tex-dimen dimen)
	(values '((-101 . #f)) ts))))

(define (token->dimen ts)
  (if (not (= -101 (car ts))) (error "here expects dimensions")
      (cdr ts)))


#;(define (get-tex-dimen ts)
  (receive (num-unit rest)
	   (tex-dimen ts)
	   (values `((-101 . ,(tokenlist->string num-unit))) rest)))
