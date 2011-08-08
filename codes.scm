;; Treats catcode, mathcode, etc.

;; In tex-modoki, each character is represented as a dotted-pair of 
;; integer and character, while it is just a integer in TeX82. 

(define-module codes
  (use srfi-1)
  (use util.list)
  (use tokenlist-utils)
  (use parser-combinator.parser-combinator)
  (use eqtb)
  (use register)
  (export-all)
)

(select-module codes)

(define (get-codename ts env)
  (receive (num rest)
	   ((get-tex-int-num env) ts)
	   (receive (alt rest)
		    ((parser-many
		      (parser-do return altnum
				 in eqat   <- (parser-cont
					       (skip tex-space1)
					       (orothers "" #\=)
					       (skip tex-space1))
				    altnum <- (get-tex-int-num env)))
		     rest)
		    (values num
			    alt
			    rest))))

(define (update-catcode! char newcode env global?)
  (eqtb-update! (if global? (last env) (car env)) 'catcode char newcode))

(define (update-mathcode! char newcode env global?)
  (eqtb-update! (if global? (last env) (car env)) 'mathcode char newcode))

(define (find-catcode t env)
  (cond ((or (not (textoken? t)) (not t) (null? env))
	 #f)
	((eqtb-get (car env) 'catcode  (cdr t))
	 => (lambda (v) (if (= (car t) v) #f v)))
	(else
	 (find-catcode t (cdr env)))))

(define (find-mathcode t env)
  (cond ((null? env) #f)
	((eqtb-get (car env) 'mathcode (cdr t))
	 => (cut list <>))
	(else 
	 (find-mathcode t (cdr env)))))

(define (catcode! ts env global?)
  (receive (num newcode rest)
	   (get-codename (cdr ts) env)
	   (begin (update-catcode! (integer->char num) newcode env global?)
		  rest)))

(define (mathcode! ts env global?)
  (receive (num newcode rest)
	   (get-codename (cdr ts) env)
	   (begin (update-mathcode! (integer->char num) newcode env global?)
		  rest)))

;; we haven't implemented the bihavior of space factors.
(define (sfcode! ts env global?)
  (receive (num newcode rest)
	   (get-codename (cdr ts) env)
	   (begin (update-mathcode! (integer->char num) newcode env global?)
		  rest)))

;; we haven't supported big/small delimiter yet
(define (delcode! ts env global?)
  (receive (num newcode rest)
	   (get-codename (cdr ts) env)
	   (begin (update-mathcode! (integer->char num) newcode env global?)
		  rest)))

(provide "codes")
