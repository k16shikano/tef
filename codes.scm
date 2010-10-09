;; Treats catcode, mathcode, etc.

;; In tex-modoki, each character is represented as a dotted-pair of 
;; integer and character, while it is just a integer in TeX82. 

(use util.list)
(load "eqtb.scm")
(load "tokenlist-utils.scm")
(load "parser-combinator/parser-combinator.scm")

(define (get-codename ts)
  (receive (num rest)
	   (tex-int-num ts)
	   (receive (alt rest)
		    ((parser-many
		      (parser-do return altnum
				 in eqat   <- (parser-cont
					       (skip tex-space1)
					       (orothers "" #\=)
					       (skip tex-space1))
				    altnum <- tex-int-num))
		     rest)
		    (values (tex-int->integer num) 
			    (tex-int->integer alt)
			    rest))))

(define (update-catcode! char newcode env)
  (eqtb-update! (car env) 'catcode char newcode))

(define (update-mathcode! char newcode env)
  (eqtb-update! (car env) 'mathcode char newcode))

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

(define default-mathcodes-list
  (list
   '(#\+ . #x2b)
   '(#\- . #x2212)
   '(#\< . #x3c)
   '(#\= . #x3d)
   '(#\> . #x3e)))

