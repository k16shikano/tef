;; Treats catcode, mathcode, etc.

;; In tex-modoki, each character is represented as a dotted-pair of 
;; integer and character, while it is just a integer in TeX82. 

(use util.list)

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

(define (update-code! char newcode env)
  (let1 env (car env)
	(if (hash-table-exists? env char)
	    (hash-table-update! env char (lambda (old) newcode))
	    (hash-table-put! env char newcode))))

;; catcode
(define (find-catcode t env)
  (cond ((or (not (textoken? t)) (not t) (null? env))
	 #f)
	((hash-table-get (car env) (cdr t) #f)
	 => (lambda (v) (if (= (car t) v) #f v)))
	(else
	 (find-catcode t (cdr env)))))

;; mathcode
(define default-mathcodes-list
  (list
   '(#\+ . #x2b)
   '(#\- . #x2212)
   '(#\< . #x3c)
   '(#\= . #x3d)
   '(#\> . #x3e)))

(define default-mathcodes 
  (list (alist->hash-table default-mathcodes-list)))

(define (find-mathcode t tbls)
  (cond ((null? tbls) #f)
	((hash-table-get (car tbls) (cdr t) #f)
	 => (cut list <>))
	(else 
	 (find-mathcode t (cdr tbls)))))

