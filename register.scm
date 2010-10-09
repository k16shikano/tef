(load "eqtb.scm")
(load "tokenlist-utils.scm")
(load "parser-combinator/parser-combinator.scm")

(define (get-register-value ts)
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
			    (if (null? alt) #f
				(tex-int->integer alt))
					; temporary just for \\count
			    rest))))

(define (find-register-definition type num env)
  (cond ((or (not num) (null? env))
	 #f)
	((eqtb-get (car env) type num)
	 => values)
	(else
	 (find-register-definition type num (cdr env)))))

