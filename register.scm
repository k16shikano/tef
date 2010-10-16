(load "eqtb.scm")
(load "tokenlist-utils.scm")
(load "parser-combinator/parser-combinator.scm")

(define (get-register-value ts env)
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
			    (if (null? alt) #f alt)
					; temporary just for \\count
			    rest))))

(define (find-register-value type num env)
  (cond ((or (not num) (null? env))
	 #f)
	((eqtb-get (car env) type num)
	 => values)
	(else
	 (find-register-value type num (cdr env)))))

;; env -> ([tokenlist] -> integer)
(define (get-tex-int-num env)
  (lambda (ts)
    (guard (e
	    ((<parser-error> e) 
	     (receive (type num-rest)
		      ((parser-or
			(make-command-parser "count")
			(make-command-parser "dimen")) ts)
		      (receive (n rest)
			       ((get-tex-int-num env) num-rest)
			       (values 
				(find-register-value 
				 (string->symbol (cdar type)) n env) rest))))
	    (else (error "parse failed" (perror ts))))
	   (receive (n rest)
		    (tex-int-num ts)
		    (values (tex-int->integer n) rest)))))
