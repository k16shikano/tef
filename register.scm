(load "eqtb.scm")
(load "tokenlist-utils.scm")
(load "num-dimen.scm")
(load "parser-combinator/parser-combinator.scm")

(define (get-register-value base ts env)
  (receive (num rest)
	   ((get-tex-int-num env) ts)
	   (receive (alt rest)
		    ((parser-many
		      (parser-do 
		       return altval
		       in eqat   <- (parser-cont
				     (skip tex-space1)
				     (orothers "" #\=)
				     (skip tex-space1))
		          altval <- (cond ((eq? 'count base)
					   (get-tex-int-num env))
					  ((eq? 'dimen base)
					   (get-tex-dimen env)))
			  ))
		     rest)
		    (values num
			    (if (null? alt) #f alt)
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
	     (receive (base num-rest)
		      ((parser-or
			(make-command-parser "count")
			(make-command-parser "dimen")) ts)
		      (receive (n rest)
			       ((get-tex-int-num env) num-rest)
			       (let1 base (string->symbol (cdar base))
				 (values (find-register-value base n env)
					 rest)))))
	    (else (error "parse failed" (perror ts))))
	   (receive (n rest)
		    (tex-int-num ts)
		    (values (tex-int->integer n) rest)))))

;; [token] -> raional and [token]
(define (get-tex-dimen env)
  (lambda (ts)
    (guard (e
	    ((<parser-error> e)
	     (receive (base num-rest)
		      ((parser-or
			(make-command-parser "count")
			(make-command-parser "dimen")) ts)
		      (receive (v rest)
			       ((get-tex-int-num env) num-rest)
			       (let1 base (string->symbol (cdar base))
				 (if (eq? base 'dimen)
				     (values v rest)
				     ((get-tex-dimen env)
				      (append 
				       (string->tokenlist
					(number->string 
					 (find-register-value base v env)))
				       rest)))))))
	    (else (error "parse failed" (perror ts))))
	   (receive (num-unit rest)
		    (tex-dimen ts)
		    (values (dimen->sp num-unit) rest)))))

(define (register-advance! ts env)
  (cond ((count? (car ts))
	 (register-advance-with get-tex-int-num ts env))
	((dimen? (car ts))
	 (register-advance-with get-tex-dimen ts env))
	(else
	 (error "not implemented" (perror ts)))))

(define-syntax register-advance-with
  (syntax-rules ()
    ((_ p ts env)
     (let1 base (string->symbol #`",(cdar ts)")
       (receive (void rest)
		((parser-do return 
			      (let1 old (find-register-value base num env)
				(eqtb-update! (car env) base num (+ old val))
				#t)
			    in num <- (get-tex-int-num env)
			       by  <- (parser-cont
				       (skip tex-space1)
				       (make-string-parser "by")
				       (skip tex-space1))
			       val <- (p env))
		 (cdr ts))
		rest)))))
