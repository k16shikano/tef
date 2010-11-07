(define-module register
  (use srfi-1)
  (use read)
  (use show)
  (use tokenlist-utils)
  (use parser-combinator.parser-combinator)
  (use eqtb)
  (use num-dimen)
  (export-all)
)

(select-module register)

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

(define (register! ts env global?)
  (let1 base (string->symbol #`",(cdar ts)")
	(receive (num val rest)
		 (get-register-value base (cdr ts) env)
		 (cond (val
			(eqtb-update! (if global? (last env) (car env))
				      base num val)
			rest)
		       (else
			(append
			 (list (find-register-value base num env))
			 rest))))))

(define (setbox! ts env getter mode global?)
  (receive (num rest)
	   ((get-tex-int-num env) (cdr ts))
	   (receive (boxval rest)
		    ((parser-many
		      (parser-do 
		       return val
		       in eqat <- (parser-cont
				   (skip tex-space1)
				   (orothers "" #\=)
				   (skip tex-space1))
		          val  <- (getter env mode)))
		     rest)
		    (let1 boxval (if (null? boxval) #f boxval)
			  (eqtb-update! (if global? (last env) (car env)) 
					'box num boxval)
			  rest))))

(define (getbox! ts env global?)
  (receive (num rest)
	   ((get-tex-int-num env) (cdr ts))
	   (let1 val (find-register-value 'box num env)
		 (eqtb-delete! (if global? (last env) (car env)) 'box num)
		 (cons 
		  (if val (car val)
		      (error "no value in box" num)) 
		  rest))))

(define (unbox! ts env global?)
  (let1 box (getbox! ts env global?)
	(cons (cadr (car box)) (cdr box))))

(define (copy ts env global?)
  (receive (num rest)
	   ((get-tex-int-num env) (cdr ts))
	   (let1 val (find-register-value 'box num env)
		 (cons 
		  (if val (car val)
		      (error "no value in box" num)) 
		  rest))))

(define (uncopy ts env delete-box? global?)
  (let1 box (copy ts env global?)
	(cons (cadr (car box)) (cdr box))))

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

(define (get-tex-dimen-after str ts env)
  (let ((dimen (match-head ts (string->tokenlist str))))
    (if dimen
	((get-tex-dimen env) dimen)
	(values #f ts))))

(define-syntax register-advance-with
  (syntax-rules ()
    ((_ p ts env global?)
     (let1 base (string->symbol #`",(cdar ts)")
       (receive (void rest)
		((parser-do return 
			      (let1 old (find-register-value base num env)
				(eqtb-update! (if global? (last env) (car env))
					      base num (+ old val))
				#t)
			    in num <- (get-tex-int-num env)
			       by  <- (parser-cont
				       (skip tex-space1)
				       (make-string-parser "by")
				       (skip tex-space1))
			       val <- (p env))
		 (cdr ts))
		rest)))))

(define (do-advance! ts env global?)
  (cond ((count? (car ts))
	 (register-advance-with get-tex-int-num ts env global?))
	((dimen? (car ts))
	 (register-advance-with get-tex-dimen ts env global?))
	(else
	 (error "not implemented" (perror ts)))))

(provide "register")
