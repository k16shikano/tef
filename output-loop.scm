;;;; Expand a token list.
;;;; env is a list of eqtb having macro definitions. 
;;;; its key is the name of macros in symbol, 
;;;; and its value is [[parameter token] . [body token]].

(use util.list)
(load "def-macro.scm")
(load "codes.scm")
(load "eqtb.scm")
(load "group.scm")
(load "box.scm")
(load "register.scm")
(load "math.scm")
(load "align.scm")
(load "tokenlist-utils.scm")

(define (init-eqtb)
  (let1 tb (make-eqtb)
    (hash-table-update!
     tb 'mathcode (lambda (old) (alist->hash-table default-mathcodes-list)))
    tb))

;; [token] -> env -> [expanded token]
(define (output ts)
  (let1 ts (expand-all ts (list (init-eqtb)))
	(cond ((null? ts)
	       '())
	      ((not (textoken? (car ts)))
	       ts)
	      ((= (cat (car ts)) 5) ; skip linebreaks
	       (output (cdr ts)))
	      (else
	       (cons (car ts) (output (cdr ts)))))))

;; [ts] -> env -> [ts]
(define (expand-all ts env)
  (cond ((null? ts)
	 '())
	((not (textoken? (car ts)))
	 (cons (car ts) (expand-all (cdr ts) env)))
  	((catcode? (car ts))
	 (receive (num newcode rest)
		  (get-codename (cdr ts) env)
		  (begin (update-catcode! (integer->char num) newcode env)
			 (expand-all rest env))))
  	((mathcode? (car ts))
	 (receive (num newcode rest)
		  (get-codename (cdr ts) env)
		  (begin (update-mathcode! (integer->char num) newcode env)
			 (expand-all rest env))))
  	((register? (car ts))
	 (receive (num val rest)
		  (get-register-value (cdr ts) env)
		  (let1 base (string->symbol #`",(cdar ts)")
		    (cond (val
			   (eqtb-update! (car env) base num val)
			   (expand-all rest env))
			  (else
			   (append
			    (list (find-register-value base num env))
			    (expand-all rest env)))))))
	((if? (car ts))
	 (receive (expanded rest)
		  (process-if ts env)
		  (append expanded
			  (expand-all rest env))))
	((box? (car ts))
	 (let1 boxed (boxen (eval-till-begingroup ts env) env)
	       (append
		(expand-box 
		 `(,(caar boxed) ,(cadar boxed) ,(caddar boxed)
		   ,@(expand-all (cdddar boxed) env)))
		(expand-all (cdr boxed) env))))
	((halign? (car ts))
	 (let1 haligned (haligning (eval-till-begingroup ts env))
	       (append
		`((-103 . ,(align-map 
			    (lambda (content) (expand-all content env))
			    (cdar (expand-halign
				   (caddar haligned)
				   (expand-all (cdddar haligned) env))))))
		(expand-all (cdr haligned) env))))
	((mathchar? (car ts))
	 (receive (mathcharcode rest)
		  (get-mathchar (cdr ts) env)
		  (expand-all (append (list mathcharcode) rest) env)))
	((delimiter? (car ts))
	 (receive (delcode rest)
		  (get-delimiter (cdr ts) env)
		  (expand-all (append (list delcode) rest) env)))
	((fraction? (car ts))
	 (receive (fracspec rest)
		  (get-fracspec (cons (car ts) (expand-all (cdr ts) env)))
		  (cons fracspec rest)))
	((radical? (car ts))
	 (receive (radicalspec rest)
		  (get-delimiter (expand-all (cdr ts) env) env)
		  (cons (cons (car ts) radicalspec) rest)))
	((or (= (cat (car ts)) -1) (= (cat (car ts)) 13))
	 (receive (expanded rest)
		  (eval-control-sequence ts env)
		  (append expanded
			  (expand-all rest env))))
	((begingroup? (car ts))
	 (let1 group (groupen ts (cons (make-eqtb) env))
	       (append 
		`((-100 . ,(expand-all (cdar group)
					 (cons (make-eqtb) env))))
		(expand-all (cdr group) env))))
	((beginmath? (car ts))
	 (let* ((gots   (mathen ts env))
		(limit  (if (null? (car gots)) 1 2))
		(math   (cdar (if (null? (car gots)) (mathen (cdr ts) env) gots)))
		(rest   (if (null? (car gots))
			    (cddr (mathen (cdr ts) env)) (cdr gots))))
	   (append 
	    `(,(mlist (expand-all math env) env limit))
	    (expand-all rest env))))
	((find-catcode (car ts) env)
	 => (lambda (v)
	      (expand-all (cons (cons v (cdar ts)) (cdr ts)) env)))
	(else
	 (cons (car ts) (expand-all (cdr ts) env)))))

(define (eval-till-begingroup ts env)
  (receive (evaled rest)
	   (eval-macro ts env)
	   (if (or (null? rest) (begingroup? (car rest)))
	       (append evaled rest)
	       (append evaled (eval-till-begingroup rest env)))))

;; [token] -> env -> [expanded token] and [rest]
(define (eval-control-sequence ts env)
  (cond
   ((null? ts)
    (values '() '()))
   ((assignment? (car ts))
    (values '() (assignment! ts env #f)))
   ((global? (car ts))
    (values '() (assignment! (cdr ts) env #t)))
   (else
    (eval-macro ts env))))

(define (eval-macro ts env)
  (cond
   ((expandafter? (car ts))
    (receive (expanded rest)
	     (eval-macro (cddr ts) env)
	     (values (expand-all `(,(cadr ts) ,@expanded) env) rest)))
   ((noexpand? (car ts))
    (values
     `(,(cons (or (find-catcode (cadr ts) env) (cat (cadr ts))) (cdadr ts)))
     (cddr ts)))
   ((or
     (and (= -1 (cat (car ts)))
	  (find-macro-definition (token->symbol (cdar ts)) env))
     (and (= 13 (or (find-catcode (car ts) env) (cat (car ts))))
	  (find-activechar-definition (token->symbol (cdar ts)) env)))
    => (lambda (v)
	 (if (and (textoken? (car v)) (< 0 (cat (car v)))) 
	     (values v (cdr ts))
	     (receive (params rest)
		      (match-def-parameter (cdr ts) (car v))
		      (if (null? params)
			  (values (expand-all (cdr v) env) rest)
			  (values (expand-all
				   (apply-pattern (cdr v) params) env) rest))))))
   (else
    (values `(,(car ts)) (cdr ts)))))

;;;; process-if
;; In TeX, conditional statements is processed while its macro expansion.

(define-condition-type <read-if-error> <error> #f)

(define (process-if ts env)
  (define (expand-if test rest)
    (if test
	(expand-true rest)
	(expand-false rest)))
  (cond ((null? ts)
	 (values '() '()))
	((if-type=? "ifnum" (car ts))
	 (receive (test rest)
		  (ifnum-test (cdr ts) env)
		  (expand-if test rest)))
	((if-type=? "ifx" (car ts))
	 (receive (test rest)
		  (ifx-test (cdr ts) env)
		  (expand-if test rest)))
	((if-type=? "if" (car ts))
	 (receive (test rest)
		  (ifchar-test (cdr ts) 'char env)
		  (expand-if test rest)))
	((if-type=? "ifcat" (car ts))
	 (receive (test rest)
		  (ifchar-test (cdr ts) 'cat env)
		  (expand-if test rest)))
	(else
	 (error <read-if-error> "Unknown Type of if" (perror ts)))))

(define (ifnum-test condi env)
  (define (token-compare n1 n2 prod)
;    (let ((n1 (token->number n1))
;	  (n2 (token->number n2)))
      (cond ((char=? (cdar prod) #\<) (< n1 n2))
	    ((char=? (cdar prod) #\=) (= n1 n2))
	    ((char=? (cdar prod) #\>) (> n1 n2))
	    (else (error "Unknown predicate for ifnum"))))
;)
  ((parser-do
    return (token-compare n1 n2 prod)
        in n1   <- (get-tex-int-num env)
	   prod <- (parser-cont (orothers "" #\< #\= #\>) extra-space)
	   n2   <- (get-tex-int-num env))
    (expand-all condi env)))

(define (ifx-test condi env)
  (if (or (null? condi) (null? (cdr condi)))
      (error <read-if-error> "no prameter for ifx")
      (let ((t1 (car condi)) (t2 (cadr condi)))
	(if (= -1 (car t1) (car t2))
	    (let ((t1 (find-definition (token->symbol (cdr t1)) env))
		  (t2 (find-definition (token->symbol (cdr t2)) env)))
	      (values (equal? t1 t2) (cddr condi)))
	    (values (equal? t1 t2) (cddr condi))))))

(define (expand-for-two ts env)
  (receive (expanded rest)
	   (eval-macro ts env)
	   (cond ((<= 2 (length expanded))
		  (append expanded rest))
		 ((null? expanded)
		  (expand-for-two rest env))
		 (else
		  (append expanded 
			  (call-with-values (lambda () (eval-macro rest env))
			    append))))))

(define (ifchar-test condi type env)
  (let* ((ts (expand-for-two condi env))
	 (t1 (and (not (null? ts)) (car ts)))
	 (t2 (and t1 (not (null? (cdr ts))) (cadr ts))))
    (values 
     (cond ((eq? 'char type)
	    (or (= -1 (cat t1) (cat t2))
		(char=? (cdr t1) (cdr t2))))
	   ((eq? 'cat type)
	    (= (cat t1) (cat t2)))
	   (else
	    (error "Unknown if type")))
     (expand-all (cddr ts) env))))

(define (seek-else ts)
  (let R ((ts ts) (body '()))
    (cond ((null? ts)
	   (error <read-if-error> "Unterminated if"))
	  ((if? (car ts))
	   (process-if ts))
	  ((fi? (car ts))
	   (values #f (cdr ts)))
	  ((else? (car ts))
	   (values (reverse body) (cdr ts)))
	  (else
	   (R (cdr ts) (cons (car ts) body))))))

(define (seek-fi ts)
  (let R ((ts ts) (body '()))
    (cond ((null? ts)
	   (error <read-if-error> "Unterminated if"))
	  ((if? (car ts))
	   (process-if ts))
	  ((fi? (car ts))
	   (values (reverse body) (cdr ts)))
	  (else
	   (R (cdr ts) (cons (car ts) body))))))

(define (expand-true ts)
  (receive (true rest)
	   (seek-else ts)
	   (if true
	       (receive (false rest)
			(seek-fi rest)
			(values true rest))
	       (seek-fi ts))))

(define (expand-false ts)
  (receive (true rest)
	   (seek-else ts)
	   (if true 
	       (seek-fi rest)
	       (values '() rest))))

