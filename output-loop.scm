;;;; Expand a token list.
;;;; env is a list of eqtb having macro definitions. 
;;;; its key is the name of macros in symbol, 
;;;; and its value is [[parameter token] . [body token]].

(define-module output-loop
  (use util.list)
  (use show)
  (use read)
  (use tokenlist-utils)
  (use parser-combinator.parser-combinator)
  (use def-macro)
  (use box)
  (use eqtb)
  (use codes)
  (use group)
  (use register)
  (use math)
  (use align)
  (export-all)
)

(select-module output-loop)

(define (init-eqtb)
  (let1 tb (make-eqtb)
    (hash-table-update!
     tb 'mathcode (lambda (old) (alist->hash-table default-mathcodes-list)))
    tb))

;; [token] -> env -> [expanded token]
(define (output ts)
  (let1 ts (expand-all ts (list (init-eqtb))'H)
	(cond ((null? ts)
	       '())
	      ((not (textoken? (car ts)))
	       ts)
	      ((= (cat (car ts)) 5) ; skip linebreaks
	       (output (cdr ts)))
	      (else
	       (cons (car ts) (output (cdr ts)))))))

(define (build-para ts)
  (let R ((ts ts) 
	  (env (list (init-eqtb))))
    (cond ((null? ts)
	   '())
	  ((not (textoken? (car ts)))
	   ts)
	  ((= (cat (car ts)) 5) ; skip linebreaks
	   (R (cdr ts) env))
	  (else
	   (receive (para rest)
		    (expand-all ts env 'V)
		    `((V () ,@para) . ,(R rest env)))))))

(define (mk-getter proc)
  (lambda (env mode)
    (lambda (ts) (proc env mode ts))))

(define-macro (expand-with getter result ts env)
  `(receive (got rest)
	    (,getter ,ts ,env)
	    ,result))

;; [ts] -> env -> [ts] and [rest para] if not \end
(define (expand-all ts env mode)
  (define rest-para '())
  (define (expand-append getter env mode ts)
    (receive (got rest)
	     ((getter env mode) ts)
	     (append got (loop rest env mode))))
  (define (prepare-math ts env)
    (cond ((mathchar? (car ts))
	   (expand-with get-mathchar
			(loop (cons got rest) env 'M)
			(cdr ts)
			env))
	  ((delimiter? (car ts))
	   (expand-with get-delimiter
			(loop (cons got rest) env 'M)
			(cdr ts)
			env))
	  ((fraction? (car ts))
	   (expand-with get-fracspec
			(cons got rest)
			(cons (car ts) (loop (cdr ts) env 'M))
			env))
	  ((radical? (car ts))
	   (expand-with get-delimiter
			(cons (cons (car ts) got) rest)
			(loop (cdr ts) env 'M)
			env))
	  ((or (= (cat (car ts)) -1) (= (cat (car ts)) 13))
	   (expand-append (mk-getter eval-control-sequence) env 'M ts))
	  (else
	   (cons (car ts) (loop (cdr ts) env 'M)))))
  (define (loop ts env mode)
    (cond ((null? ts)
	   ts)
	  ((end? ts)
	   '())
	  ((par? (car ts))
	   (set! rest-para (cdr ts))
	   '())
	  ((not (textoken? (car ts)))
	   (cons (car ts) (loop (cdr ts) env mode)))
	  ((if? (car ts))
	   (expand-append (mk-getter process-if) env mode ts))
	  ((box? (car ts))
	   (expand-append get-evaled-box env mode ts))
	  ((halign? (car ts))
	   (expand-append get-evaled-halign env mode ts))
	  ((the? (car ts))
	   (expand-append expand-the env mode ts))
	  ((begingroup? (car ts))
	   (receive (group rest)
		    (get-tex-group ts (cons (make-eqtb) env))
		    (cons
		     (list (loop group (cons (make-eqtb) env) mode))
		     (loop rest env mode))))
	  ((beginmath? (car ts))
	   (receive (limit math rest)
		    (get-mathtokens ts env)
		    (cons
		     (mlist (loop math env 'M) env limit)
		     (loop rest env mode))))
	  ((find-catcode (car ts) env)
	   => (lambda (v)
		(loop (cons (cons v (cdar ts)) (cdr ts)) env mode)))
	  ((eq? 'M mode)
	   (prepare-math ts env))
	  ((or (= (cat (car ts)) -1) (= (cat (car ts)) 13))
	   (expand-append (mk-getter eval-control-sequence) env mode ts))
	  (else
	   (cons (car ts) (loop (cdr ts) env mode)))))

  (values (loop ts env mode) rest-para))

(define (eval-till-begingroup env mode ts)
  (receive (evaled rest)
	   (eval-control-sequence env mode ts)
	   (if (or (null? rest) (list? (car rest)) (begingroup? (car rest)))
	       (append evaled rest)
	       (append evaled (eval-till-begingroup env mode rest)))))

(define (get-evaled-box env mode)
  (lambda (ts)
    (receive (box rest)
	     (get-box-parameter
	      (eval-till-begingroup env mode ts) env)
	     (values
	      (expand-box
	       `(,(car box) ,(cadr box)
		 ,@(expand-all (cddr box) env (box-mode (car box)))))
	      rest))))

(define (get-evaled-halign env mode)
  (lambda (ts)
    (receive (halign rest)
	     (get-halign (eval-till-begingroup env mode ts) env)
	     (values 
	      `((alignment
		 ,@(align-map
		    (lambda (content) (expand-all content env mode))
		    (expand-halign
		     (cadr halign)
		     (expand-all (cddr halign) env mode)))))
	      rest))))

(define (expand-the env mode)
  (lambda (ts)
    (cond ((register? (cadr ts))
	   (receive (num rest)
		    ((get-tex-int-num env) (cddr ts))
		    (values
		     (let1 base (string->symbol #`",(cdadr ts)")
			   (string->tokenlist
			    (x->string
			     (find-register-value base num env))))
		     rest))))))

;; [token] -> env -> [expanded token] and [rest]
(define (eval-control-sequence env mode ts)
  (cond
   ((null? ts)
    (values '() '()))
   ((assignment? (car ts))
    (values '() (assignment! ts env #f)))
   ((register? (car ts))
    (values '() (register! ts env #f)))
   ((setbox? (car ts))
    (values '() (setbox! ts env get-evaled-box mode #f)))
   ((getbox? (car ts))
    (values '() (getbox! ts env #f)))
   ((unbox? (car ts))
    (values '() (unbox! ts env #f)))
   ((copy? (car ts))
    (values '() (copy ts env #f)))
   ((uncopy? (car ts))
    (values '() (uncopy ts env #f)))
   ((advance? (car ts))
    (values '() (advance! ts env #f)))
   ((catcode? (car ts))
    (values '() (catcode! ts env #f)))
   ((mathcode? (car ts))
    (values '() (mathcode! ts env #f)))
   ((global? (car ts))
    (cond ((assignment? (cadr ts))
	   (values '() (assignment! (cdr ts) env #t)))
	  ((register? (cadr ts))
	   (values '() (register! (cdr ts) env #t)))
	  ((setbox? (cadr ts))
	   (values '() (setbox! (cdr ts) env get-evaled-box mode #t)))
	  ((getbox? (cadr ts))
	   (values '() (getbox! (cdr ts) env #t)))
	  ((unbox? (cadr ts))
	   (values '() (unbox! (cdr ts) env #t)))
	  ((copy? (cadr ts))
	   (values '() (copy (cdr ts) env #t)))
	  ((uncopy? (cadr ts))
	   (values '() (uncopy ts env #t)))
	  ((advance? (cadr ts))
	   (values '() (advance! (cdr ts) env #t)))
	  ((advance? (cadr ts))
	   (values '() (advance! (cdr ts) env #t)))
	  ((advance? (cadr ts))
	   (values '() (advance! (cdr ts) env #t)))
	  ))
   (else
    (eval-macro env mode ts))))

(define (eval-macro env mode ts)
  (cond
   ((expandafter? (car ts))
    (receive (expanded rest)
	     (eval-macro env mode (cddr ts))
	     (values (expand-all `(,(cadr ts) ,@expanded) env mode) rest)))
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
			  (values (expand-all (cdr v) env mode) 
				  rest)
			  (values (expand-all 
				   (apply-pattern (cdr v) params) env mode) 
				  rest))))))
   (else
    (values `(,(car ts)) (cdr ts)))))

;; [token] -> env -> bool -> [token]
(define (assignment! ts env global?)
  (cond	
   ((let? (car ts))
    (let! (cdr ts) env global?))
   ((def? (car ts))
    (update-env! (cdr ts) env global?))
   ((gdef? (car ts))
    (update-env! (cdr ts) env #t))
   ((edef? (car ts))
    (edef->def ts env))
   ((xdef? (car ts))
    `((-1 . "global") ,@(edef->def ts env)))
   ((mathchardef? (car ts))
    (mathchardef->def ts env))
;   ((countdef? (car ts))
;    (countdef->def ts))
   (else (error))
   ))

(define (edef->def ts env)
  (receive (param body rest)
	   (grab-macro-definition (cddr ts))
	   `((-1 . "def") ,(cadr ts) ,@param 
	     (1 . #\{) ,@(expand-all body env 'H) (2 . #\}) 
	     ,@rest)))

(define (mathchardef->def ts env)
  (receive (texcharint rest)
	   ((parser-cont (skip tex-space1)
			 (skip (tex-other-char #\= ""))
			 (skip tex-space1)
			 (get-tex-int-num env))
	    (cddr ts))
	   `((-1 . "def") ,(cadr ts) 
	     (1 . #\{) (-1 . "mathchar") 
	     ,@(string->tokenlist (x->string texcharint)) (2 . #\})
	     ,@rest)))


;;;; process-if
;; In TeX, conditional statements is processed while its macro expansion.

(define-condition-type <read-if-error> <error> #f)

(define (process-if env mode ts)
  (define (expand-if test rest)
    (if test
	(expand-true rest env mode)
	(expand-false rest env mode)))
  (cond ((null? ts)
	 (values '() '()))
	((if-type=? "ifnum" (car ts))
	 (receive (test rest)
		  (ifnum-test (cdr ts) env mode)
		  (expand-if test rest)))
	((if-type=? "ifx" (car ts))
	 (receive (test rest)
		  (ifx-test (cdr ts) env mode)
		  (expand-if test rest)))
	((if-type=? "if" (car ts))
	 (receive (test rest)
		  (ifchar-test (cdr ts) 'char env mode)
		  (expand-if test rest)))
	((if-type=? "ifcat" (car ts))
	 (receive (test rest)
		  (ifchar-test (cdr ts) 'cat env mode)
		  (expand-if test rest)))
	(else
	 (error <read-if-error> "Unknown Type of if" (perror ts)))))

(define (ifnum-test condi env mode)
  (define (token-compare n1 n2 prod)
      (cond ((char=? (cdar prod) #\<) (< n1 n2))
	    ((char=? (cdar prod) #\=) (= n1 n2))
	    ((char=? (cdar prod) #\>) (> n1 n2))
	    (else (error "Unknown predicate for ifnum"))))
  ((parser-do
    return (token-compare n1 n2 prod)
        in n1   <- (get-tex-int-num env)
	   prod <- (parser-cont (orothers "" #\< #\= #\>) extra-space)
	   n2   <- (get-tex-int-num env))
    (expand-all condi env mode)))

(define (ifx-test condi env mode)
  (if (or (null? condi) (null? (cdr condi)))
      (error <read-if-error> "no prameter for ifx")
      (let ((t1 (car condi)) (t2 (cadr condi)))
	(if (= -1 (car t1) (car t2))
	    (let ((t1 (find-definition (token->symbol (cdr t1)) env))
		  (t2 (find-definition (token->symbol (cdr t2)) env)))
	      (values (equal? t1 t2) (cddr condi)))
	    (values (equal? t1 t2) (cddr condi))))))

(define (expand-for-two env mode ts)
  (receive (expanded rest)
	   (eval-macro env mode ts)
	   (cond ((<= 2 (length expanded))
		  (append expanded rest))
		 ((null? expanded)
		  (expand-for-two env rest))
		 (else
		  (append expanded 
			  (call-with-values 
			      (lambda () (eval-macro env mode rest))
			    append))))))

(define (ifchar-test condi type env mode)
  (let* ((ts (expand-for-two env mode condi))
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
     (expand-all (cddr ts) env mode))))

(define (seek-else ts env mode)
  (let R ((ts ts) (body '()))
    (cond ((null? ts)
	   (error <read-if-error> "Unterminated if"))
	  ((if? (car ts))
	   (process-if env mode ts))
	  ((fi? (car ts))
	   (values #f (cdr ts)))
	  ((else? (car ts))
	   (values (reverse body) (cdr ts)))
	  (else
	   (R (cdr ts) (cons (car ts) body))))))

(define (seek-fi ts env mode)
  (let R ((ts ts) (body '()))
    (cond ((null? ts)
	   (error <read-if-error> "Unterminated if"))
	  ((if? (car ts))
	   (process-if env mode ts))
	  ((fi? (car ts))
	   (values (reverse body) (cdr ts)))
	  (else
	   (R (cdr ts) (cons (car ts) body))))))

(define (expand-true ts env mode)
  (receive (true rest)
	   (seek-else ts env mode)
	   (if true
	       (receive (false rest)
			(seek-fi rest env mode)
			(values true rest))
	       (seek-fi ts env mode))))

(define (expand-false ts env mode)
  (receive (true rest)
	   (seek-else ts env mode)
	   (if true 
	       (seek-fi rest env mode)
	       (values '() rest))))

(provide "output-loop")
