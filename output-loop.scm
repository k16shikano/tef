;;;; Evaluator for TeX macros.
;;;; env is a list of hash-tables having macro definitions. 
;;;; its key is the name of macros in symbol, 
;;;; and its value is [[parameter token] . [body token]].

(load "tex-modoki.scm")
(load "parser-utils.scm")
(load "def-macro.scm")
(load "box.scm")

(define global-env
  (list (make-hash-table)))

;; [token] -> env -> [expanded token]
(define (output ts)
  (let1 ts (expand-all ts global-env)
	(cond ((null? ts)
	       '())
	      ((not (textoken? (car ts))) 
	       ts)
	      ((< (cat (car ts)) 0)
	       (process-primitives ts))
	      ((= (cat (car ts)) 5) ; skip linebreaks
	       (output (cdr ts)))
	      (else
	       (cons (car ts) (output (cdr ts)))))))

;; primitive processors
(define (process-primitives ts)
  (cond ((box? (car ts))
	 (let1 boxed (boxen ts)
	       (append (process-box (car boxed)) (cdr boxed))))
	(else
	 (cons (car ts) (output (cdr ts))))))

;; [ts] -> env -> [ts]
(define (expand-all ts env)
  (cond ((null? ts)
	 '())
	((not (textoken? (car ts)))
	 (cons (car ts) (expand-all (cdr ts) env)))
	((begingroup? (car ts))
	 (let1 group (groupen ts)
	       (append 
		`((-100 . ,(expand-all (cdar group) 
					 (cons (make-hash-table) env))))
		(expand-all (cdr group) env))))
	((if? (car ts))
	 (receive (expanded rest)
		  (process-if ts env)
		  (append expanded
			  (expand-all rest env))))
	((= (cat (car ts)) -1)
	 (receive (expanded rest)
		  (eval-macro ts env)
		  (append expanded
			  (expand-all rest env))))
	(else
	 (cons (car ts) (expand-all (cdr ts) env)))))

;; [token] -> env -> [expanded token] and [rest]
(define (eval-macro ts env)
  (cond
   ((null? ts)
    (values '() '()))
   ((assignment? (car ts))
    (values '() (assignment! ts env #f)))
   ((global? (car ts))
    (values '() (assignment! (cdr ts) env #t)))
   ((expandafter? (car ts))
    (receive (expanded rest)
	     (eval-macro (cddr ts) env)
	     (values (expand-all `(,(cadr ts) ,@expanded) env) rest)))
   ((find-macro-definition (token->symbol (cdar ts)) env)
    => (lambda (v)
	 (receive (params rest)
		  (match-def-parameter (cdr ts) (car v))
		  (if (null? params)
		      (values (expand-all (cdr v) env) rest)
		      (values (expand-all
			       (apply-pattern (cdr v) params env) env) rest)))))
   (else
    (values `(,(car ts)) (cdr ts)))))

(define (edef->def ts env)
  (receive (param body rest)
	   (grab-macro-definition (cddr ts))
	   `((-1 . "def") ,(cadr ts) ,@param 
	     (1 . #\{) ,@(expand-all body env) (2 . #\}) 
	     ,@rest)))

;;;; process-if
;; In TeX, conditional statements is processed while its macro expansion.

(define ifnum-param
  (parser-cont tex-int-num  tex-spaces
	       (orothers "" #\< #\= #\>) tex-spaces
	       tex-int-num tex-spaces))

(define-condition-type <read-if-error> <error> #f)

(define (process-if ts env)
  (cond ((null? ts)
	 (values '() '()))
	((if-type=? "ifnum" (car ts))
	 (receive (test rest)
		  (ifnum-test (cdr ts) env)
		  (if test
		      (expand-true rest env)
		      (expand-false rest env))))
	(else
	 (error <read-if-error> "Unknown Type of if" (perror ts)))))

(define (ifnum-test condi env)
  (receive (n1 rest)
	   (tex-int-num (expand-all condi env))
	   (receive (prod rest)
		    ((orothers "" #\< #\= #\>) rest)
		    (receive (n2 rest)
			     (tex-int-num rest)
			     (let* ((n1 (token->number n1))
				    (n2 (token->number n2))
				    (tf (cond ((char=? (cdar prod) #\<) (< n1 n2))
					      ((char=? (cdar prod) #\=) (= n1 n2))
					      ((char=? (cdar prod) #\>) (> n1 n2))
					      (else (error "Unknown predicate for ifnum")))))
			       (values tf rest))))))

(define (seek-else ts env)
  (let R ((ts ts) (body '()))
    (cond ((null? ts)
	   (error <read-if-error> "Unterminated if"))
	  ((if? (car ts))
	   (process-if ts env))
	  ((fi? (car ts))
	   (values #f (cons body ts)))
	  ((else? (car ts))
	   (values (reverse body) (cdr ts)))
	  (else
	   (R (cdr ts) (cons (car ts) body))))))

(define (seek-fi ts env)
  (let R ((ts ts) (body '()))
    (cond ((null? ts)
	   (error <read-if-error> "Unterminated if"))
	  ((if? (car ts))
	   (process-if ts env))
	  ((fi? (car ts))
	   (values (reverse body) (cdr ts)))
	  (else
	   (R (cdr ts) (cons (car ts) body))))))

(define (expand-true ts env)
  (receive (true rest)
	   (seek-else ts env)
	   (if true
	       (receive (false rest)
			(seek-fi rest env)
			(values true rest))
	       (seek-fi ts env))))

(define (expand-false ts env)
  (receive (true rest)
	   (seek-else ts env)
	   (if true 
	       (seek-fi rest env)
	       (values '() rest))))

