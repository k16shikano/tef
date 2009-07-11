;;;; Evaluator for TeX macros.
;;;; env is a list of hash-tables having macro definitions. 
;;;; its key is the name of macros in symbol, 
;;;; and its value is [[parameter token] . [body token]].

(load "tex-modoki.scm")
(load "def-macro.scm")
(load "num-dimen.scm")
(load "box.scm")

(define global-env
  (list (make-hash-table)))

;; [token] -> env -> [expanded token] and [rest]
(define (eval-macro ts env)
  (cond
   ((null? ts)
    (values '() '()))
   ((global? (car ts))
    (if (def? (cadr ts))
	(eval-macro `((-1 . "gdef") ,@(cddr ts)) env)))
   ((def? (car ts))
    (values '() (update-env (cdr ts) env)))
   ((edef? (car ts))
    (values '() (edef->def ts env)))
   ((gdef? (car ts))
    (values '() (update-global-env (cdr ts) env)))
   ((expandafter? (car ts))
    (receive (expanded rest)
	     (eval-macro (cddr ts) env)
	     (values (driver-loop `(,(cadr ts) ,@expanded) env) rest)))
   ((find-macro-definition (string->symbol (cdar ts)) env)
    => (lambda (v)
	 (receive (params rest)
		  (match-def-parameter (cdr ts) (car v))
		  (if (null? params)
		      (values (driver-loop (cdr v) env) rest)
		      (values (driver-loop 
			       (apply-pattern (cdr v) params env) env) rest)))))
   (else
    (values #f '()))))
;    (values `(,(car ts)) (cdr ts)))))

(define (edef->def ts env)
  (receive (param body rest)
	   (grab-macro-definition (cddr ts))
	   `((-1 . "def") ,(cadr ts) ,@param 
	     (1 . #\{) ,@(driver-loop body env) (2 . #\}) 
	     ,@rest)))

;; [token] -> [[token]] -> env -> [expanded token]
(define (apply-pattern body params env)
  (receive (head rest)
	   (parameter-token body)
	   (cond ((null? body)
		  '())
		 ((parameter? (car head))
		  (append (list-ref params (- (x->integer (cdar head)) 1))
			  (apply-pattern rest params env)))
		 (else
		  (cons (car head)
			(apply-pattern rest params env)))
		 )))

;; [token] -> env -> [expanded token]
(define (driver-loop ts env)
  (cond ((null? ts)
	 '())
	((begingroup? (car ts))
	 (receive (group rest)
		  (get-tex-group ts)
		  (append (driver-loop group (cons (make-hash-table) env))
			  (driver-loop rest env))))
	((< (cat (car ts)) 0)
	 (receive (expanded rest)
		  (eval-macro ts env)
		  (if expanded
		      (append expanded
			      (driver-loop rest env))
		      (process-primitives ts env))))
	((= (cat (car ts)) 5)
	 (driver-loop (cdr ts) env))
	(else
	 (cons (car ts) (driver-loop (cdr ts) env)))))

;; primitive processors
(define (process-primitives ts env)
  (cond ((box? (car ts))
	 (let ((boxed (boxen ts env)))
	   (values (process-box (car boxed) env) (cdr boxed))))
	(else
	 (cons (car ts) (driver-loop (cdr ts) env)))))
	
(define (process-box box env) (driver-loop (cdddr box) env))


