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
   ((assignment? (car ts))
    (values '() (assignment! ts env #f)))
   ((global? (car ts))
    (values '() (assignment! (cdr ts) env #t)))
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

;; [token] -> env -> bool -> [token]
(define (assignment! ts env global?)
  (cond
   ((def? (car ts))
    (update-env! (cdr ts) env global?))
   ((gdef? (car ts))
    (update-env! (cdr ts) env #t))
   ((edef? (car ts))
    (edef->def ts env))
   ((xdef? (car ts))
    `((-1 . "global") ,@(edef->def ts env)))))

(define (edef->def ts env)
  (receive (param body rest)
	   (grab-macro-definition (cddr ts))
	   `((-1 . "def") ,(cadr ts) ,@param 
	     (1 . #\{) ,@(driver-loop body env) (2 . #\}) 
	     ,@rest)))


;; [token] -> env -> [expanded token]
(define (driver-loop ts env)
  (cond ((null? ts)
	 '())
	((not (textoken? (car ts))) ts)
	((begingroup? (car ts))
	 (let1 group (groupen ts)
	       (append `((-100 . ,(driver-loop (cdar group) (cons (make-hash-table) env))))
		       (driver-loop (cdr group) env))))
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
	   (append (process-box (car boxed) env) (cdr boxed))))
	(else
	 (cons (car ts) (driver-loop (cdr ts) env)))))
	

