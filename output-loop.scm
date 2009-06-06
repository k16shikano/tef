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
   ((def? (car ts))
    (values '() (update-env (cdr ts) env)))
   ((edef? (car ts))
    (values '() (edef->def ts env)))
   ((expandafter? (car ts))
    (receive (expanded rest)
	     (eval-macro (cddr ts) env)
	     (values (driver-loop `(,(cadr ts) ,@expanded) env) rest)))
   ((box? (car ts))
    (let ((boxed (boxen ts)))
      (values (eval-box (car boxed) env) (cdr boxed))))
   ((find-macro-definition (string->symbol (cdar ts)) env)
    => (lambda (v)
	 (receive (params rest)
		  (match-def-parameter (cdr ts) (car v))
		  (if (null? params)
		      (values (driver-loop (cdr v) env) rest)
		      (values (apply-pattern (cdr v) params env) rest)))))
   (else
    (values `(,(car ts)) (cdr ts)))))

;; [token] -> [[token]] -> env -> [expanded token]
(define (apply-pattern body params env)
  (receive (head rest)
	   (parameter-token body)
	   (cond ((null? body)
		  '())
		 ((parameter? (car head))
		  (append (driver-loop 
			   (list-ref params (- (x->integer (cdar head)) 1))
			   env)
			  (apply-pattern rest params env)))
		 ((def? (car head))
		  (let ((env (cons (make-hash-table) env)))
		    (receive (expanded rest)
			     (eval-macro body env)
			     (append expanded
				     (apply-pattern rest params env)))))
		 ((< (cat (car head)) 0)
		  (receive (expanded rest)
			   (eval-macro (cons (car head)
					     (apply-pattern rest params env)) env)
			   (append expanded rest)))
		 (else
		  (cons (car head)
			(apply-pattern rest params env)))
		 )))

;; [token] -> env -> [expanded token]
(define (driver-loop ts env)
  (cond ((null? ts)
	 '())
	((< (cat (car ts)) 0)
	 (receive (expanded rest)
		  (eval-macro ts env)
		  (append expanded
			  (driver-loop rest env))))
	(else
	 (cons (car ts) (driver-loop (cdr ts) env)))))

;; predicates

(defpred def? "def")
(defpred edef? "edef")
(defpred expandafter? "expandafter")

;; evaluator

(define (eval-box box env) (driver-loop (cdddr box) env))

(define (edef->def ts env)
  (receive (param body rest)
	   (grab-macro-definition (cddr ts))
	   `((-1 . "def") ,(cadr ts) ,@param 
	     (1 . #\{) ,@(driver-loop body env) (2 . #\}) 
	     ,@rest)))
