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

;; [token list with cmd head] -> env -> 
;;    parameter tokens, macro defining tokens, and rest of tokens
(define (grab-macro-definition ts)
  (cond ((null? ts)
	 (values '() '()))
	((= 1 (cat (car ts)))
	 (receive (body rest)
		  (get-tex-group ts)
		  (values '() body rest)))
	(else
	 (receive (param body rest)
		  (grab-macro-definition (cdr ts))
		  (values (cons (car ts) param) body rest)))))

;; [token] -> env -> rest tokens
(define (update-env ts env)
  (receive (param body rest)
	   (grab-macro-definition (cdr ts))
	   (if (< (cat (car ts)) 0)
	       (let ((k (string->symbol (cdar ts)))
		     (b (cons param body)))
		 (if (hash-table-exists? (car env) k)
		     (hash-table-update! (car env) k (lambda (old) b))
		     (hash-table-put! (car env) k b))
		 rest)
	       (error "malformed macro definition"))))

;; symbol -> env
(define (find-macro-definition key env)
  (cond ((null? env)
	 #f)
	((hash-table-get (car env) key #f)
	 => values)
	(else
	 (find-macro-definition key (cdr env)))))

;; [token] -> env -> [expanded token] and [rest]
(define (eval-macro ts env)
  (cond
   ((null? ts)
    (values '() '()))
   ((def? (car ts))
    (values '() (update-env (cdr ts) env)))
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
		 ((= -15 (caar head))
		  (append (driver-loop 
			   (list-ref params (- (x->integer (cdar head)) 1))
			   env)
			  (apply-pattern rest params env)))
		 ((< (cat (car head)) 0)
		  (let ((env (if (def? (car head))
				 (cons (make-hash-table) env)
				 env)))
		    (receive (expanded rest)
			     (eval-macro body env)
			     (append expanded
				     (apply-pattern rest params env)))))
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

(define (expandafter? token)
  (and (< (cat token) 0)
       (string=? "expandafter" (cdr token))))


;; evaluator

(define (eval-box box env) (driver-loop (cdddr box) env))

