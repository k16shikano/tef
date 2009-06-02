(add-load-path ".")
(load "tex-modoki.scm")

(define-condition-type <read-parameter-error> <error> #f)

;; [token] -> [parameter-token] and [rest token]
(define (parameter-token ts)
  (cond ((null? ts)
	 (values '() '()))
	((= 6 (cat (car ts)))
	 (if (null? (cdr ts))
	     (values '((1 . #\{)) (cddr ts))
;	     (error <read-parameter-error> "unterminated parameter token")
	     (cond ((= 6 (cat (cadr ts)))
		    (values '((6 . #\#)) (cddr ts)))
		   ((and (= 12 (cat (cadr ts)))
			 (char-set-contains? #[1-9] (cdadr ts)))
		    (values `((-100 . ,(x->integer (string (cdadr ts)))))
			    (cddr ts)))
		   (else
		    (error <read-parameter-error> "unterminated parameter token")))))
	(else
	 (values `(,(car ts)) (cdr ts)))))

;; [token] -> [[parameter token]] and [def body token] and [rest string token]
(define (parse-parameter ts)
  (let R ((params '())
	  (ts ts))
    (receive (pt rest)
	     (parameter-token ts)
	     (cond ((or (null? pt)
			(= 1 (cat (car pt))))
		    (receive (body after)
			     (get-tex-group ts)
			     (values (reverse (map reverse params)) body after)))
		   ((= -100 (cat (car pt)))
		    (R `((,(car pt)) . ,params) rest))
		   ((null? params)
		    (R `((,(car pt))) rest))
		   (else
		    (R `((,(car pt) . ,(car params)) . ,(cdr params)) rest))))))

;; [groupen token] -> [token]
(define (treat-group param)
  (define (ex-group token)
    (if (eq? -100 (car token))
	`((1 . #\{) ,@(cdr token) (2 . #\}))
	`(,token)))
  (let ((param (reverse param)))
    (cond ((null? param)
	   '())
	  ((and (null? (cdr param)) ; a group
		(eq? -100 (caar param)))
	   (cdar param))
	  (else
	   (append-map ex-group param)))))

;; [token] -> [head groupen token] . [rest token]
(define (groupen ls)
  (if (null? ls)
      '()
      (if (eq? 1 (cat (car ls)))
	  (receive (group unseen)
		   (get-tex-group ls)
		   (cons `(-100 . ,group) unseen))
	  ls)))

;; [token] -> [token] -> [token] or #f
(define (match-head ls pattern) 
  (if (equal? (map (lambda (p l) l) pattern ls) pattern)
      (drop ls (length pattern))
      #f))

;; [token] -> [pattern] -> ([token] and [token])
(define (tail-match token pattern)
  ;; in this environment, car equal -100 means its cdr is a group
  (let ((pattern (cdr pattern)))
    (let R ((param  '()) (target token))
      (cond ((null? target)
	     (values '() target))
	    ((match-head target pattern)
	     => (cut values (treat-group param) <>))
	    (else
	     (let ((target (groupen target)))
	       (R `(,(car target) . ,param) (cdr target))))))))

;; [token] -> ([token] and [token])
(define (single-match token)
  (cond ((null? token)
	 (values '() token))
	(else
	 (let ((target (groupen token)))
	   (values (treat-group `(,(car target))) (cdr target))))))

;; [token] -> [pattern token] -> ([[parameter token]] and [rest token])
(define (match-def-parameter token patterns)
  (let R ((params '())
	  (rest token)
	  (patterns (parse-parameter patterns)))
    (cond ((null? patterns)
	   (values (reverse params) rest))
	  ((null? rest)
	   (R (cons '() params) '() (cdr patterns)))
	  ((match-head rest (car patterns))
	   => (cut R params <> (cdr patterns)))
	  ((null? (cdar patterns))
	   (receive (param rest-token)
		    (single-match rest)
		    (R (cons param params) rest-token (cdr patterns))))
	  (else
	   (receive (param rest-token)
		    (tail-match rest (car patterns))
		    (R (cons param params) rest-token (cdr patterns)))))))



;;;; Evaluator for TeX macros.
;;;; env is a list of hash-tables having macro definitions. 
;;;; its key is the name of macros in symbol, 
;;;; and its value is [[parameter token] . [body token]].

(define global-env
  (list (make-hash-table)))

(define (def? token)
  (and (= -1 (cat token))
       (string=? "def" (cdr token))))

(define (expandafter? token)
  (and (= -1 (cat token))
       (string=? "expandafter" (cdr token))))

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
		 ((= -100 (caar head))
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




