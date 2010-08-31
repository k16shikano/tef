(load "group.scm")
(load "tokenlist-utils.scm")
(load "parser-combinator/parser-combinator.scm")

;;;; def (usre macro)

(define-condition-type <read-parameter-error> <error> #f)

;; [token] -> [parameter-token] and [rest token]
(define (parameter-token ts)
  (cond ((null? ts)
	 (values '() '()))
	((= 6 (cat (car ts)))
	 (if (null? (cdr ts))
	     (values '((11 . #\{)) (cdr ts))
	     (cond ((= 6 (cat (cadr ts)))
		    (values '((6 . #\#)) (cddr ts)))
		   ((and (= 12 (cat (cadr ts)))
			 (char-set-contains? #[1-9] (cdadr ts)))
		    (values `((-15 . ,(x->integer (string (cdadr ts)))))
			    (cddr ts)))
		   ((= 1 (cat (cadr ts)))
		    (values '((11 . #\{)) (cdr ts)))
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
		   ((= -15 (cat (car pt)))
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

;; [token] -> [pattern] -> ([token] and [token])
(define (tail-match token pattern)
  (let ((pattern (cdr pattern)))
    (let R ((param  '()) (target token))
      (cond ((null? target)
	     (values '() target))
	    ((and (charlbrace? (car pattern))  ; for \def#1#{...} pattern
		  (not (charlbrace? (last target))))
	     (R param (append target '((11 . #\{)))))
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

;; [token list with cmd head] -> env -> 
;;    parameter tokens, macro defining tokens, and rest of tokens
(define (grab-macro-definition ts)
  (cond ((null? ts)
	 (values '() '() '()))
	((= 1 (cat (car ts)))
	 (receive (body rest)
		  (get-tex-group ts)
		  (values '() body rest)))
	(else
	 (receive (param body rest)
		  (grab-macro-definition (cdr ts))
		  (values (cons (car ts) param) body rest)))))

;; [token] -> env -> bool -> rest tokens
(define (update-env! ts env global?)
  (let1 env (if global? (last env) (car env))
	(receive (param body rest)
		 (grab-macro-definition (cdr ts))		 
		 (if (= (cat (car ts)) -1)
		     (let ((k (string->symbol (cdar ts)))
			   (b (cons param body)))
		       (if (hash-table-exists? env k)
			   (hash-table-update! env k (lambda (old) b))
			   (hash-table-put! env k b))
		       rest)
		     (error "malformed macro definition" (perror ts))))))

(define (let! ts env global?)
  (let1 lenv (if global? (last env) (car env))
	(receive (t1 rest)
		 ((parser-cont (skip tex-space1) any-token) ts)
		 (receive (e rest)
			  ((skip (tex-other-char #\= "")) rest)
			  (receive (t2 rest)
				   ((parser-cont (skip tex-space1) any-token) rest)
		     (let ((k (string->symbol (cdar t1)))
			   (b (find-macro-definition (token->symbol (cdar t2)) env)))
		       (if (hash-table-exists? lenv k)
			   (hash-table-update! lenv k (lambda (old) b))
			   (hash-table-put! lenv k b))
		       rest))))))

;; symbol -> env
(define (find-macro-definition key env)
  (cond ((or (not key) (null? env))
	 #f)
	((hash-table-get (car env) key #f)
	 => values)
	(else
	 (find-macro-definition key (cdr env)))))

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
   ((let? (car ts))
    (let! (cdr ts) env global?))
   ((def? (car ts))
    (update-env! (cdr ts) env global?))
   ((gdef? (car ts))
    (update-env! (cdr ts) env #t))
   ((edef? (car ts))
    (edef->def ts env))
   ((xdef? (car ts))
    `((-1 . "global") ,@(edef->def ts env)))))







