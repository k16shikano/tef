(define-module def-macro
  (use srfi-1)
  (use tokenlist-utils)
  (use parser-combinator.parser-combinator)
  (use eqtb)
  (use group)
  (use codes)
  (use register)
  (export-all)
#;  (export match-def-parameter
	  grab-macro-definition
	  find-macro-definition
	  update-env! let!
	  find-activechar-definition
	  find-definition
	  apply-pattern)
  )

(select-module def-macro)

(define-condition-type <read-parameter-error> <error> #f)

;; [token] -> [parameter-token] and [rest token]
(define (parameter-token ts)
  (cond ((null? ts)
	 (values '() '()))
	((list? (car ts))
	 (values (parameter-token (car ts)) (cdr ts)))
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
	     => (cut values (reverse param) <>))
	    (else
	     (let ((target (groupen target)))
	       (R `(,(car target) . ,param) (cdr target))))))))

;; [token] -> ([token] and [token])
(define (single-match token)
  (cond ((null? token)
	 (values '() token))
	((begingroup? (car token))
	 (let ((target (groupen token)))
	   (values (car target) (cdr target))))
	(else
	 (let ((target (groupen token)))
	   (values (reverse `(,(car target))) (cdr target))))))


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
  (let1 tb (if global? (last env) (car env))
    (receive (param body rest)
	(grab-macro-definition (cdr ts))
      (let ((k (token->symbol (cdar ts)))
	    (b (cons param body)))
	(cond ((= (cat (car ts)) -1)
	       (eqtb-update! tb 'control-sequence k b))
	      ((= (or (find-catcode (car ts) env) (cat (car ts))) 13)
	       (eqtb-update! tb 'active-character k b))
	      (else
	       (error "malformed macro definition" (perror ts))))
	rest))))

;; [token] -> env -> bool -> rest tokens
(define (let! ts env global?)
  (let1 tb (if global? (last env) (car env))
    (receive (t1 rest)
	((parser-cont (skip tex-space1) any-token) ts)
      (receive (e rest)
	  ((skip (tex-other-char #\= "")) rest)
	(receive (t2 rest)
	    ((parser-cont (skip tex-space1) any-token) rest)
	  (let ((k (token->symbol (cdar t1)))
		(b (or (find-definition (token->symbol (cdar t2)) env) 
		       t2)))
	    (cond ((= (cat (car ts)) -1)
		   (eqtb-update! tb 'control-sequence k b))
		  ((= (cat (car ts)) 13)
		   (eqtb-update! tb 'active-character k b))
		  (else
		   (error "malformed let" (perror ts))))
	    rest))))))

(define (find-macro-definition key env)
  (cond ((or (not key) (null? env))
	 #f)
	((eqtb-get (car env) 'control-sequence key)
	 => values)
	(else
	 (find-macro-definition key (cdr env)))))

(define (find-activechar-definition key env)
  (cond ((or (not key) (null? env))
	 #f)
	((eqtb-get (car env) 'active-character key)
	 => values)
	(else
	 (find-activechar-definition key (cdr env)))))

(define (find-definition key env)
  (or (find-macro-definition key env)
      (find-activechar-definition key env)))

;; [token] -> [[token]] -> [expanded token]
(define (apply-pattern body params)
  (receive (head rest)
	   (parameter-token body)
	   (cond ((null? body)
		  '())
		 ((parameter? (car head))
		  (append (list-ref params (- (x->integer (cdar head)) 1))
			  (apply-pattern rest params)))
		 (else
		  (cons (car head)
			(apply-pattern rest params)))
		 )))

(provide "def-macro")
