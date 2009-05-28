(add-load-path ".")
(load "tex-modoki.scm")

(define-condition-type <read-parameter-error> <error> #f)

;; [token] -> [parameter-token]
(define (parameter-token ts)
  (cond ((null? ts)
	 '())
	((= 6 (cat (car ts)))
	 (if (null? (cdr ts))
	     (error <read-parameter-error> "unterminated parameter token")
	     (cond ((= 6 (cat (cadr ts)))
		    `((6 . #\#) . ,(parameter-token (cddr ts))))
		   ((and (= 12 (cat (cadr ts)))
			 (char-set-contains? #[1-9] (cdadr ts)))
		    `((-100 . ,(x->integer (string (cdadr ts)))) . 
		      ,(parameter-token (cddr ts))))
		   (else
		    (error <read-parameter-error> "unterminated parameter token")))))
	(else
	 (cons (car ts) (parameter-token (cdr ts))))))

;; [token] -> [[parameter token]] and [def body token] and [rest string token]
(define (parse-parameter paramtexts)
  (let R ((params '())
	  (rest paramtexts))
    (cond ((null? rest)
	   (error <read-parameter-error> "malformed def definition"))
	  ((= 1 (cat (car rest)))
	   (receive (body after)
		    (get-tex-group rest)
		    (values (reverse (map reverse params)) body after)))
	  ((= -100 (cat (car rest)))
	   (R `((,(car rest)) . ,params) (cdr rest)))
	  ((null? params)
	   (R `((,(car rest))) (cdr rest)))
	  (else
	   (R `((,(car rest) . ,(car params)) . ,(cdr params)) (cdr rest))))))

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

;; 



;;; test

(use gauche.test)

(test* "parameter-token: a sample from ch20 of the TeX book." 
       '((11 . #\A) (11 . #\B) (-100 . 1) (-100 . 2) (11 . #\C) (3 . #\$) 
	 (-100 . 3) (12 . #\$) (10 . #\space) 
	 (1 . #\{) (-100 . 3) (1 . #\{) (11 . #\a) (11 . #\b) (-100 . 1) (2 . #\}) 
	 (-100 . 1) (10 . #\space) (11 . #\c) 
	 (6 . #\#) (-10 . "x") (-100 . 2) (2 . #\})) 
       (parameter-token 
	(string->tokenlist "AB#1#2C$#3\\$ {#3{ab#1}#1 c##\\x #2}")))

(test* "parse parameter"
       '(((11 . #\a)) ((-100 . 1) (12 . #\.) (10 . #\space)) ((-100 . 2)))
       (parse-parameter (parameter-token (string->tokenlist "a#1. #2{...}"))))

(test* "match-def-parameter: a sample from ch20 of the TeX book"
       '("You owe {\\$5.00}" "Pay it.")
       (map tokenlist->string 
	    (match-def-parameter
	     (string->tokenlist "You owe {\\$5.00}. Pay it.\\par{...}")
	     (parameter-token (string->tokenlist "#1. #2\\par{...}")))))

(test* "match-def-parameter: a sample from ch20 of the TeX book"
       '("\\LOOK" "" "{And\\$ }{look}")
       (map tokenlist->string 
	    (match-def-parameter
	     (string->tokenlist "AB {\\LOOK}C${And\\$ }{look}\\$ 5.")
	     (parameter-token (string->tokenlist "AB #1#2C$#3\\$ {...}")))))

(test* "match-def-parmeter: rest of tokens"
       "5."
       (tokenlist->string 
	(values-ref
	 (match-def-parameter
	  (string->tokenlist "AB {\\LOOK}C${And\\$ }{look}\\$ 5.")
	  (parameter-token (string->tokenlist "AB #1#2C$#3\\$ {...}")))
	 1)))

