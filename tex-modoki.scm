(use srfi-1)
(use srfi-13)
(use text.tree)

(define (string+char str . char)
  (string-append str (apply string char)))

;; special category code
;;   code < 0     : control sequence
;;     code = -1  : terminated with a character or eof
;;     code = -10 : terminated with space(s)
;;     code = -5  : terminated with newline
;;   others       : TeX category code

;; string-port -> [(code . token)]
(define (read-tex-token . iport)
  (define (in-ctrl-seq c seq p)
    (let ((x (peek-char p)))
      (cond 
       ((eof-object? (peek-char p))
	(cons -1 seq))
       ((char-set-contains? #[\s] x)
	(in-spaces x seq p))
       ((char-set-contains? #[\W\d_] x)
	(cond ((string-null? seq)
	       (cons -1 (string (read-char p))))
	      (else
	       (cons -1 seq))))
       (else 
	(in-ctrl-seq (read-char p) (string+char seq x) p)))))
  (define (in-spaces c seq p)
    (cond ((or (eof-object? (peek-char p))
	       (char-set-contains? #[^\s] (peek-char p)))
	   (if (string-null? seq)
	       (cons 10 #\space)
	       (if (char=? #\newline c)
		   (cons -5 seq)
		   (cons -10 seq))))
	  (else
	   (let* ((readc (read-char p))
		  (nextc (if (char=? #\newline c) #\newline readc)))
	     (in-spaces nextc seq p)))))
  (define (loop c seq p)
    (cond ((eof-object? c)
	   c)
	  ((char=? #\\ c)
	   (read-char p) ; spend a backslash
	   (let ((x (peek-char p)))
	     (cond ((char-set-contains? #[{}$&#^_%~] x)
		    (cons 12 (read-char p)))
		   (else
		    (in-ctrl-seq x "" p)))))
	  ((char=? #\{ c)
	   (cons 1 (read-char p)))
	  ((char=? #\} c)
	   (cons 2 (read-char p)))
	  ((char=? #\$ c)
	   (cons 3 (read-char p)))
	  ((char=? #\& c)
	   (cons 4 (read-char p)))
	  ((char=? #\newline c)
	   (cons 5 (read-char p)))
	  ((char=? #\# c)
	   (cons 6 (read-char p)))
	  ((char=? #\^ c)
	   (cons 7 (read-char p)))
	  ((char=? #\_ c)
	   (cons 8 (read-char p)))
	  ((char=? #\null c)
	   (cons 9 (read-char p)))
	  ((char=? #\space c)
	   (in-spaces (read-char p) seq p))
	  ((char-set-contains? #[a-zA-Z] c)
	   (cons 11 (read-char p)))
	  ((char=? #\~ c)
	   (cons 13 (read-char p)))
	  ((char=? #\% c)
	   (cons 14 (read-char p)))
	  ((char=? #\delete c)
	   (cons 15 (read-char p)))
	  (else
	   (cons 12 (read-char p)))))
  (let ((p (if (null? iport)
	       (current-input-port)
	       (car iport))))
    (loop (peek-char p) "" p)))

(define (perror ls)
  (if (<= (length ls) 20)
      (tokenlist->string ls)
      (string-append (tokenlist->string (take ls 20)) "...")))

;; this gets the head group from a token list,
;; returning the group and the rest of the string in multivalues as tokenlists.
;; [token] -> ([token] and [token])
(define-condition-type <read-group-error> <error> #f)
(define (get-tex-group ls . env)
  (define (in-group ls body i)
    (cond ((null? ls)
	   (error <read-group-error> "unterminated tex group" (perror (reverse body))))
	  ((commenthead? (car ls))
	   (receive (comment rest)
		    (get-comment-line ls)
		    (in-group rest (append (reverse comment) body) i)))
	  ((endgroup? (car ls))
	   (if (= 0 i)
	       (values (reverse body) (cdr ls))
	       (in-group (cdr ls) (cons (car ls) body) (- i 1))))
	  ((begingroup? (car ls))
	   (in-group (cdr ls) (cons (car ls) body) (+ i 1)))
	  (else
	   (in-group (cdr ls) (cons (car ls) body) i))))
  (define (out-group ls)
    (cond ((null? ls)
	   (values '() '()))
	  ((commenthead? (car ls))
	   (receive (comment rest)
		    (get-comment-line ls)
		    (out-group rest)))
	  ((begingroup? (car ls))
	   (in-group (cdr ls) '() 0))
	  ((or (texspaces? (car ls))
	       (par? (car ls)))
	   (out-group (cdr ls)))
	  (else
	   (values `(,(car ls)) (cdr ls))))) ; a token is also a group
  (out-group ls))

;; this gets n groups or tokens at the top of the token list as tokenlists.
;; Integer -> [token] -> ([token] and [token])
(define (get-args n ls)
  (define (trim-texspaces ls)
    (cond ((null? ls) '())
	  ((texspaces? (car ls))
	   (trim-texspaces (cdr ls)))
	  (else
	   ls)))
  (guard (exc
	  ((<read-group-error> exc) (error "missing argument" n (perror ls)))
	  ((<error> exc) (error "missing argument" n (perror ls))))
	 (if (> n 0)
	     (receive (arg unseen)
		      (get-tex-group ls)
		      (receive (got rest)
			       (get-args (- n 1) unseen)
			       (values (cons arg got) rest)))
	     (values '() ls))))

;; this gets a command and its parameters from a token list,
;; returning the before string, the command sequence and the after string in multivalues as tokenlists.
;; String -> Integer -> [token] -> ([token], [token] and [token])
(define (get-command-sequence cmd arity tokenlist . keywords)
  (let-keywords* keywords ((comment? #t))
    (let R ((before '())
	    (cmdseq '())
	    (after  '())
	    (next   tokenlist))
      (cond ((null? next)
	     (values (reverse before)
		     cmdseq
		     after))
	    ((commenthead? (car next))
	     (receive (comment rest)
		      (get-comment-line next)
		      (if comment?
			  (R (append (reverse comment) before)
			     cmdseq after rest)
			  (R before cmdseq after (cdr rest)))))
	    ((< (cat (car next)) 0)
	     (if (string=? cmd (cdar next))
		 (values (reverse before)
			 (cons (car next) (values-ref (get-args arity (cdr next)) 0))
			 (values-ref (get-args arity (cdr next)) 1))
		 (R (cons (car next) before) cmdseq after (cdr next))))
	    (else
	     (R (cons (car next) before) cmdseq after (cdr next)))))))

;; this gets the inline math portion at the head of a token list, 
;; returning a math portion and the rest of the string in multivalues as tokenlists.
;; [token] -> ([token] and [token])
(define-condition-type <read-math-error> <error> #f)
(define (get-inline-math ls)
  (define (in-math ls body)
    (cond ((null? ls)
	   (error <read-math-error> "unterminated math $"))
	  ((mathdollar? (car ls))
	   (values (reverse body) (cdr ls)))
	  (else
	   (in-math (cdr ls) (cons (car ls) body)))))
  (define (out-math ls)
    (cond ((null? ls)
	   (values '() '()))
	  ((mathdollar? (car ls))
	   (in-math (cdr ls) '()))
	  ((texspaces? (car ls))
	   (out-math (cdr ls)))
	  (else
	   (error <read-math-error> "the first token shoule have catcode 3"))))
  (out-math ls))

;; this gets a comment line at the head of a token list,
;; returning a comment line and the rest of the string in multivalues as tokenlists.
;; [token] -> ([token] and [token])
(define-condition-type <read-comment-error> <error> #f)
(define (get-comment-line ls)
  (define (in-comment comment rest)
    (cond ((null? rest)
	   (values (reverse comment) rest))
	  ((or (newline? (car rest))
	       (= -5 (cat (car rest))))
	   (values (reverse comment) rest)) ; with \n
	  (else
	   (in-comment (cons (car rest) comment) (cdr rest)))))
  (define (out-comment ls)
    (cond ((null? ls)
	   (values '() '()))
	  ((commenthead? (car ls))
	   (in-comment `(,(car ls)) (cdr ls))) ; with %
	  ((texspaces? (car ls))
	   (out-comment (cdr ls)))
	  (else
	   (error <read-comment-error> "the first token shoule have catcode 14"))))
  (out-comment ls))

;; utils
(define (textoken? t)
  (and (dotted-list? t)
       (number? (car t))))

(define (cat token)
  (and (textoken? token)
       (car token)))

(define (begingroup? t)
  (and (textoken? t)
       (= 1 (car t))))

(define (endgroup? t)
  (and (textoken? t)
       (= 2 (car t))))

(define (mathdollar? t)
  (and (textoken? t)
       (= 3 (car t))))

(define (texspaces? t)
  (and (textoken? t)
       (or (=  5 (car t))
	   (= 10 (car t))
	   (=  9 (car t)))))

(define (newline? t)
  (and (textoken? t)
       (=  5 (car t))))

(define (commenthead? t)
  (and (textoken? t)
       (=  14 (car t))))

;; this is a kludge aimed for the pretty printing. 
;; i must refactor the line spliting process to treat newlines properly.
(define (newline->par ts post-newline? post-paragraph? commentline?)
  (cond ((null? ts)
	 '())
	((commenthead? (car ts))
	 (cons (car ts)
	       (newline->par (cdr ts) #f #f #t)))
	((newline? (car ts))
	 (cond (commentline?
		(cons (car ts) (newline->par (cdr ts) post-newline? post-paragraph? #f)))
	       (post-newline?
		(newline->par (cdr ts) #f #t #f))
	       (else
		(newline->par (cdr ts) #t #f #f))))
	(post-paragraph?
	 (cons '(-1 . "par") 
	       (cons (car ts) (newline->par (cdr ts) #f #f #f))))
	(post-newline?
	 (cons '(10 . #\space) 
	       (cons (car ts) (newline->par (cdr ts) #f #f #f))))
	(else
	 (cons (car ts) (newline->par (cdr ts) #f #f commentline?)))))

(define (tokenlist->string tls)
  (define (restore-command token)
    (cond ((= (cat token) -1)
	   (cond ((string=? "par" (cdr token))
		  "\n\n")
		 (else
		  (string-append "\\" (x->string (cdr token))))))
	 #;((or (= (cat token) 1)
	       (= (cat token) 2))
	   "")
	  ((and (= (cat token) 12)
		(char-set-contains? #[$%&#_] (cdr token)))
	   (string-append "\\" (x->string (cdr token))))
	  ((= (cat token) -10)
	   (string-append "\\" (x->string (cdr token)) " "))
	  ((= (cat token) -5)
	   (string-append "\\" (x->string (cdr token)) "\n"))
	  (else
	   (cdr token))))
  (tree->string (map restore-command tls)))

(define (string->tokenlist str)
  (with-input-from-string str
    (lambda ()
      (port-map values read-tex-token))))

(define (port->tokenlist p)
  (port->list read-tex-token p))


