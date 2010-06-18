(use srfi-1)
(use srfi-13)
(use text.tree)
(load "parser-utils.scm")

;; special category code
;;   code < 0     : control sequence
;;     code = -1  : terminated with a character or eof
;;   others       : TeX category code

;; string-port -> [(code . token)]
(define (read-tex-token . iport)
  (define (in-ctrl-seq c seq p)
    (let ((x (peek-char p)))
      (cond 
       ((or (eof-object? (peek-char p))
	    (char=? #\newline c))
	(cons -1 seq))
       ((char-set-contains? #[\s] x)
	(in-spaces x seq #t #f p))
       ((char-set-contains? #[\W\d_] x)
	(cond ((string-null? seq)
	       (cons -1 (string (read-char p))))
	      (else
	       (cons -1 seq))))
       (else
	(in-ctrl-seq (read-char p) (string+char seq x) p)))))
  (define (in-spaces c seq S? N? p)
    (cond ((or (eof-object? (peek-char p))
	       (char-set-contains? #[^\s] (peek-char p)))
	   (cond (N? (cons -1 "par"))
		 ((not (string-null? seq))
		  (cons -1 seq))
		 (S? )
		 (else
		  (cons 10 #\space))))
	  (else
	   (in-spaces (read-char p) seq S? N? p))))
  (define (in-comment c p) 
    (cond ((char=? #\newline (peek-char p))
	   (read-char p)
	   (cons 5 #\newline))
	  (else
	   (in-comment (read-char p) p))))
  (define (in-newlines c N? p)
    (cond ((char-set-contains? #[\s] (peek-char p))
	   (in-spaces (read-char p) "" #t #t p))
	  (N? (cons -1 "par"))
	  (else
	   (cons 10 #\space))))
  (define (loop c p)
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
	   (in-newlines (read-char p) #f p))
	  ((char=? #\# c)
	   (cons 6 (read-char p)))
	  ((char=? #\^ c)
	   (cons 7 (read-char p)))
	  ((char=? #\_ c)
	   (cons 8 (read-char p)))
	  ((char=? #\null c)
	   (cons 9 (read-char p)))
	  ((char=? #\space c)
	   (in-spaces (read-char p) "" #f #f p))
	  ((char-set-contains? #[a-zA-Z] c)
	   (cons 11 (read-char p)))
	  ((char=? #\~ c)
	   (cons 13 (read-char p)))
	  ((char=? #\% c)
	   (in-comment (read-char p) p))
	  ((char=? #\delete c)
	   (cons 15 (read-char p)))
	  (else
	   (cons 12 (read-char p)))))
  (let ((p (if (null? iport)
	       (current-input-port)
	       (car iport))))
    (loop (peek-char p) p)))

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
	  ((begingroup? (car ls))
	   (in-group (cdr ls) '() 0))
	  ((or (texspaces? (car ls))
	       (par? (car ls)))
	   (out-group (cdr ls)))
	  (else
	   (values `(,(car ls)) (cdr ls))))) ; a token is also a group
 (out-group ls))

(define groupen 
  (put-specific-code -100 begingroup? get-tex-group))

;; utils
(define (tokenlist->string tls)
  (define (restore-command ts)
    (cond ((null? ts)
	   '())
	  ((not (textoken? (car ts)))
	   (cond
            ; group
	    ((= -100 (caar ts))
	     (cons (restore-command (cdar ts)) (restore-command (cdr ts))))
	    (else '())))
	  ((= -1 (cat (car ts)))
	   (cons 
	    (cond ((string=? "par" (cdar ts))
		   "\n\n")
		  ((null? (cdr ts))
		   (string-append "\\" (x->string (cdar ts))))
		  ((= 11 (cat (cadr ts)))
		   (string-append "\\" (x->string (cdar ts)) " "))
		  (else
		   (string-append "\\" (x->string (cdar ts)))))
	    (restore-command (cdr ts))))
	   ((and (= (cat (car ts)) 12)
		 (char-set-contains? #[$%&#_] (cdar ts)))
	    (cons (string-append "\\" (x->string (cdar ts)))
		  (restore-command (cdr ts))))
	   (else
	    (cons (cdar ts) (restore-command (cdr ts))))))
  (tree->string (restore-command tls)))

(define (string->tokenlist str)
  (with-input-from-string str
    (lambda ()
      (port-map values read-tex-token))))

(define (port->tokenlist p)
  (port->list read-tex-token p))


