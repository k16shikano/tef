(use srfi-1)
(use srfi-13)
(use text.tree)

(define (string+char str . char)
  (string-append str (apply string char)))

;; special category code
;;   code = -1  : control sequence
;;   others     : TeX category code

;; string-port -> (code . token)
(define (read-tex-token . iport)
  (define (in-ctrl-seq c seq p)
    (let ((x (peek-char p)))
      (cond 
       ((char-set-contains? #[\s] x)
	(in-spaces (read-char p) seq p))
       ((char-set-contains? #[\W\d_] x) 
	(cond ((string-null? seq)
	       (cons -1 (string (read-char p))))
	      (else
	       (cons -1 seq))))
       (else 
	(in-ctrl-seq (read-char p) (string+char seq x) p)))))
  (define (in-spaces c seq p)
    (cond ((char-set-contains? #[\s] (peek-char p))
	   (in-spaces (read-char p) seq p))
	  (else
	   (cond ((string-null? seq)
		  (cons 10 #\space))
		 (else
		  (cons -1 seq))))))
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
	   (in-spaces (read-char p) "" p))
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

;; take the head group from a token list
(define (get-tex-group ls)
  (define (in-group ls body i)
    (cond ((endgroup? (car ls))
	   (if (= 0 i)
	       (values (reverse body) (cdr ls))
	       (in-group (cdr ls) (cons (car ls) body) (- i 1))))
	  ((begingroup? (car ls))
	   (in-group (cdr ls) (cons (car ls) body) (+ i 1)))
	  (else
	   (in-group (cdr ls) (cons (car ls) body) i))))
  (define (out-group ls)
    (cond ((null? ls)
	   '())
	  ((begingroup? (car ls))
	   (in-group (cdr ls) '() 0))
	  ((texspaces? (car ls))
	   (out-group (cdr ls)))
	  (else
	   (error <read-group-error> "the first token shoule have catcode 1"))))
  (out-group ls))


;; get a command and its parameters from a manuscript
(define (get-command-sequence cmd arity tokenlist)
  (let R ((before '())
	  (cmdseq '())
	  (after  '())
	  (next   tokenlist))
    (cond ((null? next)
	   (values (reverse before)
		   cmdseq
		   after))
	  ((< (cat (car next)) 0)
	   (if (string=? cmd (cdar next))
	       (values (reverse before)
		       (cons (car next) (values-ref (get-args arity (cdr next)) 0))
		       (values-ref (get-args arity (cdr next)) 1))
	       (R (cons (car next) before) cmdseq after (cdr next))))
	  (else
	   (R (cons (car next) before) cmdseq after (cdr next))))))

(define-condition-type <read-group-error> <error> #f)

(define (get-args n ls)
  (guard (exc
	  ((<read-group-error> exc) (error "missing argument" n))
	  ((<error> exc) (error "missing argument" n)))
	 (if (> n 0)
	     (receive (arg unseen)
		      (get-tex-group ls)
		      (receive (got rest)
			       (get-args (- n 1) unseen)
			       (values (cons arg got) rest)))
	     (values '() ls))))


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

(define (texspaces? t)
  (and (textoken? t)
       (or (=  5 (car t))
	   (= 10 (car t))
	   (=  9 (car t)))))

(define (tokenlist->string tls)
  (define (restore-command token)
    (if (> (cat token) 0)
	(cdr token)
	(string-append "\\" (cdr token))))
  (tree->string (map restore-command tls)))

(define (string->tokenlist str)
  (with-input-from-string str
    (lambda ()
      (port-map values read-tex-token))))


