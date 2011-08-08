(define-module read
  (use srfi-1)
  (use srfi-13)
  (use tokenlist-utils)
  (export-all)
)

(select-module read)

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
       ((and (char-set-contains? #[\W\d_] x)
	     (char-set-contains? #[^@] x))
	(cond ((string-null? seq)
	       (cons -1 (string (read-char p))))
	      (else
	       (cons -1 seq))))
       (else
	(in-ctrl-seq (read-char p) (string+char seq x) p)))))
  (define (in-spaces c seq S? N? p)
    (cond ((or (eof-object? (peek-char p))
	       (char-set-contains? #[^\s] (peek-char p)))
	   (cond ((not (string-null? seq))
		  (cons -1 seq))
		 (N? (cons -1 "par"))
		 (S? (cons 5 #\newline))
		 (else
		  (cons 10 #\space))))
	  (else
	   (in-spaces (read-char p) seq S? N? p))))
  (define (in-comment c p) 
    (cond ((char=? #\newline (peek-char p))
	   (read-char p)
	   (loop (peek-char p) p))
;	   (cons 5 #\newline))
	  (else
	   (in-comment (read-char p) p))))
  (define (in-newlines c N? p)
    (cond ((eof-object? (peek-char p))
	   (peek-char p))
	  ((char-set-contains? #[\x0d\x0a] (peek-char p))
	   (in-spaces (read-char p) "" #t #t p))
	  ((char-set-contains? #[\s] (peek-char p))
	   (in-spaces (read-char p) "" #t #f p))
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
	  ((char-set-contains? #[\x0d\x0a] c)
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

(define (string->tokenlist str)
  (with-input-from-string str
    (lambda ()
      (port-map values read-tex-token))))

(define (port->tokenlist p)
  (port->list read-tex-token p))

(provide "read")
