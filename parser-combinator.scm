;;;; A simple parser combinator for tex-modoki.
;;;; parser := [token] -> [matched token] and [rest token]

(load "parser-utils.scm")
(load "tex-modoki.scm")

(define-condition-type <parser-error> <error> #f)

(define (make-token-parser type-name cat-code pred)
  (lambda (ts)
    (cond ((null? ts)
	   (error <parser-error> "there's no tokens"))
	  ((or (eq? 'any cat-code)
	       (and (textoken? (car ts))
		    (= (cat (car ts)) cat-code)
		    (pred (cdar ts))))
	   (values (list (car ts)) (cdr ts)))
	  (else
	   (error <parser-error> (format "it's not a ~a, anyway" type-name))))))

(define-syntax parser-or
  (syntax-rules (error)
    ((_ (error m1 ...))
     (lambda (ts) (error <parser-error> m1 ... (perror ts))))
    ((_)
     (lambda (ts) (values '() ts)))
    ((_ p)
     (lambda (ts) (p ts)))
    ((_ p1 p2 ...)
     (lambda (ts)
       (guard (e
	       ((<parser-error> e) ((parser-or p2 ...) ts)))
	      (p1 ts))))))

(define-syntax parser-cont
  (syntax-rules ()
    ((_) (lambda (ts) (values '() ts)))
    ((_ p)
     (lambda (ts) (p ts)))
    ((_ p1 p2 ...)
     (lambda (ts)
       (receive (match-p1 rest-p1)
		(p1 ts)
		(receive (match-p2 rest-p2)
			 ((parser-cont p2 ...) rest-p1)
			 (values `(,@match-p1 ,@match-p2) rest-p2)))))))

(define (parser-many p)
  (lambda (ts)
    (let R ((match '())
	    (rest  ts))
      (guard (e
	      ((<parser-error> e) (values match rest)))
	     (receive (m r)
		      (p rest)
		      (R (append match m) r))))))

(define (parser-many1 p)
  (parser-cont p (parser-many p)))

(define tex-space1
  (make-token-parser
   "space"
   10
   char?))

(define tex-null
  (lambda (ts)
    (values '() ts)))

(define tex-spaces
  (parser-many tex-space1))

(define (make-char-parser catcode char desc)
  (make-token-parser
   (if (null? desc) 
       (string char)
       (car desc))
   catcode
   (cut char=? char <>)))

(define (tex-other-char char . desc)
  (make-char-parser 12 char desc))

(define (tex-alphabet char . desc)
  (make-char-parser 11 char desc))

(define (x->digit x)
  (cond ((integer? x)
	 (integer->digit x))
	(else x)))

(define-syntax ordigits
  (syntax-rules ()
    ((_ desc d) (tex-other-char (x->digit d) desc))
    ((_ desc d1 ...)
     (parser-or (tex-other-char (x->digit d1) desc) ...))))

(define-syntax oralpha
  (syntax-rules ()
    ((_ desc d) (tex-alphabet d desc))
    ((_ desc d1 ...)
     (parser-or (tex-alphabet d1 desc) ...))))

(define (make-string-parser str)
  (lambda (ts)
    (let ((str (string->tokenlist (x->string str))))
      (let ((match (match-head ts str)))
	(if match
	    (values str match)
	    (error <parser-error> "it's not" (perror str)))))))

(define-syntax orstring
  (syntax-rules (error)
    ((_ (error m1 ...))
     (lambda (ts) (error <parser-error> m1 ... (perror ts))))
    ((_ d) (lambda (ts) ((make-string-parser d) ts)))
    ((_ d1 d2 ...)
     (lambda (ts)
       (guard (e
	       ((<parser-error> e) ((orstring d2 ...) ts)))
	      ((make-string-parser d1) ts))))))

;; for test
(define-macro (parser-test* desc match-expect rest-expect parser tokens)
  `(begin
     (display #\newline)
     (test* (format "<~a>: match" ,desc) ,match-expect
	    (tokenlist->string 
	     (values-ref (,parser (string->tokenlist ,tokens)) 0)))
     (test* (format "<~a>:  rest" ,desc) ,rest-expect
	    (tokenlist->string
	     (values-ref (,parser (string->tokenlist ,tokens)) 1)))))
