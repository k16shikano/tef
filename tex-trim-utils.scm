(load "read.scm")
(load "group.scm")

;;;; utils for triming tex manusctipts.
;;;; this is not the part of the tex-modoki in itself.

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
	  ((<read-group-error> exc) (error "missing argument" n))
	  ((<error> exc) (error "missing argument" n)))
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
(define (get-command-sequence cmd arity tokenlist)
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
		    (R (append (reverse comment) before)
		       cmdseq after rest)))
	  ((< (cat (car next)) 0)
	   (if (string=? cmd (cdar next))
	       (values (reverse before)
		       (cons (car next) (values-ref (get-args arity (cdr next)) 0))
		       (values-ref (get-args arity (cdr next)) 1))
	       (R (cons (car next) before) cmdseq after (cdr next))))
	  (else
	   (R (cons (car next) before) cmdseq after (cdr next))))))

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
	  ((newline? (car rest))
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

;; ([token] -> [token]) -> String -> Integer -> [token]
(define (replace-texcmd-all transformer texcmd arity tokens)
  (receive (s a)
	   (replace-texcmd1 transformer texcmd arity tokens)
	   (append s
		   (if (null? a)
		       '()
		       (replace-texcmd-all transformer texcmd arity a)))))

;; ([token] -> [token]) -> String -> Integer -> ([token] and [token])
(define (replace-texcmd1 transformer texcmd arity tokens)
  (receive (b c a)
	      (get-command-sequence texcmd arity tokens)
	      (values 
	       (append b
		       (if (null? c)
			   '()
			   (transformer c)))
	       (if (null? a)
		   '()
		   a))))

;;;; macro to make a tex command trimer
;; String -> Integer -> [string or token of replaced string] -> ([token] -> [token])
(define-macro (cmd-trimer cmd arity . parts)
  `(lambda (tokens)
     (replace-texcmd-all
      (lambda (cmd-seq)
	(let ((@rg (lambda (n) (list-ref cmd-seq n))))
	  (combine ,@parts)))
      ,cmd ,arity tokens)))

;; [token or string]
(define (combine . parts)
  (define (tokenize part)
    (cond ((string? part) (string->tokenlist part))
          (else part)))
  (append-map  tokenize parts))

;; samples usage
;; to be added ...

