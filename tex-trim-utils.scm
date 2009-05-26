(load "tex-modoki.scm")

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

