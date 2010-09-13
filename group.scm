(load "tokenlist-utils.scm")
(load "codes.scm")

;; this gets the head group from a token list,
;; returning the group and the rest of the string in multivalues as tokenlists.
;; [token] -> ([token] and [token])
(define-condition-type <read-group-error> <error> #f)
(define (get-tex-group ls . env)
  (define localenv (if (null? env) (list (make-hash-table)) (car env)))
  (define (in-group ls body i)
    (cond ((null? ls)
	   (error <read-group-error> "unterminated tex group" 
		  (perror (reverse body))))
	  ((codename? (car ls))
	   (receive (num newcode rest)
		    (get-codename (cdr ls))
		    (begin (update-code! (integer->char num) newcode 
					    localenv)
			   (in-group rest body i))))
	  ((find-catcode (car ls) localenv)
	   => (lambda (v)
		(in-group (cons (cons v (cdar ls)) (cdr ls)) body i)))
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
