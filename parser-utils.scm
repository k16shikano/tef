;;;; utilities for parser

;; [token] -> [token] -> [token] or #f
(define (match-head ls pattern) 
  (if (equal? (map (lambda (p l) l) pattern ls) pattern)
      (drop ls (length pattern))
      #f))

;; [token] -> [get token] [rest token]
(define (put-specific-code code finder getter)
  (lambda (ls)
    (if (null? ls)
	'()
	(if (finder (car ls))
	    (receive (group unseen)
		     (getter ls)
		     (cons `(,code . ,group) unseen))
	    ls))))

;; are there any values whose first value is not null?
(define-syntax orvalues
  (syntax-rules ()
    ((_) (values '() '()))
    ((_ v1) v1)
    ((_ v1 v2 ...) 
     (if (null? (values-ref v1 0)) (orvalues v2 ...) v1))))

(define-syntax defpred
  (syntax-rules ()
    ((_ name str)
     (define (name token)
       (and (< (car token) 0)
	    (string=? str (cdr token)))))))

