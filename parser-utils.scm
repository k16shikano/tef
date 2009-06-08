;;;; utilities for parser

(define (string+char str . char)
  (string-append str (apply string char)))

;; [token] -> [token] -> [token] or #f
(define (match-head ls pattern) 
  (if (equal? (map (lambda (p l) l) pattern ls) pattern)
      (drop ls (length pattern))
      #f))

;; [token] -> [get token] [rest token]
(define (put-specific-code code finder getter)
  (lambda (ls . env)
    (if (null? ls)
	'()
	(if (finder (car ls))
	    (receive (group unseen)
		     (if (null? env)
			 (getter ls)
			 (getter ls (car env)))
		     (cons `(,code . ,group) unseen))
	    ls))))

;; are there any values whose first value is not null?
(define-syntax orvalues
  (syntax-rules ()
    ((_) (values '() '()))
    ((_ v1) v1)
    ((_ v1 v2 ...) 
     (if (null? (values-ref v1 0)) (orvalues v2 ...) v1))))

;; predicates

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

(define (charlbrace? token)
  (and (= 11 (cat token))
       (char=? #\{ (cdr token))))

(define (parameter? token)
  (= -15 (car token)))

(define-syntax defpred
  (syntax-rules ()
    ((_ name str)
     (define (name token)
       (and (< (car token) 0)
	    (string=? str (cdr token)))))))

(defpred def? "def")

(defpred edef? "edef")

(defpred expandafter? "expandafter")

(defpred par? "par")
