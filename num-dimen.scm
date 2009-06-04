(load "tex-modoki.scm")

;; this get-tex-dimen is not completed. 
;; it must support the spec. described in the TeX book chapter 24.
;; [token] -> [pattern] -> [matched pattern] [rest token]
(define (get-tex-dimen ts)
  (define unit-strings
    (map (compose string->tokenlist symbol->string)
	 '(pt pc in bp cm mm dd cc sp)))
  (define (for-pattern target us)
    (cond ((null? us)
	   (values #f target))
	  ((match-head target (car us))
	   => (lambda (rest) (values (car us) rest)))
	  (else
	   (for-pattern target (cdr us)))))
  (define (unit-rest unit target)
    (receive (num-unit rest)
	     (for-pattern target unit-strings)
	     (cond ((null? target)
		    (values '((-101 . "")) 
			    target))
		   (num-unit
		    (values `((-101 . ,(tokenlist->string (append (reverse unit) num-unit))))
			    rest))
		   ((and (= 12 (cat (car target)))
			 (char-set-contains? #[1-9] (cdar target)))
		    (unit-rest `(,(car target) . ,unit) (cdr target)))
		   ((texspaces? (car target))
		    (unit-rest unit (cdr target)))
		   (else
		    (error <read-parameter-error> "unknown unit" target)))))
  (unit-rest '() ts))

(define (get-tex-dimen-after str ts)
  (let ((dimen (match-head ts (string->tokenlist str))))
    (if dimen
	(get-tex-dimen dimen)
	(values '() ts))))

(define-syntax orvalues
  (syntax-rules ()
    ((_) (values '() '()))
    ((_ v1) v1)
    ((_ v1 v2 ...) 
     (if (null? (values-ref v1 0)) (orvalues v2 ...) v1))))



;;; test

(use gauche.test)

(test* "get-tex-dimen-after and orvalues"
       "136pt"
       (tokenlist->string
	(let ((ts (string->tokenlist "to136pt{...}")))
	  (orvalues (get-tex-dimen-after "to" ts)
		    (get-tex-dimen-after "spread" ts)))))
