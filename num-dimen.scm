(load "tex-modoki.scm")
(load "parser-utils.scm")
(load "def-macro.scm")

(define-condition-type <read-num-error> <error> #f)

;; this get-tex-dimen is not completed. 
;; it must support the spec. described in the TeX book chapter 24.
;; [token] -> [pattern] -> [matched pattern] and [rest token]
(define (get-tex-dimen ts)
  (define unit-strings
    (map (compose string->tokenlist symbol->string)
	 '(pt pc in bp cm mm dd cc sp em)))
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
			 (or (char-set-contains? #[0-9] (cdar target))
			     (char-set-contains? #[.] (cdar target))))
		    (unit-rest `(,(car target) . ,unit) (cdr target)))
		   ((= 6 (cat (car target)))
		    (receive (param rest)
			     (parameter-token target)
			     (values param rest)))
		   ((texspaces? (car target))
		    (unit-rest unit (cdr target)))
		   (else
		    (error <read-num-error> "unsupported number" (tokenlist->string target))))))
  (unit-rest '() ts))

(define (get-tex-dimen-after str ts)
  (let ((dimen (match-head ts (string->tokenlist str))))
    (if dimen
	(get-tex-dimen dimen)
	(values '() ts))))
