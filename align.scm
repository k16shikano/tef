;; halign := [-103, type, dimen, body] 

(use gauche.sequence)
(load "tokenlist-utils.scm")
(load "num-dimen.scm")
(load "group.scm")

(defpred halign? "halign")
(defpred cr? "cr")

(define (amp? token)
  (and (textoken? token) (= 4 (cat token))))
(define (align-slot? token)
  (and (textoken? token) (= 6 (cat token))))

(define (get-halign ts env)
  (receive (dimen rest)
	   (orvalues (get-tex-dimen-after "to" (cdr ts) env)
		     (get-tex-dimen-after "spread" (cdr ts) env))
	   (let* ((body  (cdar (groupen rest)))
		  (rest  (cdr (groupen rest)))
		  (preamble (take-while (lambda (t) (not (cr? t))) body))
		  (rows  (drop-while (lambda (t) (not (cr? t))) body)))
	     (values `(,dimen ,preamble ,@rows)
		     rest))))

(define haligning
  (put-specific-code -103 halign? get-halign))

(define (expand-halign preamble ts)
  (list 
   (cons -103
	 (map (lambda (row)
		(sloting (make-template (group-sequence preamble :key amp?))
			 (remove-car amp? (group-sequence row :key amp?))))
	      (remove-car cr? (group-sequence ts :key cr?))))))

(define (remove-car pred ts)
  (remove (lambda (x) (pred (car x))) ts))

(define (sloting preamble ts)
  (map
   (lambda (spec content)
     (append-map (lambda (t)
	    (if (align-slot? t) content (list t)))
	  spec))
   preamble ts))

(define (make-template preamble)
  (if (amp? (caar preamble))
      (apply circular-list (make-template (cdr preamble)))
      (let R ((preamble preamble))
	(cond ((null? preamble) '())
	      ((amp? (caar preamble))
	       (if (null? (cdar preamble))
		   (R (cdr preamble))
		   (apply circular-list (R (cdr preamble)))))
	      (else (cons (car preamble) (R (cdr preamble))))))))

(define (align-map proc align)
  (map (lambda (row)
	 (map (lambda (col) (proc col)) row))
       align))

