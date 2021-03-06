;; halign := [type, dimen, body] 

(define-module align
  (use srfi-1)
  (use gauche.sequence)
  (use tokenlist-utils)
  (use register)
  (use group)
  (export-all)
)

(select-module align)

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
	   (let* ((body  (car (groupen rest)))
		  (rest  (cdr (groupen rest)))
		  (preamble (take-while (lambda (t) (not (cr? t))) body))
		  (rows  (drop-while (lambda (t) (not (cr? t))) body)))
	     (values `(,dimen ,preamble ,@rows)
		     rest))))

(define (expand-halign preamble ts)
  (map (lambda (row)
	 (sloting (make-template (group-sequence preamble :key amp?))
		  (remove-car amp? (group-sequence row :key amp?))))
       (remove-car cr? (group-sequence ts :key cr?))))

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

(provide "align")
