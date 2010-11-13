;; expanded box := [type, dimen, body] 

(define-module box
  (use srfi-1)
  (use tokenlist-utils)
  (use group)
  (use register)
  (use num-dimen)
  (export-all)
)

(select-module box)

(define (box? token)
  (and (textoken? token)
       (= (cat token) -1)
       (member (cdr token) 
	       ;; box and copy primitive are to be in register.scm
	       '("hbox" "vbox" "vtop" "vsplit" "lastbox" "vcenter"))))

(define (box-mode type)
  (cond
   ((box-type=? "hbox" type) 'H)
   ((box-type=? "vbox" type) 'V)))

(define (box-type=? typestr token)
  (and (= (cat token) -1)
       (string=? typestr (cdr token))))

(define (get-box-parameter ts env)
  (cond ((member (cdar ts) '("hbox" "vbox" "vtop"))
	 (receive (dimen body)
		  (orvalues (get-tex-dimen-after "to" (cdr ts) env)
			    (get-tex-dimen-after "spread" (cdr ts) env))
		  (values `(,(car ts) ,dimen
			    ,@(car (groupen body)))
			  (cdr (groupen body)))))
	((string=? (cdar ts) "vsplit")
	 (receive (oct rest)
		  ((get-tex-int-num env) (cdr ts))
		  (receive (dimen rest)
			   (get-tex-dimen-after "to" (cdr ts) env)
			   (values `(,(car ts) ,@oct ,@dimen '()) dimen rest))))
	((string=? (cdar ts) "lastbox")
	 (values '() rest))))

(define (expand-box box)
  (let ((type  (first box))
	(dimen (second box))
	(body  (cddr box)))
    (cond ((box-type=? "hbox" type)
	   `((H ,dimen ,@body)))
	  ((box-type=? "vbox" type)
	   `((V ,dimen ,@body)))
	  (else body))))

(provide "box")
