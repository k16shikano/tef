;; box := [-102, type, dimen, body] 

(load "tokenlist-utils.scm")
(load "num-dimen.scm")
(load "group.scm")
(load "register.scm")

(define (box? token)
  (and (textoken? token)
       (= (cat token) -1)
       (member (cdr token) 
	       ;; box and copy primitive are to be in register.scm
	       '("hbox" "vbox" "vtop" "vsplit" "lastbox" "vcenter"))))

(define (box-type=? typestr token)
  (and (< (cat token) 0)
       (string=? typestr (cdr token))))

(define (get-box-parameter ts env)
  (cond ((member (cdar ts) '("hbox" "vbox" "vtop"))
	 (receive (dimen body)
		  (orvalues (get-tex-dimen-after "to" (cdr ts) env)
			    (get-tex-dimen-after "spread" (cdr ts) env))
		  (values `(,(car ts) ,(if dimen dimen #f) 
			       ,@(cdar (groupen body)))
			  (cdr (groupen body)))))
	((string=? (cdar ts) "vsplit")
	 (receive (oct rest)
		  ((get-tex-int-num env) (cdr ts))
		  (receive (dimen rest)
			   (get-tex-dimen-after "to" (cdr ts) env)
			   (values `(,(car ts) ,@oct ,@dimen '()) rest))))
	((string=? (cdar ts) "lastbox")
	 (values '() rest))))

(define boxen
  (put-specific-code -102 box? get-box-parameter))

(define (expand-box box)
  (if (not (= -102 (car box))) (error "here expects a tex box.")
      (receive (type rest)
	       (values (cadr box) (cddr box))
	       (let1 body (cdr rest)
		     (cond ((box-type=? "hbox" type)
			    `((-102 (0 ,@body))))
			   ((box-type=? "vbox" type)
			    `((-102 (1 ,@body))))
			   (else body))))))
