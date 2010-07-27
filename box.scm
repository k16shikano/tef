(load "tex-modoki.scm")
(load "parser-utils.scm")
(load "num-dimen.scm")

(define (box? token)
  (and (= (cat token) -1)
       (member (cdr token) 
	       '("hbox" "vbox" "vtop" "box" "copy" "vsplit" "lastbox" "vcenter"))))

(define (box-type=? typestr token)
  (and (< (cat token) 0)
       (string=? typestr (cdr token))))

(define (get-box-parameter ts)
  (cond ((member (cdar ts) '("hbox" "vbox" "vtop"))
	 (receive (dimen body)
		  (orvalues (get-tex-dimen-after "to" (cdr ts))
			    (get-tex-dimen-after "spread" (cdr ts)))
		  (values `(,(car ts) ,@dimen ,(car (groupen body)))
			  (cdr (groupen body)))))
	((member (cdar ts) '("box" "copy"))
	 (receive (oct rest)
		  (tex-int-num (cdr ts))
		  (values `(,(car ts) ,@oct) rest)))
	((string=? (cdar ts) "vsplit")
	 (receive (oct rest)
		  (tex-int-num (cdr ts))
		  (receive (dimen rest)
			   (get-tex-dimen-after "to" (cdr ts))
			   (values `(,(car ts) ,@oct ,@dimen) rest))))	
	((string=? (cdar ts) "lastbox")
	 (values '() rest))))

(define boxen
  (put-specific-code -102 box? get-box-parameter))

(define (process-box box)
  (if (not (= -102 (car box))) (error "here expects a tex box.")
      (receive (type rest)
	       (values (cadr box) (cddr box))
	       (receive (spec body)
			(if (= -101 (caar rest)) ; has spec as dimen?
			    (values (token->dimen (car rest)) (cdr rest))
			    (values '() (cdr rest)))
			(cond ((box-type=? "hbox" type)
			       `((-102 (0 ,@body))))
			      ((box-type=? "vbox" type)
			       `((-102 (1 ,@body))))
			      (else body))))))




