(load "tex-modoki.scm")
(load "parser-utils.scm")
(load "num-dimen.scm")

(define (box? token)
  (and (< (cat token) 0)
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
		  (values `(,(car ts) ,@dimen ,@(cdar body)) (cdr body))))
	((member (cdar ts) '("box" "copy"))
	 (receive (oct rest)
		  (get-tex-dimen (cdr ts))
		  (values `(,(car ts) ,@oct) rest)))
	((string=? (cdar ts) "vsplit")
	 (receive (oct rest)
		  (get-tex-dimen (cdr ts))
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
			    (values '() rest))
			(cond ((box-type=? "hbox" type)
			       `((11 . #\[) ,@body (11 . #\]))) ; just for tt representation
			      ((box-type=? "vbox" type)
			       `((11 . #\|) ,@body (11 . #\|))) ; just for tt representation
			      (else body))))))




