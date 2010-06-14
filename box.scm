(load "tex-modoki.scm")
(load "parser-utils.scm")
(load "num-dimen.scm")
(load "def-macro.scm")

(define (box? token)
  (and (< (cat token) 0)
       (member (cdr token) 
	       '("hbox" "vbox" "vtop" "box" "copy" "vsplit" "lastbox" "vcenter"))))

(define (box-type=? typestr token)
  (and (< (cat token) 0)
       (string=? typestr (cdr token))))

(define (get-box-parameter ts env)
  (cond ((member (cdar ts) '("hbox" "vbox" "vtop"))
	 (receive (dimen body)
		  (orvalues (get-tex-dimen-after "to" (driver-loop (cdr ts) env)) 
			    (get-tex-dimen-after "spread" (driver-loop (cdr ts) env)))
		  (values `(,(car ts) ,@dimen ,@(cdar body)) (cdr body))))
	((member (cdar ts) '("box" "copy"))
	 (receive (oct rest)
		  (get-tex-dimen (driver-loop (cdr ts) env))
		  (values `(,(car ts) ,@oct) rest)))
	((string=? (cdar ts) "vsplit")
	 (receive (oct rest)
		  (get-tex-dimen (driver-loop (cdr ts) env))
		  (receive (dimen rest)
			   (get-tex-dimen-after "to" (driver-loop (cdr ts) env))
			   (values `(,(car ts) ,@oct ,@dimen) rest))))	
	((string=? (cdar ts) "lastbox")
	 (values '() rest))))

(define boxen
  (put-specific-code -102 box? get-box-parameter))

(define (process-box box env) 
  (if (not (= -102 (car box))) (error "here expects a tex box.")
      (receive (type rest)
	       (values (cadr box) (cddr box))
	       (receive (spec body)
			(if (= -101 (caar rest)) ; has spec as dimen?
			    (values (token->dimen (car rest)) (cdr rest))
			    (values '() rest))
			(let1 ex-body (driver-loop body env) ; define representation of boxes.
			      (display spec)
			      (cond ((box-type=? "hbox" type)
				     `((11 . #\[) ,@ex-body (11 . #\]))) ; just for tt representation
				    ((box-type=? "vbox" type)
				     `((11 . #\|) ,@ex-body (11 . #\|))) ; just for tt representation
				    (else ex-body)))))))




