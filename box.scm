(load "tex-modoki.scm")
(load "num-dimen.scm")

(define (box? token)
  (and (< (cat token) 0)
       (member (cdr token) 
	       '("hbox" "vbox" "vtop" "box" "copy" "vsplit" "lastbox" "vcenter"))))

(define (get-box-parameter ts)
  (cond ((member (cdar ts) '("hbox" "vbox" "vtop"))
	 (receive (dimen rest)
		  (orvalues (get-tex-dimen-after "to" (cdr ts))
			    (get-tex-dimen-after "spread" (cdr ts)))
		  (receive (body rest)
			   (get-tex-group rest)
			   (values `(,(car ts) ,@dimen ,@body) rest))))
	((member (cdar ts) '("box" "copy"))
	 (receive (oct rest)
		  (get-oct (cdr ts))
		  (values `(,(car ts) ,@oct) rest)))
	((string=? (cdar ts) "vsplit")
	 (receive (oct rest)
		  (get-oct (cdr ts))
		  (receive (dimen rest)
			   (get-tex-dimen-after "to" (cdr ts))
			   (values `(,(car ts) ,@oct ,@dimen) rest))))	
	((string=? (cdar ts) "lastbox")
	 (values '() rest))))

(define boxen
  (put-specific-code -102 box? get-box-parameter))


(boxen (string->tokenlist "\\box136pt{...}rrr"))
