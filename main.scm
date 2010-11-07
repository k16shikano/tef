(load "read")
(load "show")
(load "output-loop")
(load "tokenlist-utils")

(define (main args)
  (call-with-input-file (cadr args)
    (lambda (p)
      (display
       (tokenlist->html
	(output 
	 (string->tokenlist (port->string p)))))))
  0)

