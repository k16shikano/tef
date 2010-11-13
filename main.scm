(use read)
(use show)
(use output-loop)
(use tokenlist-utils)

(define (main args)
  (call-with-input-file (cadr args)
    (lambda (p)
      (display
       (tokenlist->html
	(build-para
	 (string->tokenlist (port->string p)))))
      )
    )
  0)
