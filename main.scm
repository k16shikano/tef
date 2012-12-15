(use read)
(use show)
(use output-loop)
(use tokenlist-utils)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ((output "o|output=s") . args)
    (call-with-input-file (car args)
      (lambda (p)
        (display
         ((cond ((equal? output "text") tokenlist->string)
                ((equal? output "html") tokenlist->html)
                (else tokenlist->string))
          (build-para
            (string->tokenlist (port->string p)))))
        )
      ))
  0)
