;;;; Evaluator for TeX macros.
;;;; env is a list of hash-tables having macro definitions. 
;;;; its key is the name of macros in symbol, 
;;;; and its value is [[parameter token] . [body token]].

(load "tex-modoki.scm")
(load "def-macro.scm")
(load "num-dimen.scm")
(load "box.scm")
(load "if.scm")

(define global-env
  (list (make-hash-table)))

;; [token] -> env -> [expanded token]
(define (output ts)
  (let1 ts (expand-macro ts global-env)
	(cond ((null? ts)
	       '())
	      ((not (textoken? (car ts))) 
	       ts)
	      ((< (cat (car ts)) 0)
	       (process-primitives ts))
	      ((= (cat (car ts)) 5) ; skip linebreaks
	       (output (cdr ts)))
	      (else
	       (cons (car ts) (output (cdr ts)))))))

;; primitive processors
(define (process-primitives ts)
  (cond ((box? (car ts))
	 (let1 boxed (boxen ts)
	       (append (process-box (car boxed)) (cdr boxed))))
	((if? (car ts))	
	 (let1 ifstate (ifen ts)
	       (append (process-if (car ifstate)) (cdr ifstate))))
	(else
	 (cons (car ts) (output (cdr ts))))))

