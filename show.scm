;; show tokenlist as a plain text string
;; using HTML4 and CSS2.1 to display mlist.

(use text.html-lite)
(load "tokenlist-utils.scm")

(define (tokenlist->string tls)
  (define (restore-command ts)
    (cond ((null? ts)
	   '())
          ; math charcode
	  ((integer? (car ts))
	   (cons #`"&#,(mathchar (list (car ts)));" (restore-command (cdr ts))))
	  ((not (textoken? (car ts)))
	   (cond
            ; group
	    ((= -100 (caar ts))
	     (cons (restore-command (cdar ts)) (restore-command (cdr ts))))
	    ; box
	    ((= -102 (caar ts))
	     (cond ((= 0 (caadar ts)) ; vbox
		    (cons (list #\[ (restore-command (cdadar ts)) #\])
			  (restore-command (cdr ts))))
		   ((= 1 (caadar ts)) ; hbox
		    (cons (list #\| (restore-command (cdadar ts)) #\|)
			  (restore-command (cdr ts))))))
	    ; math
	    ((= 100 (caar ts))
	     (cons (print-math (cdar ts))
		   (restore-command (cdr ts))))
	    (else '())))
	  ((= -1 (cat (car ts)))
	   (cons 
	    (cond ((string=? "par" (cdar ts))
		   "\n\n")
		  ((null? (cdr ts))
		   (string-append "\\" (x->string (cdar ts))))
		  ((= 11 (cat (cadr ts)))
		   (string-append "\\" (x->string (cdar ts)) " "))
		  (else
		   (string-append "\\" (x->string (cdar ts)))))
	    (restore-command (cdr ts))))
	   ((and (= (cat (car ts)) 12)
		 (char-set-contains? #[$%&#_] (cdar ts)))
	    (cons (string-append "\\" (x->string (cdar ts)))
		  (restore-command (cdr ts))))	    
	   (else
	    (cons (cdar ts) (restore-command (cdr ts))))))
  (tree->string (restore-command tls)))


(define (print-math ts)
  (define (sup ts r)
    (html:sup :style #`"font-size: ,|r|;\
	              vertical-align:super"
	      (print-math (third (car ts)))))
  (define (sub ts r)
    (html:sub :style #`"font-size: ,|r|;\
	              vertical-align:sub"
	      (print-math (fourth (car ts)))))

  (cond ((null? ts)
	 '())
	((null? (cdar ts))
	 (html:span :style "font-style:normal" (tokenlist->string (car ts))))
	((eq? 'Nil (caar ts))
	 (cons
	  (html:i "" (sup ts "60%") (sub ts "60%"))
	  (print-math (cdr ts))))
	((eq? 'Ord (caar ts))
	 (cons
	  (html:i (cdr (second (car ts)))
		  (sup ts "60%") (sub ts "60%"))
	  (print-math (cdr ts))))
	((eq? 'Op (caar ts))
	 (cons
	  (html:span :style "font-size: 150%" (tokenlist->string (second (car ts)))
		     (sup ts "40%") (sub ts "40%"))
	  (print-math (cdr ts))))
	((eq? 'Box (caar ts))
	 (cons 
	  (html:span :style "font-style:normal" 
		     (tokenlist->string (second (car ts)))
		     (sup ts "60%") (sub ts "60%"))
	  (print-math (cdr ts))))
	(else (cdr ts))))
