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
  (cond ((null? ts)
	 (html:span :style "display:inline-block;width:0px" "&nbsp;"))
	((null? (cdar ts))
	 (cons (tokenlist->string (car ts))
	       (print-math (cdr ts))))
	((eq? 'Nil (caar ts))
	 (cons
	  (html:i "" (sup ts "60%") (sub ts "60%"))
	  (print-math (cdr ts))))
	((eq? 'Ord (caar ts))
	 (let1 style (if (= 11 (cat (second (car ts)))) "italic" "normal")
	       (cons
		(html:span :style #`"font-style:,style"
		  (print-math (list (second (car ts))))
		  (html:span :style 
			     "display:inline-block; text-align:center;\
			      vertical-align:middle; font-size:60%"
			     (html:div (print-math (third (car ts))))
			     (html:div (print-math (fourth (car ts))))))
		(print-math (cdr ts)))))
	((eq? 'Op (caar ts))
	 (list
	  (html:span :style
		     "display:inline-block; text-align:center;\
		      vertical-align:middle;"
		     (html:div :style "font-size:60%;vertical-align:bottom"
				(print-math (third (car ts))))
		     (html:div :style "font-size:200%"
				(tokenlist->string (second (car ts))))
		     (html:div :style "font-size:60%;vertical-align:top"
				(print-math (fourth (car ts)))))
	  (html:span :style "width:20%" "&nbsp;")
	  (print-math (cdr ts))))
	((eq? 'Bin (caar ts))
	 (list
	  (html:span :style "width:20%" "&nbsp;")
	  (html:span :style
		     "display:inline-block; text-align:center;\
		      vertical-align:middle; font-size:110%;"
		     (cdr (second (car ts))))
	  (html:span :style "width:20%" "&nbsp;")
	  (print-math (cdr ts))))
	((eq? 'Box (caar ts))
	 (cons 
	  (html:span :style "font-style:normal; vertical-align:middle" 
		     (tokenlist->string (second (car ts)))
		     (sup ts "60%") (sub ts "60%"))
	  (print-math (cdr ts))))
	((eq? 'Inner (caar ts))
	 (cons
	  (html:span 
	   (print-math (list (list (second (car ts)))))
	   (html:span :style 
		      "display:inline-block;w text-align:center;\
		       vertical-align:middle; font-size:60%"
		      (html:div (print-math (third (car ts))))
		      (html:div (print-math (fourth (car ts))))))
	  (print-math (cdr ts))))
	((eq? 'Rad (caar ts))
	 (list
	  (html:span :style "width:20%" "&nbsp;")
	  (html:span :style "font-size:130%"
		     (print-math (list (list (fifth (car ts))))))
	  (html:span :style #`"border-top:1pt solid"
		     (print-math (list (list (second (car ts)))))
		     (html:span :style 
				"display:inline-block;text-align:center;\
			         vertical-align:middle;font-size:60%"
				(html:div (print-math (third (car ts))))
				(html:div (print-math (fourth (car ts))))))
	  (html:span :style "width:20%" "&nbsp;")
	  (print-math (cdr ts))))
	((eq? 'Fraction (caar ts))
	 (let1 border (format "~apx" (if (pair? (second (car ts)))
					 (cdr (second (car ts)))
					 1))
	 (append
	  (list
	   (html:span :style "width:20%" "&nbsp;")
	   (html:span :style "font-size:150%; vertical-align:middle"
		      (tokenlist->string (fifth (car ts))))
	   (html:span :style 
		      "display:inline-block; text-align:center;\
		       vertical-align:middle; font-size:80%"		       
		      (html:div :style
				#`"border-bottom:,border solid; margin:10%"
				(print-math (third (car ts))))
		      (html:div (print-math (fourth (car ts)))))
	   (html:span :style "font-size:150%; vertical-align:middle"
		      (tokenlist->string (sixth (car ts))))
	   (html:span :style "width:20%" "&nbsp;"))
	  (print-math (cdr ts)))))
	((textoken? (car ts))
	 (if (= 10 (cat (car ts))) (cdr ts) (cons (cdar ts) (cdr ts))))
	(else (cons (cdar ts) (cdr ts)))))
