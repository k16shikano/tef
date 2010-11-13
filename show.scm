;; show tokenlist as a plain text string
;; using HTML4 and CSS2.1 to display mlist.

(define-module show
  (use srfi-1)
  (use text.html-lite)
  (use text.tree)
  (use tokenlist-utils)
  (export-all)
  )

(select-module show)

(define (tokenlist->string tls)
  (define (restore-command ts)
    (cond ((null? ts)
	   '())
          ; math charcode
	  ((and (null? (cdar ts)) (integer? (caar ts)) (> (caar ts) 0))
	   (cons #`"&#,(mathchar (list (caar ts)));"
		 (restore-command (cdr ts))))
	  ((not (textoken? (car ts)))
	   (cond
	    ((null? (caar ts)) 
	     (restore-command (cdr ts)))
	    ; box
	    ((symbol? (caar ts))
	     (cond ((eq? 'V (caar ts))
		    (list (restore-command (cddar ts))
			  (list "\n\n")
			  (restore-command (cdr ts))))
		   ((eq? 'H (caar ts))    ; vbox
		    (cons (list #\[ (restore-command (cddar ts)) #\])
			  (restore-command (cdr ts))))
		   ((or (eq? 'M (caar ts)) (eq? 'MD (caar ts))) ; math
		    (cons (print-math (cdar ts) (caar ts))
			  (restore-command (cdr ts))))
		   ((eq? 'alignment (caar ts))           ; alignment
		    (cons (print-align (cdar ts))
			  (restore-command (cdr ts))))
		   (else
		    (error "Cannot display" (car ts)))))
	    ((list? (car ts)) ; group
	     (cons (restore-command (car ts)) (restore-command (cdr ts))))
	    (else '())))
	  ((= -1 (cat (car ts)))
	   (cons 
	    (cond ((string=? "par" (cdar ts))
		   "\n\n")
;		   "<br/>")
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

(define (mathchar mathtoken)
  (if (integer? (car mathtoken))
      (remainder (car mathtoken) #x10000)
      mathtoken))

(define (print-math ts limit)
  (define (padding n)
    (html:span :style #`"width:,(x->string n)%;" " "))

  (define (print-nolimit-supsub t)
    (html:span 
     :class "noad"
     (html:div 
      :class "sub" 
      (print-math (third t) limit))
     (html:div 
      :class "sup"
      (print-math (fourth t) limit))))

  (define (print-nolimit t . class)
    (let1 class (if (null? class) 
		    (if (and (textoken? (second t)) (= 12 (cat (second t))))
			"normal" "italic")
		    (car class))
	  (html:span 
	   :class class 
	   (cond ((null? (second t)) "")
		 ((and (pair? (car (second t))) (number? (caar (second t))))
		  (tokenlist->string (second t)))
		 (else (tokenlist->string (list (second t)))))
	   (print-nolimit-supsub t))))

  (define (print-nolimit-op t)
    (html:span 
     :class "op" 
      (tokenlist->string (list (second t)))
      (print-nolimit-supsub t)))

  (define (print-limit-op t)
    (html:span 
     :class "op"
     (html:div :class "sup"
	       (print-math (third t) limit))
     (html:div :class "nuc"
	       (tokenlist->string (list (second t))))
     (html:div :class "sub"
	       (print-math (fourth t) limit))))

  (define (print-binrel t)
    (html:span :class "binrel"
	       (tokenlist->string (list (second t)))))

  (define (print-mathtoken t class)
    (html:span :class class
	       (tokenlist->string t)))

  (define (print-inner t class)
    (html:span :class class
     (print-math (list (list (second t))) limit)
     (print-nolimit-supsub t)))

  (define (print-code code class)
    (html:span 
     :class class
     (print-math `(((,code))) limit)))

  (cond ((null? ts)
	 (html:span :class "null" ""))
	((null? (cdar ts))
	 (tokenlist->string (car ts)))
	((eq? 'Nil (caar ts))
	 (cons
	  (print-nolimit (car ts) "")
	  (print-math (cdr ts) limit)))
	((eq? 'Ord (caar ts))
	 (cons
	  (print-nolimit (car ts))
	  (print-math (cdr ts) limit)))
	((eq? 'Op (caar ts))
	 (list
	  (if (eq? 'M limit)
	      (print-limit-op (car ts))
	      (print-nolimit-op (car ts)))
	  (padding 20)
	  (print-math (cdr ts) limit)))
	((eq? 'Bin (caar ts))
	 (list
	  (padding 100)
	  (print-binrel (car ts))
	  (padding 100)
	  (print-math (cdr ts) limit)))
	((eq? 'Rel (caar ts))
	 (list
	  (padding 100)
	  (print-binrel (car ts))
	  (padding 100)
	  (print-math (cdr ts) limit)))
	((eq? 'Open (caar ts))
	 (list
	  (print-binrel (car ts))
	  (print-math (cdr ts) limit)))
	((eq? 'Close (caar ts))
	 (list
	  (print-binrel (car ts))
	  (print-math (cdr ts) limit)))
	((eq? 'Punct (caar ts))
	 (list
	  (print-binrel (car ts))
	  (padding 150)
	  (print-math (cdr ts) limit)))
	((eq? 'Box (caar ts))
	 (cons
	  (print-nolimit (car ts) "box")
	  (print-math (cdr ts) limit)))
	((eq? 'Inner (caar ts))
	 (cons
	  (print-inner (car ts) "inner")
	  (print-math (cdr ts) limit)))
	((eq? 'Rad (caar ts))
	 (list
	  (padding 20)
	  (print-code (fifth (car ts)) "delim")
	  (print-inner (car ts) "rad")
	  (padding 20)
	  (print-math (cdr ts) limit)))
	((eq? 'Fraction (caar ts))
	 (let1 border (format "~apx" (if (pair? (second (car ts)))
					 (cdr (second (car ts)))
					 1))
	       (append
		(list
		 (print-mathtoken (fifth (car ts)) "fracdelim")
		 (padding 10)
		 (html:span :class "fraction"
		      (html:div :style
				#`"border-bottom:,border solid;"
				(print-math (third (car ts)) limit))
		      (html:div (print-math (fourth (car ts)) limit)))
		 (padding 10)
		 (print-mathtoken (sixth (car ts)) "fracdelim")
		 (print-math (cdr ts) limit)))))
	((textoken? (car ts))
	 (if (= 10 (cat (car ts))) (cdr ts) (cons (cdar ts) (cdr ts))))
	(else (cons (cdar ts) (cdr ts)))))

(define (css)
  (html:style 
   (string-join 
    (list 
    "<!--"
    "body {font-family:serif}"
    "span.noad {display:inline-block;text-align:left;vertical-align:middle;font-size:60%}"
    "span.noad div.sub {position:relative;bottom:0.4em;}"
    "span.noad div.sup {position:relative;top:0.4em;}"
    "span.italic {font-style:italic}" 
    "span.normal {font-style:normal}" 
    "span.op {display:inline-block; text-align:center; vertical-align:middle;font-size:200%}"
    "span.op span.noad div.sub {font-size:50%;position:relative;bottom:1em}"
    "span.op span.noad div.sup {font-size:50%;position:relative;top:1em}"
    "span.op div.sup {font-size:30%;vertical-align:bottom}"
    "span.op div.sub {font-size:30%;vertical-align:bottom}"
    "span.op div.nuc {font-size:100%;line-height:80%}"
    "span.null {display:inline-block;width:0px;line-height:0px}"
    "span.binrel {display:inline-block;text-align:center;vertical-align:middle;font-size:110%;}"
    "span.box {font-style:normal; vertical-align:middle}" 
    "span.delim {font-size:130%;vertical-align:middle}"
    "span.rad {display:inline-block;border-top:1pt solid;vertical-align:middle}" 
    "span.fraction {display:inline-block;text-align:center;vertical-align:middle;font-size:80%;}"
    "span.fracdelim {font-size:150%; vertical-align:middle}"
    "-->"
    ) "\n")))

(define (tokenlist->html ts)
  (define (tokenlist->body ts)
    (fold-right loop '() ts))
  (define (loop t r)
    (cond
     ; box
     ((symbol? (car t))
      (cond ((eq? 'V (car t))
	     (cons (html:p (tokenlist->body (cdr t))) r))))
     (else
      (cons (cdr t) r))))
  (tree->string 
   (html:html 
    (css) 
    (html:body (tokenlist->body ts)))))

(define (print-align align)
  (html:table
   (map (lambda (row)
	  (html:tr 
	   (map (lambda (col)
		  (html:td (tokenlist->string col)))
		row)))
	align)))

(define (perror ls)
  (if (<= (length ls) 20)
      (tokenlist->string ls)
      (string-append (tokenlist->string (take ls 20)) "...")))

(provide "show")
