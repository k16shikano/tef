(load "parser-utils.scm")
(load "parser-combinator.scm")
(load "num-dimen.scm")

(define (if? token)
  (and (< (cat token) 0)
       (member (cdr token) 
	       '("if" "ifx" "ifcat"
		 "ifnum" "ifdim" "ifodd" 
		 "ifvmode" "ifhmode" "ifmmode" "ifinner"
		 "ifvoid" "ifhbox" "ifvbox"
		 "ifeof" "iftrue" "iffalse"
		 "ifcase"))))

(defpred fi? "fi")
(defpred else? "else")

(define (if-type=? typestr token)
  (and (< (cat token) 0)
       (string=? typestr (cdr token))))

(define ifnum-param
  (parser-cont tex-int-num  tex-spaces
	       (orothers "" #\< #\= #\>) tex-spaces
	       tex-int-num tex-spaces))

(define-condition-type <read-if-error> <error> #f)

(define (get-if-statement ts)
  (define (in ts type)
    (cond ((null? ts)
	   (error <read-if-error> "unterminated if statement" 
		  (perror (reverse ts))))
	  (else
	   (receive (test-param rest) 
		    (condition ts type)
		    (receive (state rest)
			     (statements rest '(() ()) #f)
			     (values `(,type ,test-param ,(car state) 
					     ,(cadr state)) rest))))))
  (define (condition ts type)
    (cond ((if-type=? "ifnum" type)
	   (ifnum-param ts))
	  (else 
	   (error <read-if-error> "undefined if statement" (perror ts)))))
  (define (statements rest s has-else?)
    (define (update-s t)
      `((,t . ,(car s)) . ,(cdr s)))
    (cond 
     ((not (textoken? (car rest)))
      (statements (cdr rest) (update-s (car rest)) has-else?))
     ((if? (car rest))
      (receive (inner-if rest)
	       (values (car (ifen rest)) (cdr (ifen rest)))
	       (statements rest (update-s inner-if) has-else?)))
     ((fi? (car rest))
      (let1 s (map reverse s)
	    (if has-else? (values (reverse s) (cdr rest))
		(values s (cdr rest)))))
     ((else? (car rest))
      (statements (cdr rest) (reverse s) #t))
     (else
      (statements (cdr rest) (update-s (car rest)) has-else?))))
  (define (out ts)
    (cond ((null? ts)
	   (values '() '()))
	  ((if? (car ts))
	   (in (cdr ts) (car ts)))
	  ((or (texspaces? (car ts))
	       (par? (car ts)))
	   (out (cdr ts)))
	  (else
	   (error "here expects if statement." (perror ts)))))
  (out ts))

(define ifen
  (put-specific-code -103 if? get-if-statement))

(define (process-if ifstate)
  (if (not (= -103 (car ifstate))) (error "here expects a if statement.")
      (receive (type param attrue atfalse)
	       (values (cadr ifstate) (caddr ifstate) 
		       (cadddr ifstate) (car (cddddr ifstate)))
	       '())))
