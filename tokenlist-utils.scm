;;;; utilities for tex-modoki parser

(define (perror ls)
  (if (<= (length ls) 20)
      (tokenlist->string ls)
      (string-append (tokenlist->string (take ls 20)) "...")))

(define (string+char str . char)
  (string-append str (apply string char)))

(define (token->symbol token)
  (cond ((string? token) 
	 (string->symbol token))
	((char? token)
	 (string->symbol (string token)))
	(else
	 #f)))

;; [token] -> [token] -> [token] or #f
(define (match-head ls pattern) 
  (if (equal? (map (lambda (p l) l) pattern ls) pattern)
      (drop ls (length pattern))
      #f))

;; [token] -> [get token] [rest token]
(define (put-specific-code code finder getter)
  (lambda (ls . env)
    (if (null? ls)
	'()
	(if (finder (car ls))
	    (receive (group unseen)
		     (if (null? env)
			 (getter ls)
			 (getter ls (car env)))
		     (cons (if (null? group) '() `(,code . ,group)) unseen))
	    ls))))

;; are there any values whose first value is not null?
(define-syntax orvalues
  (syntax-rules ()
    ((_) (values '(-101 . #f) '()))
    ((_ v1) v1)
    ((_ v1 v2 ...) 
     (if (not (cdar (values-ref v1 0))) (orvalues v2 ...) v1))))

(define-syntax orp
  (syntax-rules ()
    ((_) #t)
    ((_ p1) (lambda (x) (p1 x)))
    ((_ p1 ...) (lambda (x) (or (p1 x) ...)))))


;; predicates

(define (textoken? t)
  (and (pair? t)
       (dotted-list? t)
       (number? (car t))))

(define (cat token)
  (if (textoken? token) (car token) 
      (error "not token" token)))

(define (begingroup? t)
  (and (textoken? t)
       (= 1 (car t))))

(define (endgroup? t)
  (and (textoken? t)
       (= 2 (car t))))

(define (mathdollar? t)
  (and (textoken? t)
       (= 3 (car t))))

(define (texspaces? t)
  (and (textoken? t)
       (or (=  5 (car t))
	   (= 10 (car t))
	   (=  9 (car t)))))

(define (newline? t)
  (and (textoken? t)
       (=  5 (car t))))

(define (commenthead? t)
  (and (textoken? t)
       (=  14 (car t))))

(define (charlbrace? token)
  (and (= 11 (cat token))
       (char=? #\{ (cdr token))))

(define (parameter? token)
  (= -15 (car token)))

(define-syntax defpred
  (syntax-rules ()
    ((_ name str)
     (define (name token)
       (and (textoken? token)
	    (< (car token) 0)
	    (string=? str (cdr token)))))))

(defpred par? "par")
(defpred expandafter? "expandafter")
(defpred noexpand? "noexpand")
(defpred global? "global")

(defpred let? "let")
(defpred def? "def")
(defpred edef? "edef")
(defpred gdef? "gdef")
(defpred xdef? "xdef")
(defpred mathchardef? "mathchardef")

(define (assignment? token)
  (or (let? token)
      (def? token)
      (edef? token)
      (gdef? token)
      (xdef? token)
      (mathchardef? token)
      ))

(define (if? token)
  (and (textoken? token)
       (< (cat token) 0)
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

(defpred catcode?  "catcode")
(defpred mathcode? "mathcode")

(define codename? (orp catcode? mathcode?))

(defpred count? "count")
(defpred dimen? "dimen")
(defpred skip? "skip")
(defpred muskip? "muskip")

(define register? (orp count? dimen? skip? muskip?))
