;; math.scm

; mlist := [100, noad]
; noad  := [atom description, nuclear, supscript, subscript]
; nuclear,supscript,subscript := mathchar | mlist | box

; The representation of mathchar is not same as that of TeX82.
; Every mathchar is a unicode character.
; (eventually we omit some TeX features such as \fam.)

(use srfi-1)
(use gauche.collection)
(load "tex-modoki.scm")
(load "box.scm")
(load "parser-utils.scm")

(define (mlist ts)
  (define (loop token result next-field)
    (cond ;; supscr
	  ((= 7 (car token))
	   (values result 1))
	  ((= 1 next-field)
	   (values (set-supscr token result) 0))
	  ;; subscr
	  ((= 8 (car token))
	   (values result 2))
	  ((= 2 next-field)
	   (values (set-subscr token result) 0))
	  (else
	   (values (make-noad token result) 0))))

  (define (make-noad token result)
    (cond ((null? token)
	   (cons '(() () ()) result))
	  ;; group
	  ((= -100 (car token))
	   (cons `(,(mlist (cdr token))) result))
	  ;; box (shouldn't use expand-box here. to be fixed)
          ((= -102 (car token))
	   (cons `(Box ,(expand-box token `(,(make-hash-table))) () ()) result))
	  (else
	   (cons `(,(select-atom token) ,token () ()) result))))

  (define (set-subscr token result)
    (if (null? result)
	`((Nil () () ,(make-noad token '())))
	(let1 head (car result)
	  (let1 token (make-noad token '())
		(cons `(,(first head) ,(second head) ,(third head) ,token) 
		      (cdr result))))))

  (define (set-supscr token result)
    (if (null? result)
	`((Nil () ,(make-noad token '()) ()))
	(let1 head (car result)
	      (let1 token (make-noad token '())
		    (cons `(,(first head) ,(second head) ,token ,(fourth head)) 
			  (cdr result))))))

  (cons 100 (reverse (fold2 loop '() 0 ts))))

(define (select-atom token)
  (cond ((ord?   token) 'Ord)))
;; 	((op?    token) 'Op)
;; 	((bin?   token) 'Bin)
;; 	((rel?   token) 'Rel)
;; 	((open?  token) 'Open)
;; 	((close? token) 'Close)
;; 	((punct? token) 'Punct)
;; 	((inner? token) 'Inner)
;; 	((over?  token) 'Over)
;; 	((under? token) 'Under)
;; 	((acc?   token) 'Acc)
;; 	((rad?   token) 'Rad)
;; 	((vcent? token) 'Vcent)))

(define (ord? token)
  (or 
   (and (textoken? token)
	(> (cat token) 10)
	(char-set-contains? #[a-zA-Z0-9] (cdr token)))
   (= 0 (mathclass token))))

(define (op? token)
  (= 1 (mathclass token)))


;; [token] -> ([token] and [token])
(define-condition-type <read-math-error> <error> #f)
(define (get-inline-math ls)
  (define (in-math ls body)
    (cond ((null? ls)
	   (error <read-math-error> "unterminated math $"))
	  ((box? (car ls))
	   (let1 boxed (boxen ls)
		 (in-math (cdr boxed) (cons (car boxed) body))))
	  ((mathdollar? (car ls))
	   (values (reverse body) (cdr ls)))
	  (else
	   (in-math (cdr ls) (cons (car ls) body)))))
  (define (out-math ls)
    (cond ((null? ls)
	   (values '() '()))
	  ((mathdollar? (car ls))
	   (in-math (cdr ls) '()))
	  ((texspaces? (car ls))
	   (out-math (cdr ls)))
	  (else
	   (error <read-math-error> "the first token shoule have catcode 3"))))
  (out-math ls))

(define (beginmath? token)
  (and (textoken? token)
       (= 3 (cat token))))

(define mathen
  (put-specific-code 100 beginmath? get-inline-math))


;; A math token is list of a number whose top hexadecimal represents 
;; the class and the rest is the unicode encoding.
;; We omit the mechanisim of the math font family in TeX82.

(define mathclass-table
  ; vari won't used. 
  (zip '(mathord mathop mathbin mathrel mathopen mathclose mathpunct vari)
       '(0 1 2 3 4 5 6 7)))

(define (mathtoken class char)
  (list
   (+ (* 65536 (if (number? class) class
		   (cadr (assoc class mathclass-table))))
      (if (number? char) char (char->ucs char)))))

(define (mathclass token)
  (if (textoken? token) 0
      (floor (/ (car token) #xffff))))

; example of \intop
; (mathtoken 'mathop #x222b) ;=> (#x1222b)


;;pred

(defpred mathord?   "mathord")
(defpred mathop?    "mathop")
(defpred mathbin?   "mathbin")
(defpred mathrel?   "mathrel")
(defpred mathopen?  "mathopen")
(defpred mathclose? "mathclose")
(defpred mathpunct? "mathpunct")
(defpred mathinner? "mathinner")
(defpred underline? "underline")
(defpred overline?  "overline")

(define mathprim?
  (or mathord? mathop? mathbin? mathrel?
      mathopen? mathclose? mathpunct?))
