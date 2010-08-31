;; math.scm

; mlist := [100, noad]
; noad  := [atom description, nuclear, supscript, subscript]
; nuclear,supscript,subscript := mathchar | mlist | box

; The representation of mathchar is not same as that of TeX82.
; Every mathchar is a unicode character.
; (eventually we omit some TeX features such as \fam.)

(use srfi-1)
(use gauche.collection)
(load "box.scm")
(load "tokenlist-utils.scm")
(load "parser-combinator/parser-combinator.scm")

(define (mlist ts)
  (define (loop token result next-field fracspec)
    (cond 
     ;; fraction noad
     ((fracspec? token)
      (values `(,result) next-field token))
     ;; supscr
     ((= 7 (car token))
      (values result -1 fracspec))
     ((= -1 next-field)
      (values (set-supscr token result) 0 fracspec))
     ;; subscr
     ((= 8 (car token))
      (values result -2 fracspec))
     ((= -2 next-field)
      (values (set-subscr token result) 0 fracspec))
     ;; mathprim
     ((mathprim? token)
      (values result (classname->num (cdr t)) fracspec))
     ((> next-field 0)
      (values 
       (make-noad 
	(by-mathprim next-field (cdr (or (get-mathcode token) token))) result)
       0
       fracspec))
     (else
      (values (make-noad token result) 0 fracspec))))

  (define (make-noad token result)
    (cond ((null? token)
	   (cons '(() () ()) result))
	  ;; group
	  ((= -100 (car token))
	   (cons `(,(mlist (cdr token))) result))
	  ;; box
          ((= -102 (car token))
	   (cons `(Box ,(expand-box token) () ()) result))
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

  (define (by-mathprim classnum t)
    (if classnum (mathtoken classnum (mathchar t)) t))

  (define (make-fraction spec numerator denominator)
    (list
     (cons 'Fraction
	   (cond ((over? (car spec))
		  `(default-code ,numerator ,denominator () ()))
		 ((atop? (car spec))
		  `(() ,numerator ,denominator () ()))
		 ((above? (car spec))
		  `(,(second spec) ,numerator ,denominator () ()))
		 ((overwithdelims? (car spec))
		  `(default-code ,numerator ,denominator 
		     ,(third spec) ,(fourth spec)))
		 ((atopwithdelims? (car spec))
		  `(() ,numerator ,denominator 
		    ,(third spec) ,(fourth spec)))
		 ((abovewithdelims? (car spec))
		  `(,(second spec) ,numerator ,denominator 
		    ,(third spec) ,(fourth spec)))))))
  
  (receive (result next-field fracspec)
	   (fold3 loop '() 0 #f ts)
	   (cons 100 
		 (let1 result (reverse result)
		       (if fracspec
			   (make-fraction fracspec (car result) (cdr result))
			   result)))))

(define (select-atom token)
  (cond ((ord?   token) 'Ord)
 	((op?    token) 'Op)
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
;; 	((vcent? token) 'Vcent)
))

(define (asis-mathchar? token)
  (and (textoken? token)
       (> (cat token) 10)
       (char-set-contains? #[a-zA-Z0-9] (cdr token))))

(define (ord? token)
  (or (asis-mathchar? token)
      (= 0 (mathclass token))))

(define (op? token)
  (= 1 (mathclass token)))

;;;; getter used by output-loop
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

(define (get-mathchar ts)
  (receive (num rest)
	   (tex-int-num ts)
	   (values (list (tex-int->integer num)) rest)))

(define (get-delimiter ts)
  (receive (num rest)
	   (tex-int-num ts)
	   (values (list (tex-int->integer num)) rest)))

(define (get-fracspec ts)
  (cond (((orp over? atop?) (car ts))
	 (values (list (car ts)) (cdr ts)))
	(((orp overwithdelims? atopwithdelims?) (car ts))
	 (values (list (car ts) (cadr ts) (caddr ts)) (cdddr ts)))
	((above? (car ts))
	 (receive (dimen rest)
		  (get-tex-dimen (cdr ts))
		  (values (list (car ts) 
				(dimen->sp (car dimen))) rest)))
	((abovewithdelimes? (car ts))
	 (receive (dimen rest)
		  (get-tex-dimen (cdddr ts))
		  (values (list (car ts) (cadr ts) (caddr ts) 
				(dimen-sp (car dimen))) rest)))))


;; A math token is list of a number whose top hexadecimal represents 
;; the class and the rest is the unicode encoding.
;; We omit the mechanisim of the math font family in TeX82.

(define mathclass-table
  ; vari won't be used. 
  (zip '(mathord mathop mathbin mathrel mathopen mathclose mathpunct vari)
       '(0 1 2 3 4 5 6 7)))

(define (classname->num x)
  (let1 x (cond ((symbol? x) x)
		((string? x) (string->symbol x))
		(else (error "expecting class name, but got " x)))
	(cadr (assoc x mathclass-table))))

(define (mathtoken class char)
  (list
   (+ (* 65536 (if (number? class) class
		   (classname->num class)))
      (if (number? char) char (char->ucs char)))))

(define (mathchar mathtoken)
  (remainder (car mathtoken) #x10000))

(define (mathclass token)
  (if (textoken? token) 0
      (floor (/ (car token) #xffff))))

(define (get-mathcode t)
  (assoc (cdr t) mathcodes))

#;(define mathcodes
  (list
   '(#\< . #x313c)
   '(#\* . #x2203)))


;; preds

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
  (orp mathord? mathop? mathbin? mathrel?
       mathopen? mathclose? mathpunct?))

(defpred mathchar? "mathchar")
(defpred delimiter? "delimiter")

(defpred overwithdelims?  "overwithdelims")
(defpred atopwithdelims?  "atopwithdelims")
(defpred abovewithdelims? "abovewithdelims")
(defpred over?            "over")
(defpred atop?            "atop")
(defpred above?           "above")

(define fraction?
  (orp overwithdelims? atopwithdelims? abovewithdelims?
       over? atop? above?))

(define (fracspec? token)
  (if (and (pair? (car token)) 
	   (textoken? (car token)))
      (fraction? (car token))
      #f))
