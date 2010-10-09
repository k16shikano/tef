;; math.scm

; mlist := [100, noad] (display style) or [200, noad] (text style)
; noad  := [atom description, nuclear, supscript, subscript]
; nuclear,supscript,subscript := mathchar | mlist | box

; The representation of mathchar is not same as that of TeX82.
; Every mathchar is a unicode character.
; (eventually we omit some TeX features such as \fam.)

(use srfi-1)
(use gauche.collection)
(use util.list)
(load "box.scm")
(load "codes.scm")
(load "eqtb.scm")
(load "tokenlist-utils.scm")
(load "parser-combinator/parser-combinator.scm")

(define (mlist ts codetbl limit)
  (define (loop token result next-field spec)
    (cond 
     ((and (textoken? token) (= 10 (cat token)))
      (values result next-field spec))
     ;; fraction noad
     ((fracspec? token)
      (values `(,result) next-field token))
     ;; radical noad
     ((radicalspec? token)
      (values result -3 (cons token spec)))
     ((= -3 next-field)
      (values (make-radical-noad token (car spec) result) 0 (cdr spec)))
     ;; supscr
     ((= 7 (car token))
      (values result -1 spec))
     ((= -1 next-field)
      (values (set-supscr token result) 0 spec))
     ;; subscr
     ((= 8 (car token))
      (values result -2 spec))
     ((= -2 next-field)
      (values (set-subscr token result) 0 spec))
     ;; minus
     ((minus-symbol? token)
      (values (make-minus-noad token result) 0 spec))
     ;; mathprim
     ((mathprim? token)
      (values result (classname->num (cdr token)) spec))
     ((> next-field 0)
      (let* ((token (or (find-mathcode token codetbl) token))
	     (noad  (cons (cons (select-atom next-field)
				(cdar (make-noad token result))) 
			  result)))
	(values noad 0 spec)))
     ;; limit
     ((nolimits? token)
      (set! limit 1)
      (values result 0 spec))
     ((limits? token)
      (set! limit 2)
      (values result 0 spec))
     (else
      (values (make-noad token result) 0 spec))))

  (define (make-noad token result)
    (cond ((null? token)
	   (cons '(() () ()) result))
	  ;; group
	  ((= -100 (car token))
	   (cons `(Inner 
		   ,(mlist (cdr token) (cons (make-eqtb) codetbl) limit)
		   () ())
		 result))
	  ;; box
          ((= -102 (car token))
	   (cons `(Box ,(expand-box token) () ()) result))
	  ;; align
          ((= -103 (car token))
	   (cons `(Inner ,token () ()) result))
	  (else
	   (cons `(,(select-atom token) 
		   ,(or (find-mathcode token codetbl) token)
		   () ())
		 result))))

  (define (minus-symbol? token)
    (and (textoken? token) 
	 (= 12 (cat token)) 
	 (char=? #\- (cdr token))))

  (define (make-minus-noad t result)
    (if (or (null? result) (null? (car result)) 
	    (not (memq (caar result) '(Ord Inner Close))))
	(cons `(Ord ,(find-mathcode t codetbl) () ()) result)
	(cons `(Bin ,(find-mathcode t codetbl) () ()) result)))

  (define (make-radical-noad token spec result)
    (cons `(Rad ,(mlist (cdr token) codetbl limit) () () ,(cadr spec)) result))

  (define (set-subscr token result)
    (if (null? result)
	`((Nil () () ,(make-noad token '())))
	(let1 head (car result)
	  (let1 token (make-noad token '())
		(cons 
		 (if (eq? 'Rad (first head))
		     `(Rad ,(second head) ,(third head) ,token (fifth head))
		     `(,(first head) ,(second head) ,(third head) ,token))
		 (cdr result))))))

  (define (set-supscr token result)
    (if (null? result)
	`((Nil () ,(make-noad token '()) ()))
	(let1 head (car result)
	  (let1 token (make-noad token '())
		(cons
		 (if (eq? 'Rad (first head))
		     `(Rad ,(second head) ,token ,(fourth head) ,(fifth head))
		     `(,(first head) ,(second head) ,token ,(fourth head)))
		 (cdr result))))))

  (define (make-fraction spec numerator denominator)
    (list
     (cons 'Fraction
	   (cond ((over? (car spec))
		  `(default-code ,numerator ,denominator () ()))
		 ((atop? (car spec))
		  `((0) ,numerator ,denominator () ()))
		 ((above? (car spec))
		  `(,(second spec) ,numerator ,denominator () ()))
		 ((overwithdelims? (car spec))
		  `(default-code ,numerator ,denominator 
		     (,(second spec)) (,(third spec))))
		 ((atopwithdelims? (car spec))
		  `((0) ,numerator ,denominator 
		    (,(second spec)) (,(third spec))))
		 ((abovewithdelims? (car spec))
		  `(,(fourth spec) ,numerator ,denominator 
		    (,(second spec)) (,(third spec))))))))
  
  (receive (result next-field spec)
	   (fold3 loop '() 0 #f ts)
	   (cons (* 100 limit) 
		 (let1 result (reverse result)
		       (if spec
			   (make-fraction spec 
					  (reverse (car result)) 
					  (cdr result))
			   result)))))

(define (select-atom token)
  (cond ((ord?   token) 'Ord)
 	((op?    token) 'Op)
 	((bin?   token) 'Bin)
 	((rel?   token) 'Rel)
 	((open?  token) 'Open)
 	((close? token) 'Close)
 	((punct? token) 'Punct)
	((inner? token) 'Inner)
;; 	((over?  token) 'Over)
;; 	((under? token) 'Under)
;; 	((acc?   token) 'Acc)
;; 	((vcent? token) 'Vcent)
))

(define (asis-mathchar? token)
  (and (textoken? token)
       (> (cat token) 10)
       (char-set-contains? #[a-zA-Z0-9!/] (cdr token))))

(define (asis-binchar? token)
  (and (textoken? token)
       (> (cat token) 10)
       (char-set-contains? #[+\-] (cdr token))))

(define (asis-relchar? token)
  (and (textoken? token)
       (> (cat token) 10)
       (char-set-contains? #[=><] (cdr token))))

(define (asis-openchar? token)
  (and (textoken? token)
       (> (cat token) 10)
       (char-set-contains? #[(] (cdr token))))

(define (asis-closechar? token)
  (and (textoken? token)
       (> (cat token) 10)
       (char-set-contains? #[)] (cdr token))))

(define (asis-punctchar? token)
  (and (textoken? token)
       (> (cat token) 10)
       (char-set-contains? #[,] (cdr token))))

(define (ord? token)
  (or (asis-mathchar? token)
      (= 0 (mathclass token))))

(define (op? token)
  (= 1 (mathclass token)))

(define (bin? token)
  (or (asis-binchar? token)
      (= 2 (mathclass token))))

(define (rel? token)
  (or (asis-relchar? token)
      (= 3 (mathclass token))))

(define (open? token)
  (or (asis-openchar? token)
      (= 4 (mathclass token))))

(define (close? token)
  (or (asis-closechar? token)
      (= 5 (mathclass token))))

(define (punct? token)
  (or (asis-punctchar? token)
      (= 6 (mathclass token))))

(define (inner? token)
  (= 8 (mathclass token)))

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
	((abovewithdelims? (car ts))
	 (receive (dimen rest)
		  (get-tex-dimen (cdddr ts))
		  (values (list (car ts) (cadr ts) (caddr ts) 
				(dimen->sp (car dimen))) rest)))))

;; A math token is list of a number whose top hexadecimal represents 
;; the class and the rest is the unicode encoding.
;; We omit the mechanisim of the math font family in TeX82.

(define mathclass-table
  ; vari won't be used. 
  (zip '(mathord mathop mathbin mathrel mathopen mathclose mathpunct vari mathinner)
       '(0 1 2 3 4 5 6 7 8)))

(define (classname->num x)
  (let1 x (cond ((symbol? x) x)
		((string? x) (string->symbol x))
		(else (error "expecting class name, but got " x)))
	(cadr (assoc x mathclass-table))))

(define (char->mathtoken class char)
  (list
   (+ (* 65536 (if (number? class) class
		   (classname->num class)))
      (if (number? char) char (char->ucs char)))))

(define (mathchar mathtoken)
  (if (integer? (car mathtoken))
      (remainder (car mathtoken) #x10000)
      mathtoken))

(define (mathclass token)
  (cond ((integer? token) token)
	((textoken? token) 7)
	(else
	 (floor (/ (car token) #xffff)))))




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
       mathopen? mathclose? mathpunct? mathinner?))

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

(defpred radical? "radical")

(define (radicalspec? token)
  (if (and (pair? (car token)) 
	   (textoken? (car token)))
      (radical? (car token))
      #f))

(defpred nolimits? "nolimits")
(defpred limits? "limits")
