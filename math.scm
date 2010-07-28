;; math.scm

; (car token) = 100 means the token is a mlist.
 
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
	   (values (add-new-atom token result) 0))))

  (define (add-new-atom token result)
    (cond ;; box
          ((= -102 (car token))
	   (cons `(,token () ()) result))
	  (else
	   (cons `(,(math-atom token) ,(math-field token) () ()) result))))

  (define (set-supscr token result)
    (let1 head (car result)
	  (let1 token (math-field token)
		(cons `(,(first head) ,(second head) ,(third head) ,token) 
		      (cdr result)))))

  (define (set-supscr token result)
    (let1 head (car result)
	  (let1 token (math-field token)
		(cons `(,(first head) ,(second head) ,token ,(fourth head)) 
		      (cdr result)))))

  (cons 100 (reverse (fold2 loop '() 0 ts))))

(define (math-field token)
  (cond ((= -100 (car token))
	 (mlist (cdr token)))
	((= -102 (car token))
	 token)
	(else
	 token)))

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



(define (math-atom token)
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
  (and (textoken? token)
       (> (cat token) 10)
       (char-set-contains? #[a-zA-Z0-9] (cdr token))))

