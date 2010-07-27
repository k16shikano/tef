;; math.scm

; (car token) = 100 means the token is a mlist.
 
; The representation of mathchar is not same as that of TeX82.
; Every mathchar is a unicode character.
; (eventually we omit some TeX features such as \fam.)

(use srfi-1)
(use gauche.collection)
(load "tex-modoki.scm")

(define (mlist ts)
  (define (loop token result next-field)
    (cond ;; supscr
	  ((eq? 7 (car token))
	   (values result 1))
	  ((eq? 1 next-field)
	   (values (set-supscr token result) 0))
	  ;; subscr
	  ((eq? 8 (car token))
	   (values result 2))
	  ((eq? 2 next-field)
	   (values (set-subscr token result) 0))
	  (else
	   (values (add-new-atom token result) 0))))

  (define (add-new-atom token result)
    (cons `(Ord ,(math-field token) () ()) result))

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
  (cond ((eq? -100 (car token))
	 (mlist (cdr token)))
	(else
	 token)))
