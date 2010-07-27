;; math.scm

; (car token) = 100 means the token is a mlist.
 
; The representation of mathchar is not same as that of TeX82.
; Every mathchar is a unicode character.
; (eventually we omit some TeX features such as \fam.)

(use srfi-1)
(use gauche.collection)
(load "tex-modoki.scm")
(load "box.scm")

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
    (cond ((= -102 (car token))
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

(define (math-atom token)
  (cond ((ord?   token) 'Ord)
	(else 'Hoge)))
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


