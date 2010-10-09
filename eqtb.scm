;; Table of Equivalents. (TeX The Program 220)
;; Here we simply define it as a hashtable containing hashtables
;; each represents the active characters base, control sequence 
;; base, catcode base, token base, etc.

(define (make-eqtb)
  (hash-table 'eq?
    `(active-character . ,(make-hash-table))
    `(control-sequence . ,(make-hash-table))
    `(catcode  . ,(make-hash-table))
    `(mathcode . ,(make-hash-table))
    `(count-base  . ,(make-hash-table))
    `(dimen-base  . ,(make-hash-table))
    `(skip-base   . ,(make-hash-table))
    `(muskip-base . ,(make-hash-table))
    ))

(define (eqtb-get tb base key)
  (hash-table-get 
   (hash-table-get tb base #f) key #f))

(define (eqtb-update! tb base key new)
  (define (base-update! base)
    (let ()
      (if (hash-table-exists? base key)
	  (hash-table-update! base key (lambda (old) new))
	  (hash-table-put! base key new))
      base))
  (if (hash-table-exists? tb base)
      (hash-table-update! tb base base-update!)
      (error "Unknown base:" base)))


