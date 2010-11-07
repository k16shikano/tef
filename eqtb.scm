(define-module eqtb
  (export-all)
  )

(select-module eqtb)

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
    `(count    . ,(make-hash-table))
    `(dimen    . ,(make-hash-table))
    `(skip     . ,(make-hash-table))
    `(muskip   . ,(make-hash-table))
    `(box      . ,(make-hash-table))
    ))

(define (eqtb-get tb base key)
  (hash-table-get 
   (hash-table-get tb base #f) key #f))

(define (eqtb-delete! tb base key)
  (hash-table-delete!
   (hash-table-get tb base #f) key))

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

(provide "eqtb")
