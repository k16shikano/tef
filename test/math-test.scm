(use gauche.test)

(test-start "tex-modoki")
(load "../tex-modoki.scm")
(load "../output-loop.scm")

(test-section "parse math list")
(load "../math.scm")
(load "../box.scm")

(test* "make mlist" 
       '(100 (Ord (11 . #\x) (12 . #\2) ()))
       (mlist
	(output (string->tokenlist "x^2"))))

(test* "make mlist" 
       '(100 (Ord (11 . #\x) (100 (Ord (11 . #\y) (12 . #\2) ())) ()))
       (mlist
	(output (string->tokenlist "x^{y^2}"))))

(test* "make mlist" 
       '(100 (Ord (11 . #\x) (100 ((-102 (0 (-100 (11 . #\y)))) () ())) ()))
       (mlist 
	(output (string->tokenlist "x^{\\hbox{y}}"))))

(test-end)

