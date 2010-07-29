(use gauche.test)

(test-start "tex-modoki")
(load "../tex-modoki.scm")
(load "../output-loop.scm")

(test-section "parse math list")
(load "../math.scm")
(load "../box.scm")

(test* "math text" 
       '((100 (Ord (11 . #\x) (((100 ((-102 (0 (100 (Ord (11 . #\y) () ())))))) () ())) ((Ord (11 . #\u) () ())))))
       (output
	 (string->tokenlist "$x^{\\hbox{$y$}}_u$")))

#;(test* "make mlist" 
       '((100 (Ord (11 . #\x) (12 . #\2) ())))
       (output
	(string->tokenlist "x$x y$")))

#;(test* "make mlist" 
       '(100 (Ord (11 . #\x) (100 (Ord (11 . #\y) (12 . #\2) ())) ()))
	(output 
	 (string->tokenlist "$x^{y^2}$")))

(test-end)


