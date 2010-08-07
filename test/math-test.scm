(use gauche.test)

(test-start "tex-modoki")
(load "tex-modoki.scm")
(load "output-loop.scm")

(test-section "parse math list")
(load "math.scm")
(load "box.scm")

(test* "math text" 
       "<i>x<sup><span style=\"font-style:normal\"><span style=\"font-style:normal\">[y]<sup></sup\n><sub></sub\n></span\n></span\n></sup\n><sub><i>u<sup></sup\n><sub></sub\n></i\n></sub\n></i\n>"
       (tokenlist->string
	(output
	 (string->tokenlist "$x^{\\hbox{y}}_u$"))))

(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->string
      (output
       (string->tokenlist "$x^{\\hbox{y}}_u$"))))))

#;(test* "make mlist" 
       '(100 (Ord (11 . #\x) (100 (Ord (11 . #\y) (12 . #\2) ())) ()))
	(output 
	 (string->tokenlist "$x^{y^2}$")))

#;(test* "make mlist" 
       '((100 (Ord (11 . #\x) (12 . #\2) ())))
       (output
	(string->tokenlist "x$x y$")))

(test-end)


