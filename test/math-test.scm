(use gauche.test)

(test-start "tex-modoki math related features")
(load "read.scm")
(load "show.scm")
(load "output-loop.scm")

(test-section "parse math list")
(load "math.scm")
(load "box.scm")

(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->string
      (output
       (string->tokenlist 
	"\\def\\intop{\\mathchar\"1222b}\
         $\\intop_{-1}^{1}x^2dx$"))))))

(test* "make mlist" 
       '((100 (Ord (11 . #\x) (((100 (Ord (11 . #\y) ((Ord (12 . #\2) () ())) ())))) ())))
       (output 
	(string->tokenlist "$x^{y^2}$")))

(test* "math with box" 
       '((100 (Ord (11 . #\x) (((100 (Box ((-102 (0 (11 . #\y)))) () ())))) ((Ord (11 . #\u) () ())))))
       (output
	(string->tokenlist "$x^{\\hbox{y}}_u$")))

(test* "math with fraction" 
       '((100 (Fraction default-code ((Ord (11 . #\x) () ())) ((Ord (11 . #\y) () ())) () ())))
       (output
	(string->tokenlist "$x\\over y$")))

(test* "math with fraction" 
       '((100 (Fraction (-101 . -80) ((Ord (11 . #\x) () ())) ((Ord (11 . #\y) () ())) () ())))
       (output
	(string->tokenlist "$x\\above-'120pt y$")))

(test-section "math token")

(test* "\intop"
       '(#x1222b)
       (mathtoken 'mathop #x222b))

(test-end)

