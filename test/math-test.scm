(use gauche.test)

(test-start "tex-modoki math related features")
(load "read.scm")
(load "show.scm")
(load "output-loop.scm")
(load "tokenlist-utils.scm")

(test-section "parse math list")
(load "math.scm")
(load "box.scm")

(test* "make mlist" 
       '((100 (Ord (11 . #\x) ((Inner (100 (Ord (11 . #\y) () ())) () ())) ((Ord (12 . #\2) () ())))))
       (output 
	(string->tokenlist "$x^{y}_2$")))


(test* "make mlist" 
       '((100 (Ord (11 . #\x) ((Inner (100 (Ord (11 . #\y) ((Ord (12 . #\2) () ())) ())) () ())) ())))
       (output 
	(string->tokenlist "$x^{y^2}$")))
 
(test* "math with box" 
       '((100 (Ord (11 . #\x) ((Inner (100 (Box ((-102 (0 (11 . #\y)))) () ())) () ())) ((Ord (11 . #\u) () ())))))
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
       (char->mathtoken 'mathop #x222b))

(test-end)

(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->string
      (output
       (string->tokenlist
	"\\mathchardef\\infty = \"0221e\
         \\mathchardef\\sum = \"103a3\
         \\mathchardef\\cdotp = \"000b7\
         \\def\\cdots{\\mathinner{\\cdotp\\cdotp\\cdotp}}\
         The exponetial function
         $e^x = \\sum_{n=0}^{\\infty} {x^n \\over n!} = \
          {x^1\\over 1!} + {x^2\\over 2!}+ \\cdots$ ."))))))

#;(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->string
      (output
       (string->tokenlist
	"\\mathchardef\\intop=\"1222b\
         \\mathchardef\\infty=\"221e\
         \\def\\sqrt{\\radical\"2221a}\
         $\\intop_{-\\infty}^\\infty \\sqrt{x\\over y\\sqrt{z}i} dx$"))))))
