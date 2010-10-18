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
       '((200 (Ord (11 . #\x) ((Inner (200 (Ord (11 . #\y) () ())) () ())) ((Ord (12 . #\2) () ())))))
       (output
	(string->tokenlist "$x^{y}_2$")))

(test* "make mlist" 
       '((200 (Ord (11 . #\x) ((Inner (200 (Ord (11 . #\y) ((Ord (12 . #\2) () ())) ())) () ())) ())))
       (output 
	(string->tokenlist "$x^{y^2}$")))
 
(test* "math with box" 
       '((200 (Ord (11 . #\x) ((Inner (200 (Box ((-102 (0 (11 . #\y)))) () ())) () ())) ((Ord (11 . #\u) () ())))))
       (output
	(string->tokenlist "$x^{\\hbox{y}}_u$")))

(test* "math with fraction" 
       '((200 (Fraction default-code ((Ord (11 . #\x) () ())) ((Ord (11 . #\y) () ())) () ())))
       (output
	(string->tokenlist "$x\\over y$")))

(test* "math with fraction" 
       '((200 (Fraction -5242880 ((Ord (11 . #\x) () ())) ((Ord (11 . #\y) () ())) () ())))
       (output
	(string->tokenlist "$x\\above-'120pt y$")))


(test-section "math token")

(test* "\intop"
       '(#x1222b)
       (char->mathtoken 'mathop #x222b))

(test-section "mathcode")

(test* "mathcode" 
       '((200 (Ord (12 . #\1) . #0=(() ())) (Ord (33) . #0#)))
       (output
	(string->tokenlist "\\mathcode`!=\"21 $1!$")))

(test-end)

(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->html
      (output
       (string->tokenlist
	"\\mathchardef\\infty = \"0221e\
         \\mathchardef\\sum = \"103a3\
         \\mathchardef\\cdotp = \"000b7\
         \\def\\cdots{\\mathinner{\\cdotp\\cdotp\\cdotp}}\
         The exponential function 
         The exponential function
         $e^x = \\sum_{n=0}^{\\infty} {x^n \\over n!} = \
          1 + {x^1\\over 1!} + {x^2\\over 2!}+ \\cdots$ ."))))))

(with-output-to-file 
    "result.html"
  (lambda ()
   (display 
    (tokenlist->html
     (output
      (string->tokenlist
       "\\mathchardef\\intop=\"1222b\
         \\mathchardef\\infty=\"1221e\
         \\mathchardef\\pi=\"003c0\
         \\mathchardef\\sigma=\"003c3\
         \\mathchardef\\mu=\"003bc\
         \\def\\sqrt{\\radical\"2221a}\
         \\def\\int{\\intop\\nolimits}\
         $\\int_{-\\infty}^\\infty \
             {1 \\over \\sqrt{2\\pi\\sigma^2}}\
             \\hbox{exp}(- {{(x-\\mu)}^2\\over 2\\sigma^2}) dx$"))))))

(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->html
      (output
       (string->tokenlist
	"$a_0+{1\\over a_1+{1\\over a_2+{1\\over a_3+{1\\over a_4}}}}$"))))))

(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->html
      (output
       (string->tokenlist
	"$_nC_{k/2} = {n\\atopwithdelims() {k\\over 2}}$"))))))

(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->html
      (output
       (string->tokenlist
	"\\def\\sqrt{\\radical\"2221a}\
         \\mathchardef\\plusminus=\"300b1\
         Roots of the quadratic equation $ax^2+bx+c=0$ are
         $x={-b\\plusminus\\sqrt{b^2 - 4ac} \\over 2a}$"))))))

(with-output-to-file 
    "result.html"
  (lambda ()
    (display 
     (tokenlist->html
      (output
       (string->tokenlist
	"\\def\\matrix#1{\\halign{$##$&&$##$\\cr#1}}\
         \\mathchardef\\lambda=\"003bb\
         $$\\matrix{x-\\lambda & 1 & 0 \\cr 
                    0 & x-\\lambda & 1 \\cr
                    0 & 0 & x-\\lambda \\cr}$$"))))))
