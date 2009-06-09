(use gauche.test)

(load "tex-modoki.scm")

(test* "read-tex-token: case \"{\\\\hskip 136 pt}\"" 
       '((1 . #\{) (-1 . "hskip") (12 . #\1) (12 . #\3) (12 . #\6) (10 . #\ ) (11 . #\p) (11 . #\t) (2 . #\}))
       (with-input-from-string "{\\hskip 136 pt}" 
	 (lambda () 
	   (port-map values read-tex-token))))

(test* "read-tex-token: case \"    d     \""
       '((10 . #\space) (11 . #\d) (10 . #\space)) 
       (with-input-from-string "    d     " 
	 (lambda () 
	   (port-map values read-tex-token))))

(test* "tokenlist->string for \"{\\\\hskip 36 pt}{This    is \\\\bf{pen}}\""
       "{\\hskip36 pt}{This is \\bf{pen}}" 
       (with-input-from-string "{\\hskip 36 pt}{This    is \\bf{pen}}" 
	 (lambda () 
	   (tokenlist->string (port-map values read-tex-token)))))

(test* "get-tex-group" 
       "This information should be {centered}"
       (tokenlist->string 
	(get-tex-group 
	 (string->tokenlist "{This information should be    {centered}}"))))

;;;;;;;;;;;;;;

(load "tex-trim-utils.scm")

(test* "get-args" 
       '(((12 . #\1)) ((12 . #\2)) ((12 . #\3)))
       (get-args 3 (string->tokenlist "{1}{2}{3}")))

(test* "get-command-sequence" 
       '((11 . #\a) (11 . #\b) (11 . #\c) (10 . #\space)) 
       (get-command-sequence 
	"twocol" 2 
	(string->tokenlist "abc \\twocol{1}\n {2}abc")))

(test* "get-command-sequence" 
       '((-1 . "twocol") ((12 . #\1)) ((12 . #\2))) 
       (values-ref 
	(get-command-sequence 
	 "twocol" 2 
	 (string->tokenlist "abc \\twocol{1}\n {2}abc"))
	1))

(test* "read-tex-token: case \"    d     \""
       '((10 . #\space) (11 . #\d) (10 . #\space)) 
       (with-input-from-string "   d   " 
	 (lambda () 
	   (port-map values read-tex-token))))

(test* "get-inline-math" 
       '((11 . #\a) (11 . #\b) (11 . #\c) (10 . #\space) 
	 (12 . #\=) (10 . #\space) (12 . #\1))
       (get-inline-math (string->tokenlist "$abc = 1$.")))

(test* "get-comment-line"
       '()
       (get-comment-line (string->tokenlist "% comment \n")))

;;;;;;;;;;;;;;

(load "def-macro.scm")

(test* "parse parameter"
       '(((11 . #\a)) ((-15 . 1) (12 . #\.) (10 . #\space)) ((-15 . 2)))
       (parse-parameter (string->tokenlist "a#1. #2{...}aa")))

(test* "match-def-parameter: a sample from ch20 of the TeX book"
       '("You owe {\\$5.00}" " Pay it.")
       (map tokenlist->string 
	    (match-def-parameter
	     (string->tokenlist "You owe {\\$5.00}. Pay it.\\par{...}")
	     (string->tokenlist "#1.#2\\par{...}"))))

(test* "match-def-parameter: a sample from ch20 of the TeX book"
       '("\\LOOK" "" "{And\\$ }{look}")
       (map tokenlist->string 
	    (match-def-parameter
	     (string->tokenlist "AB {\\LOOK}C${And\\$ }{look}\\$ 5.")
	     (string->tokenlist "AB #1#2C$#3\\$ {...}"))))

(test* "match-def-parmeter: rest of tokens"
       "5."
       (tokenlist->string 
	(values-ref
	 (match-def-parameter
	  (string->tokenlist "AB {\\LOOK}C${And\\$ }{look}\\$ 5.")
	  (string->tokenlist "AB #1#2C$#3\\$ {...}"))
	 1)))

;;;;;;;;;;;;;;

(load "output-loop.scm")

(test* "eval macro: exercise 20.2 of the TeX book" 
       "ABCAB" 
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
\\def\\a{\\b}\
\\def\\b{A\\def\\a{B\\def\\a{C\\def\\a{\\b}}}}\
\\def\\puzzle{\\a\\a\\a\\a\\a}\
\\puzzle") 
	 (list (make-hash-table)))))

(test* "innner parameter definition"
       "bb" ; neither bb nor cc
       (tokenlist->string
	(driver-loop
	 (string->tokenlist "\\def\\/{c}\\def\\a#1{\\def\\/{b}#1}\\a\\/\\/")
	 global-env
	 )))

(test* "expandafter: exercise 20.2 of the TeX book" 
       "yx" 
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
\\def\\A#1,#2{#2#1}\
\\def\\B#1;#2{#1,#2}\
\\def\\C{x;y}\
\\expandafter\\A\\expandafter\\B\\C")
	 (list (make-hash-table)))))

(test* "box expansion" 
       "x" 
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
\\hbox to 3pt{x}")
	 (list (make-hash-table)))))

(test* "get-tex-dimen-after and orvalues"
       "1.0pt"
       (tokenlist->string
	(let ((ts (string->tokenlist "spread 1.0pt{...}")))
	  (orvalues (get-tex-dimen-after "to" ts)
		    (get-tex-dimen-after "spread" ts)))))

(test* "\\def#1#{...}" 
       "x" 
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
\\def\\a#1#{\\hbox to #1}\
\\a3pt{x}")
	 (list (make-hash-table)))))

(test* "\\def#1#{...}" 
       "1cx" ; not 1xc!
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
\\def\\a#1#{#1x}\
\\a1c")
	 (list (make-hash-table)))))

(test* "\\edef..." 
       "xyxyxyxy"
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
\\def\\double#1{#1#1}\
\\edef\\a{\\double{xy}}\
\\edef\\a{\\double\\a}\
\\a")
	 (list (make-hash-table)))))

(test* "for real lexer" 
       "You owe \\$5.00!!! --- Pay it.!!!"
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
\\def\\cs #1. #2\\par{#1!!! --- #2!!!}%
\\cs You owe \\$5.00. Pay it.


")
	 (list (make-hash-table)))))

(test* "for real lexer" 
       "a\n\nb x s\n\nc"
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
a

b x
s

c")
	 (list (make-hash-table)))))

(test* "for real lexer" 
       "\\ab c"
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\
\\ab c")
	 (list (make-hash-table)))))

(test* "for box parameters" 
       "x"
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist "\\def\\w{3pt}\\hbox to\\w{x}")
	 (list (make-hash-table)))))

(test* "for box parameters" 
       "a012b012c"
       (tokenlist->string 
	(driver-loop
	 (string->tokenlist 
	  "\\def\\newtoken#1{\\def#1=##1{a##1b##1c}}\\newtoken\\ss\\ss={012}")
	 (list (make-hash-table)))))

