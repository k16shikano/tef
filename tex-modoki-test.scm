(use gauche.test)

(add-load-path ".")
(load "tex-modoki.scm")

(test* "read-tex-token: case \"{\\\\hskip 136 pt}\"" 
       '((1 . #\{) (-10 . "hskip") (12 . #\1) (12 . #\3) (12 . #\6) (10 . #\ ) (11 . #\p) (11 . #\t) (2 . #\}))
       (with-input-from-string "{\\hskip 136 pt}" 
	 (lambda () 
	   (port-map values read-tex-token))))

(test* "read-tex-token: case \"    d     \""
       '((10 . #\space) (11 . #\d) (10 . #\space)) 
       (with-input-from-string "    d     " 
	 (lambda () 
	   (port-map values read-tex-token))))

(test* "tokenlist->string for \"{\\\\hskip 36 pt}{This    is \\\\bf{pen}}\""
       "{\\hskip 36 pt}{This is \\bf{pen}}" 
       (with-input-from-string "{\\hskip 36 pt}{This    is \\bf{pen}}" 
	 (lambda () 
	   (tokenlist->string (port-map values read-tex-token)))))

(test* "get-tex-group" 
       "This information should be {centered}"
       (tokenlist->string 
	(get-tex-group 
	 (string->tokenlist "{This information should be    {centered}}"))))

(test* "get-args" 
       '(((12 . #\1)) ((12 . #\2)) ((12 . #\3)))
       (get-args 3 (string->tokenlist "{1}{2}{3}")))

(test* "get-command-sequence" 
       '((-1 . "twocol") ((12 . #\1)) ((12 . #\2))) 
       (values-ref 
	(get-command-sequence "twocol" 2 (string->tokenlist "abc \\twocol{1}\n {2}abc")) 
	1))


(test* "read-tex-token: case \"    d     \""
       '((10 . #\space) (11 . #\d) (10 . #\space)) 
       (with-input-from-string "   d   " 
	 (lambda () 
	   (port-map values read-tex-token))))

(test* "get-inline-math" 
       '((11 . #\a) (11 . #\b) (11 . #\c) (10 . #\space) (12 . #\=) (10 . #\space) (12 . #\1))
       (get-inline-math (string->tokenlist "$abc = 1$.")))

(test* "get-comment-line"
       '((14 . #\%) (10 . #\space) (11 . #\c) (11 . #\o) (11 . #\m) (11 . #\m) (11 . #\e) (11 . #\n) (11 . #\t) (10 . #\space)) 
       (get-comment-line (string->tokenlist "% comment \n")))

