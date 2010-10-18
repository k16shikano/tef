(use gauche.test)

(test-start "tex-modoki")
(load "read.scm")
(load "show.scm")
(load "group.scm")

(test-section "some utilites")
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

(test-end)
