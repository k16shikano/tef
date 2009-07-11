(use gauche.test)

(test-start "parser-combinator")
(load "parser-combinator.scm")

(parser-test* "number"
	      "1" "2"
	      (ordigits "digit" 0 1 2 3 4 5 6 7 8 9) "12")

(parser-test* "one space"
	      " " ""
	      tex-space1 "  \n ")

(parser-test* "tex integer constatnt"
	      "1234" "abc00"
	      (parser-many (ordigits "digit" 0 1 2 3 4 5 6 7 8 9)) "1234abc00")

(test-end)