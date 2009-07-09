(use gauche.test)

(test-start "parser-combinator")
(load "parser-combinator.scm")

(parser-test* "number"
	      "1" "2"
	      tex-number "12")

(parser-test* "one space"
	      " " ""
	      tex-space1 "  \n ")

(parser-test* "tex integer constatnt"
	      "1234" "abc00"
	      tex-int-const "1234abc00")


