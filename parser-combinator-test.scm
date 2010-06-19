(use gauche.test)

(test-start "parser-combinator")
(load "parser-combinator.scm")

(parser-test* "number"
	      "1" "2"
	      (orothers "digit" 0 1 2 3 4 5 6 7 8 9) "12")

(parser-test* "one space"
	      " " ""
	      tex-space1 "  \n ")

(parser-test* "tex integer constatnt"
	      "1234" "abc00"
	      (parser-many (orothers "digit" 0 1 2 3 4 5 6 7 8 9)) "1234abc00")

(parser-test* "skip"
	      "" "1234"
	      (skip tex-space1) "    1234")

(parser-test* "anytoken"
	      "\\let" "1234"
	      any-token "\\let1234")

(test-end)