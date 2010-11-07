(use gauche.test)
(use read)
(use show)
(use num-dimen)
(use tokenlist-utils)
(use parser-combinator.parser-combinator)

(test-start "parser-combinator")

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
	      (skip tex-space1) "1234")

(parser-test* "skip"
	      "" "1234"
	      (skip tex-space1) "    1234")

(parser-test* "anytoken"
	      "\\let" "1234"
	      any-token "\\let1234")

(parser-test* "space and anytoken"
	      "\\let" "1234"
	      (parser-cont (skip tex-space1) any-token) "\\let1234")

(load "num-dimen.scm")
(parser-test* "do parser"
	      "000" "cc"
	      (parser-do 
	        return (begin (display (map tokenlist->string (list a b))) 
			      (tex-int-num c))
		in a <- (make-string-parser "42aa")
		   b <- (make-string-parser "bb")
		   c <- (parser-many tex-int-num))
	      "42aabb000cc")

(test-end)