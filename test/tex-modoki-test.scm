(use gauche.test)

(use read)
(use show)
(use eqtb)
(use output-loop)
(use def-macro)
(use group)
(use num-dimen)
(use register)
(use box)
(use tokenlist-utils)
(use parser-combinator.parser-combinator)

(test-start "tex-modoki")

(test-section "parse tex strings")

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

(test-section "macro definition")


(test* "parse parameter"
       '(((11 . #\a)) ((-15 . 1) (12 . #\.) (10 . #\space)) ((-15 . 2)))
       (parse-parameter (string->tokenlist "a#1. #2{...}aa")))

(test* "match-def-parameter: a sample from ch20 of the TeX book"
       '("You owe \\$5.00" " Pay it.")
       (map tokenlist->string 
	    (match-def-parameter
	     (string->tokenlist "You owe {\\$5.00}. Pay it.\\par{...}")
	     (string->tokenlist "#1.#2\\par{...}"))))

(test* "match-def-parameter: a sample from ch20 of the TeX book"
       '("\\LOOK" "" "And\\$ look")
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

(test-section "macro expantion")

(test* "eval macro: exercise 20.2 of the TeX book" 
       "ABCAB" 
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\a{\\b}\
\\def\\b{A\\def\\a{B\\def\\a{C\\def\\a{\\b}}}}\
\\def\\puzzle{\\a\\a\\a\\a\\a}\
\\puzzle"))))

(test* "innner parameter definition"
       "bb"
       (tokenlist->string
	(output
	 (string->tokenlist "\\def\\/{c}\\def\\a#1{\\def\\/{b}#1}\\a\\/\\/"))))

(test* "expandafter: exercise 20.2 of the TeX book" 
       "yx" 
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\A#1,#2{#2#1}\
\\def\\B#1;#2{#1,#2}\
\\def\\C{x;y}\
\\expandafter\\A\\expandafter\\B\\C"))))

(test* "expandafter: appendix D of the TeX book" 
       "dcba" 
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\A#1#2#3{#1#2#3a}\\def\\B#1#2{#1#2b}\\def\\C#1{#1c}\\def\\D{d}\
\\expandafter\\expandafter\\expandafter\\expandafter\
\\expandafter\\expandafter\\expandafter\\A\
\\expandafter\\expandafter\\expandafter\\B\\expandafter\\C\\D"))))

(test* "\\def#1#{...}" 
       "1cx" ; not 1xc!
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\a#1#{#1x}\
\\a1c"))))

(test* "\\edef..." 
       "xyxyxyxy"
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\double#1{#1#1}\
\\edef\\a{\\double{xy}}\
\\edef\\a{\\double\\a}\
\\a"))))

(test* "for real lexer" 
       "You owe \\$5.00!!! --- Pay it.!!!"
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\cs #1. #2\\par{#1!!! --- #2!!!}%
\\cs You owe \\$5.00. Pay it.


"))))

(test* "text display build-para"
       "a\n\nb x s\n\nc\n\n"
       (tokenlist->string
	(build-para
	 (string->tokenlist "\
a

b x
s

c"))))

(test* "for real lexer" 
       "\\ab c"
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\ab c"))))

(test* "for 'macro'" 
       "a012b012c"
       (tokenlist->string 
	(output
	 (string->tokenlist 
	  "\\def\\newtoken#1{\\def#1=##1{a##1b##1c}}\\newtoken\\ss\\ss={012}"))))

(test* "for inner definition" 
       "0011"
       (tokenlist->string 
	(output
	 (string->tokenlist 
	  "\\def\\a{11}{\\def\\a{00}\\a}\\a"))))

(test* "for global definition" 
       "12"
       (tokenlist->string 
	(output
	 (string->tokenlist 
	  "\\def\\a{0}{\\global\\def\\a{2}\\def\\a{1}\\a}\\a"))))

(test* "inner \\def" 
       "vv" 
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\b#1{}\
\\def\\a#1{#1#1}\
{\\def\\b#1{\\a{#1}}\\b{v}}\
\\b{cb}\
\\b{x}"))))

(test* "global \\def..." 
       "aaAA"
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\a{a}\
{\\a}\
{\\a\\global\\def\\a{A}\\a}\
{\\a}"))))

(test* "global \\edef..." 
       "AAABBB"
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\double#1{#1#1}\
{\\xdef\\triple#1{\\double{#1}#1}\
 \\triple{A}}\
{\\triple{B}}"))))

(test-section "\\let")

(test* "trivial let"
       "*"
       (tokenlist->string
	(output (string->tokenlist "\\let\\b=*\\b"))))

(test* "let" 
       "A: B: B"
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\b{B}\
{\\def\\a#1.{#1:}\
\\let\\b\\a\
\\a{A}.
\\b{B}.}
\\b "))))


(test-section "number and dimension")

(test* "get-tex-dimen-after and orvalues"
       65536.0
       (let ((ts (string->tokenlist "spread1.0pt{...}")))
	 (orvalues (get-tex-dimen-after "to" ts `(,(make-eqtb)))
		   (get-tex-dimen-after "spread" (string->tokenlist "spread1.0pt{...}") `(,(make-eqtb))))))

(define tempenv `(,(make-eqtb)))

(parser-test* "tex-dimen"
	      "3pt" "{x}"
	      (tex-dimen tempenv) "3pt{x}")

(parser-test* "dimen-unit"
	      "pt" ""
	      (dimen-unit tempenv) "pt")

(parser-test* "tex-number"
	      "\"AB" "pt"
	      tex-number "\"ABpt")

(parser-test* "tex-number"
	      "`a" "hoge"
	      tex-number "`a hoge")

(parser-test* "tex-dimen"
	      "54.2em" "{...}"
	      (tex-dimen tempenv) "54.2em{...}")

(test-section "box")

(test* "box expansion" 
       "[this text is boxed] not boxed"
       (tokenlist->string 
	(output
	 (string->tokenlist "\\hbox{this text is boxed} not boxed"))))

(test* "box expansion" 
       "[xx]aa" 
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\hbox spread 3pt{xx}aa"))))

(test* "box expansion" 
       "xx\n\naa" 
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\vbox to 1pc{xx}aa"))))

(test* "\\def#1#{...}"
       "[x]" 
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\a#1#{\\hbox to #1}\
\\a3pt{x}"))))

(test* "for box parameters" 
       "[x]"
       (tokenlist->string 
	(output
	 (string->tokenlist "\\def\\w{3pt}\\hbox to\\w{x}"))))

(test* "box and unbox"
       "box3 is [[A]B], box4 is [AB]"
       (tokenlist->string
	(output
	 (string->tokenlist "\
\\setbox3=\\hbox{A}\\setbox3=\\hbox{\\box3 B}\
\\setbox4=\\hbox{A}\\setbox4=\\hbox{\\unhbox4 B}\
 box3 is \\box3, \
 box4 is \\box4"))))

(test* "box and unbox"
       "[A]"
       (tokenlist->string
	(output
	 (string->tokenlist "\
{\\global\\setbox3=\\hbox{A}\\setbox3=\\hbox{}}\\box3"))))

(test-section "if")

(test* "ifnum" 
       "short  just  over "
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\check#1{\
  \\ifnum \
    #1 > 50\
    \\ifnum  #1< 200\
      just \\else over \\fi\
    \\else short \\fi}\
\\check{10}
\\check{100}
\\check{1000}"))))

(test* "ifx" 
       "yes no no "
       (tokenlist->string 
	(output
	 (string->tokenlist "\
\\def\\a{\\c}\\def\\b{\\d}\
\\def\\c{\\e}\\def\\d{\\e}\
\\def\\e{A}\
\\def\\isifx#1#2{\
  \\ifx#1#2yes \\else no \\fi}\
\\isifx\\c\\d
\\isifx\\a\\b
\\isifx\\d\\e"))))

(test* "if"
       "yes1yes3"
       (tokenlist->string
	(output
	 (string->tokenlist "\
\\def\\a{*}\
\\let\\b=*\
\\def\\c{/}\
\\if*\\a yes1\\fi
\\if\\a\\c yes2\\fi
\\if\\par\\let yes3\\fi"))))

(test* "if"
       "zyes"
       (tokenlist->string
	(output
	 (string->tokenlist "\
\\def\\a{xx}\
\\def\\b{z}\
\\if\\a\\b yes\\else no \\fi"))))

(test* "ifcat"
       "yes1yes2no3"
       (tokenlist->string
	(output
	 (string->tokenlist "\
\\catcode`[=13 \\catcode`]=13 \\def[{*}\
\\ifcat[*yes1\\else no1\\fi\
\\ifcat\\noexpand[\\noexpand]yes2\\else no2\\fi\
\\ifcat\\noexpand[*yes3\\else no3\\fi"))))


(test-section "alignment")

(test* "align"
       '((alignment (((M (Ord (11 . #\a) () ()))) ((12 . #\2) (11 . #\b)) ((M (Ord (11 . #\c) () ()))) ((12 . #\2) (11 . #\d)))) (11 . #\g) (11 . #\g))
       (output 
	(string->tokenlist "\\halign{&$#$&2#\\cr a&b&c&d\\cr}gg")))

(test* "align"
       ; "<table><tr><td>a</td\n><td>2b</td\n><td>c</td\n><td>2d</td\n></tr\n></table\n>gg"
       '((alignment (((11 . #\a)) (#0=(12 . #\2) (11 . #\b)) ((11 . #\c)) (#0# (11 . #\d)))) (11 . #\g) (11 . #\g))					
       (output 
	(string->tokenlist "\\halign to3pt{&#&2#\\cr a&b&c&d\\cr}gg")))

(test-section "codename")
 
(test* "catcode"
       "a<"
       (tokenlist->string
	(output (string->tokenlist "{\\catcode`\\<=1 <a}}<"))))

(test* "mathcode"
       ""
       (tokenlist->string
	(output
	 (string->tokenlist "\\mathcode`\\+=\"202B"))))

(test* "active char"
       "*"
       (tokenlist->string
	(output (string->tokenlist "\\catcode`[=13\\def[{*}["))))

(test-section "registers")

(test* "count register"
       '(100)
       (output (string->tokenlist "\\count20=100\\count\\count20=\\count20\\count100")))

(test* "dimen register"
       '(655360)
       (output (string->tokenlist "\\count20=10\\dimen100=\\count20pt\\dimen100")))

(test* "skip register"
       '()
       (output (string->tokenlist "\\skip2=0pt plus 2fill minus 3fill")))

(test* "advance counter"
       '(7208960)
       (output (string->tokenlist "\\count10=100\\dimen10=10pt\\advance\\dimen10 by \\count10pt\\dimen10")))

(test* "ex.15.9 of the TeX Book"
       "4"
       (tokenlist->string
	(output 
	 (string->tokenlist 
	  "\\count1=5{\\count1=2\\global\\advance\\count1by\\count1\
             \\advance\\count1by\\count1}\\the\\count1"))))

(test* "hskip to glue"
       '((HG 327680 32768.0 32768.0) (11 . #\a))
       (output 
	(string->tokenlist 
	 "\\hskip5pt plus.5pt minus.5pt a")))

(test-end)
