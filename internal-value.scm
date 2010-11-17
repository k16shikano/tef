(define-module internal-value
  (use parser-combinator.parser-combinator)
  (export internal-int
	  internal-dimen
	  internal-glue
	  internal-mu-glue)
  )

(select-module internal-value)

(define int-parameters-list
  (list
   "pretolerance"
   "tolerance"
   "hbadness"
   "vbadness"
   "linepenalty"
   "hyphenpenalty"
   "exhyphenpenalty"
   "binoppenalty"
   "widowpenalty"
   "displaywidowpenalty"
   "brokenpenalty"
   "predisplaypenalty"
   "postdisplaypenalty"
   "interlinepenalty"
   "floatingpenalty"
   "outputpenalty"
   "doublehyphendemerits"
   "finalhyphendemerits"
   "adjdemerits"
   "looseness"
   "pausing"
   "holdinginserts"
   "tracingonline"
   "tracingmacros"
   "tracingstats"
   "tracingparagraphs"
   "tracingpages"
   "tracingoutput"
   "tracinglostchars"
   "tracingcommands"
   "tracingrestores"
   "language"
   "nchyph"
   "lefthyphenmin"
   "righthyphenmin"
   "globaldefs"
   "defaulthyphenchar"
   "escapechar"
   "endlinechar"
   "newlinechar"
   "maxdeadcycles"
   "hangafter"
   "fam"
   "mag"
   "delimiterfactor"
   "time"
   "day"
   "month"
   "year"
   "showboxbreadth"
   "showboxdepth"
   "errorcontextlines"))

(define dimen-parameters-list
  (list
   "hfuzz"
   "vfuzz"
   "overfullrule"
   "emergencystretch"
   "hsize"
   "vsize"
   "maxdepth"
   "splitmaxdepth"
   "boxmaxdepth"
   "lineskiplimit"
   "delimiterspace"
   "scriptspace"
   "mathsurround"
   "predisplaysize"
   "displaywidth"
   "displayindent"
   "parindent"
   "hangindent"
   "hoffset"
   "voffset"))

(define glue-parameters-list
  (list
   "baselineskip"
   "lineskip"
   "parskip"
   "abovedisplayskip"
   "abovedisplayshortskip"
   "belowdisplayskip"
   "belowdisplayshortskip"
   "leftskip"
   "rightskip"
   "topskip"
   "splittopskip"
   "tabskip"
   "spaceskip"
   "xspaceskip"
   "parfillskip"))

(define mu-glue-parameters-list
  (list
   "thinmuskip"
   "medmuskip"
   "shickmuskip"))

(define int-parameters
  (apply-parser-or
   (map make-command-parser int-parameters-list)))

(define dimen-parameters
  (apply-parser-or
   (map make-command-parser dimen-parameters-list)))
   
(define glue-parameters
  (apply-parser-or
   (map make-command-parser glue-parameters-list)))

(define mu-glue-parameters
  (apply-parser-or
   (map make-command-parser mu-glue-parameters-list)))

(define (internal-int env)
  (parser-or
   int-parameters
   special-int
   (make-command-parser "lastpenalty")
   (register-token 'countdef env)
   (parser-cont (make-command-parser "count") (get-tex-int-num env))
   (parser-cont codename (get-tex-int-num env))
   (register-token 'chardef env)
   (register-token 'mathchardef env)
   (make-command-parser "parshape")
   (make-command-parser "inputlineno")
   (parser-cont (make-command-parser "hyphenchar") font)
   (parser-cont (make-command-parser "skewchar") font)
   (make-command-parser "badness")))

(define special-int
  (parser-or
   (make-command-parser "spacefactor")
   (make-command-parser "prevgraf")
   (make-command-parser "deadcycles")
   (make-command-parser "insertpenalties")))

(define codename
  (parser-or
   (make-command-parser "catcode")
   (make-command-parser "mathcode")
   (make-command-parser "lccode")
   (make-command-parser "uccode")
   (make-command-parser "sfcode")
   (make-command-parser "delcode")))

(define (font env)
  (parser-or
   (register-token 'fontdef env)
   (make-command-parser "font")
   (family-member env)))

(define (family-member env)
  (parser-cont
   font-range
   (get-tex-int-num env)))

(define font-range
  (parser-or
   (make-command-parser "textfont")
   (make-command-parser "scriptfont")
   (make-command-parser "scriptscriptfont")))

(define (internal-dimen env)
  (parser-or
   dimen-parameters
   special-dimen
   (make-command-parser "lastkern")
   (register-token 'dimendef env)
   (parser-cont (make-command-parser "dimen") (get-tex-int-num env))
   (parser-cont (make-command-parser "fontdimen") 
		(get-tex-int-num env) (font env))
   (parser-cont boxdimen (get-tex-int-num env))))

(define special-dimen
  (parser-or
   (make-command-parser "prevdepth")
   (make-command-parser "pagegoal")
   (make-command-parser "pagetotal")
   (make-command-parser "pagestretch")
   (make-command-parser "pagefilstretch")
   (make-command-parser "pagefillstretch")
   (make-command-parser "pagefilllstretch")
   (make-command-parser "pageshrink")
   (make-command-parser "pagedepth")))

(define boxdimen
  (parser-or
   (make-command-parser "ht")
   (make-command-parser "wd")
   (make-command-parser "dp")))

(define (internal-glue env)
  (parser-or
   glue-parameters
   (register-token 'skipdef env)
   (make-command-parser "lastskip")
   (parser-cont (make-command-parser "skipdef") (get-tex-int-num env))))

(define (internal-mu-glue env)
  (parser-or
   mu-glue-parameters
   (make-command-parser "lastskip")
   (register-token 'muskipdef env)
   (parser-cont (make-command-parser "muskip") (get-tex-int-num env))))

(define (register-token type env)
  (lambda (ts)
    (guard 
     (e 
      ((<error> e) (error <parser-error> "no register value"))
      (else
       (cond
	((and (integer? e) (eq? type 'mathchardef))
	 (values e (cdr ts)))
	(else
	 (error <parser-error> "no register value")))))
     (or
      (and (= -1 (cat (car ts)))
	   (find-macro-definition (token->symbol (cdar ts)) env))
      (and (= 13 (or (find-catcode (car ts) env) (cat (car ts))))
	   (find-activechar-definition (token->symbol (cdar ts)) env))))))

(provide "internal-value")