;; abbrevs.rkt
;;
;; "You win again, gravity."
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))


;; Ruby Keywords (see: http://ruby-doc.org/core-2.3.0/doc/keywords_rdoc.html)
(provide keyword single-keyword)

(define-lex-abbrevs
  [keyword (:: (:* punct) single-keyword (:+ (:or punct whitespace)))]
  [single-keyword (:or  "__ENCODING__" "__LINE__" "__FILE__" "BEGIN" "END"
                        "alias" "and" "begin" "break" "case" "class" "def"
                        "defined?" "do" "else" "elsif" "end" "ensure" "false"
                        "for" "if" "in" "module" "nil" "not" "or" "redo"
                        "rescue" "retry" "return" "self" "super" "then" "true"
                        "undef" "unless" "until" "when" "while" "yield")])


;; Ruby Comments
(provide comment)

(define-lex-abbrev comment (:: (:+ "#")
                               (:* (:- any-char #\newline))
                               (:? #\newline)))


;; Newlines & Whitespace
(provide newlines space)

(define-lex-abbrevs
  [newlines (:+ #\newline)]
  [space (:+ (:- whitespace #\newline))])


;; Operations
(provide op unary-op single-unary-op spaced-op
         spaced-op-char id-op-seq id-op def-op)

(define-lex-abbrevs
  [spaced-op (:: (:+ whitespace) spaced-op-char (:+ whitespace))]
  [spaced-op-char (:or #\? #\:)]

  [id-op-seq (:: (:- id-start whitespace op)
                 (:* (:- any-char whitespace op id-op))
                 id-op)]
  [id-op "[]"]

  [def-op (:: "def" space id-op (:* space) (:or #\( (:+ whitespace)))])

(define-lex-abbrev unary-op (:: single-unary-op (:or whitespace #\( #\newline)))
(define-lex-abbrev single-unary-op (:or "-@" "+@" "~@" "!@"))
(define-lex-abbrev op (:or #\! #\~ #\+ #\- #\* #\/ #\% #\& #\> #\< #\=
                           #\| #\^ "**" ">>" "<<" "&&" "||" ".." "..."
                           "<=>" "==" "===" "!=" "=~" "!~" "+=" "-="
                           "*=" "/=" "^=" "%=" "**=" ">=" "<=" "&&="
                           "||=" "|=" "<<=" ">>=" "[]=" "::"))


;; Identifiers
(provide id-start)

(define-lex-abbrev id-start (:- any-char #\{ #\} #\[ #\] #\< #\> #\( #\)))


;; Numbers
(provide int-literal float-literal)

(define-lex-abbrevs
  [int-literal (:or bin-number oct-number ten-number hex-number)]

  [bin-number (:: bin-prefix (:+ bin-digit))]
  [bin-prefix (:: #\0 (:or #\b #\B))]
  [bin-digit (:or #\0 #\1)]

  [oct-number (:: oct-prefix (:+ oct-digit))]
  [oct-prefix (:: #\0 (:or #\o #\O))]
  [oct-digit (:or bin-digit #\2 #\3 #\4 #\5 #\6 #\7 #\8)]

  [hex-number (:: hex-prefix (:+ hex-digit))]
  [hex-prefix (:: #\0 (:or #\x #\X))]
  [hex-digit (:or oct-digit #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F)]

  [dec-prefix (:: #\0 (:or #\d #\D))]

  [ten-number (:: (:? (:: #\0 (:or #\d #\D))) (:+ ten-digit))]
  [ten-digit (:or oct-digit #\9)]

  [float-literal (:or float-pt float-exp)]
  [float-pt (:or (:: int-part fraction-part))]
  [float-exp (:: (:or float-pt int-part) exponent)]

  [fraction-part (:: #\. int-part)]

  [int-part (:+ ten-digit)]

  [exponent (:: (:or #\e #\E) (:** 0 1 (:or #\+ #\-)) int-part)])


;; Symbols
(provide symbeg)

(define-lex-abbrev symbeg (:or #\: (:: #\: #\") (:: #\: #\')))


;; Punctuation
(provide punct)

(define-lex-abbrev punct (:or #\. #\, #\; #\( #\) #\{ #\} #\[ #\]))


;; Heredocs
(provide heredoc-beg heredoc-interp heredoc-no-interp
         heredoc-prefix-indent heredoc-prefix-no-indent)

(define-lex-abbrevs
  [heredoc-beg (:: hdoc-prefix (:or hdoc-id hdoc-id-quoted))]

  ;; Captures whether the heredoc is interpolated.
  [heredoc-interp (:: (:or hdoc-id hdoc-id-btick hdoc-id-dbl))]
  [heredoc-no-interp (:: hdoc-id-single)]

  ;; Captures whether the heredoc is whitespace indifferent for terminator.
  [heredoc-prefix-indent (:: "<<" hdoc-indent)]
  [heredoc-prefix-no-indent "<<"]

  [hdoc-id-quoted (:or hdoc-id-single hdoc-id-dbl hdoc-id-btick)]
  [hdoc-id-single (:: #\' hdoc-id #\')]
  [hdoc-id-dbl (:: #\" hdoc-id #\")]
  [hdoc-id-btick (:: #\` hdoc-id #\`)]
  [hdoc-id (:+ (:- any-char #\< whitespace punct hdoc-quotes))]

  [hdoc-prefix (:: "<<" (:? hdoc-indent))]
  [hdoc-indent (:or "-" "~")]
  [hdoc-quotes (:or #\' #\" #\`)])


;; Regex
(provide rx-style rx-mark-end rx-mark)

(define-lex-abbrevs
  [rx-style (:: rx-mark (:* any-char) rx-mark-end)]
  [rx-mark-end (:: rx-mark (:* rx-delimiter))]
  [rx-mark #\/]
  [rx-delimiter (:or #\i #\m #\x #\o #\u #\e #\s #\n)])


;; Strings
(provide str-beg backtick)

(define-lex-abbrev str-beg (:or str-dbl str-single))
(define-lex-abbrev backtick "`")


(define-lex-abbrevs
  [str-dbl (:or #\" per-dbl)]
  [str-single (:or #\' per-single)]
  [per-dbl (:: "%Q" (:or #\( #\< #\{ #\[))]
  [per-single (:: "%q" (:or #\( #\< #\{ #\[))])


;; Embedded Expressions
(provide embexpr embexpr-end)

(define-lex-abbrevs
  [embexpr (:: #\# #\{)]
  [embexpr-end (:: #\})])
