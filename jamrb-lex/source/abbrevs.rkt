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
  [keyword (:: single-keyword (:or #\. #\( #\) #\[ #\] whitespace))]
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
(provide op unary-op single-unary-op)

(define-lex-abbrev unary-op (:: single-unary-op (:or whitespace #\( #\newline)))
(define-lex-abbrev single-unary-op (:or "-@" "+@" "~@" "!@"))
(define-lex-abbrev op (:or #\! #\~ #\+ #\- #\* #\/ #\% #\& #\> #\< #\=
                           #\| #\^ "**" ">>" "<<" "&&" "||" ".." "..."
                           "<=>" "==" "===" "!=" "=~" "!~" "+=" "-="
                           "*=" "/=" "^=" "%=" "**=" ">=" "<=" "&&="
                           "||=" "|=" "<<=" ">>=" "[]" "[]=" "::"))


;; Identifiers
(provide id-start)

(define-lex-abbrev id-start (:- any-char (:or #\{ #\} #\[ #\] #\< #\> #\( #\))))


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
(provide heredoc-beg)

(define-lex-abbrev heredoc-beg (:: "<<" (:? (:or #\- #\~))
                                   (:- any-char (:or #\{ #\} #\[ #\] #\<
                                                     #\> #\( #\space #\=))
                                   (:* (:- any-char #\newline))))


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
