;; abbrevs.rkt
;;
;; "You win again, gravity."
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide operation int-literal float-literal symbeg punct heredoc-beg
         string-opening embexpr)

(define-lex-abbrev operation (:or #\! #\~ #\+ #\- #\* #\/ #\% #\& #\> #\< #\=
                                  #\| #\^ "**" ">>" "<<" "&&" "||" ".." "..."
                                  "<=>" "==" "===" "!=" "=~" "!~" "+=" "-="
                                  "*=" "/=" "^=" "%=" "**=" ">=" "<=" "&&="
                                  "||=" "|=" "<<=" ">>=" "-@" "+@" "~@" "!@"
                                  "[]" "[]="))

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
  [ten-digit (:or oct-digit #\9)])

(define-lex-abbrevs
  [float-literal (:or float-pt float-exp)]
  [float-pt (:or (:: int-part fraction-part) (:: int-part #\.))]
  [float-exp (:: (:or float-pt int-part) exponent)]

  [fraction-part (:: #\. int-part)]

  [int-part (:+ ten-digit)]

  [exponent (:: (:or #\e #\E) (:** 0 1 (:or #\+ #\-)) int-part)])

(define-lex-abbrev symbeg (:or #\: (:: #\: #\") (:: #\: #\')))

(define-lex-abbrev punct (:or #\. #\, #\( #\) #\{ #\} #\[ #\]))

(define-lex-abbrev heredoc-beg (:: "<<" (:? (:or #\- #\~))
                                   (:- any-char (:or #\{ #\} #\[ #\] #\<
                                                     #\> #\( #\space #\=))
                                   (:* (:- any-char #\newline))))

;; Defines the lexer abbreviation for a string opening.
;;
;; TO-DO: Support %q, %Q.
(define-lex-abbrev string-opening (:or str-dbl str-single))

;; Defines the individual lexer abbreviations for string.
(define-lex-abbrevs
  [str-dbl (:or #\" per-dbl)]
  [str-single (:or #\' per-single)]
  [per-dbl (:: "%Q" (:or #\( #\< #\{ #\[))]
  [per-single (:: "%q" (:or #\( #\< #\{ #\[))]
  [embexpr (:: #\# #\{)])
