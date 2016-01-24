;; numbers.rkt
;;
;; "And my wolf pack... it grew by one."
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide int-literal
         float-literal)

;; Defines the lexer abbreviation(s) for integer literals.
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

;; Defines the lexer abbreviation(s) for float literals.
(define-lex-abbrevs
  [float-literal (:or float-pt float-exp)]
  [float-pt (:or (:: int-part fraction-part) (:: int-part #\.))]
  [float-exp (:: (:or float-pt int-part) exponent)]

  [fraction-part (:: #\. int-part)]

  [int-part (:+ ten-digit)]

  [exponent (:: (:or #\e #\E) (:** 0 1 (:or #\+ #\-)) int-part)])
