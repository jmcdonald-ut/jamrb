;; symbols.rkt
;;
;; "Just shut up and take my money!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide symbeg)

;; Defines a lexer abbreviation for a symbol beginning.
(define-lex-abbrev symbeg (:or #\: (:: #\: #\")))
