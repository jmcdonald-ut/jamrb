;; identifiers.rkt
;;
;; "There's a time and a place for everything, and it's called college."
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide identifier)

(define-lex-abbrev identifier (:: (:+ (:or #\_ lower-case upper-case numeric))))
