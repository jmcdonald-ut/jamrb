;; symbols.rkt
;;
;; "Just shut up and take my money!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "strings.rkt"
         "utility.rkt")

(provide symbeg
         handle-sym)

;; Defines a lexer abbreviation for a symbol beginning.
(define-lex-abbrev symbeg (:or #\: (:: #\: #\") (:: #\: #\')))

;; (number, number, string, port, fn) -> '()
;;
;; Tokenizes the symbol beginning and then passes the port to the proper lexer.
(define (handle-sym line col value port callback)
  (define token (tokenize line col 'symbeg value))
  (match value
    [":\"" (cons token (string-lex-no-open port "\"" callback))]
    [":'" (cons token (string-lex-no-open port "'" callback))]
    [_ (cons token (callback port))]))
