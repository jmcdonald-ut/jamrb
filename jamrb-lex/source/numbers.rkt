;; numbers.rkt
;;
;; "And my wolf pack... it grew by one."
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide int-literal float-literal)

;; Integer Lexer Patterns
(define-lex-abbrev int-literal (:or bin-number oct-number ten-number hex-number))

(define-lex-abbrev bin-number (:: bin-prefix (:+ bin-digit)))
(define-lex-abbrev bin-prefix (:: #\0 (:or #\b #\B)))
(define-lex-abbrev bin-digit (:or #\0 #\1))

(define-lex-abbrev oct-number (:: oct-prefix (:+ oct-digit)))
(define-lex-abbrev oct-prefix (:: #\0 (:or #\o #\O)))
(define-lex-abbrev oct-digit (:or bin-digit #\2 #\3 #\4 #\5 #\6 #\7 #\8))

(define-lex-abbrev hex-number (:: hex-prefix (:+ hex-digit)))
(define-lex-abbrev hex-prefix (:: #\0 (:or #\x #\X)))
(define-lex-abbrev hex-digit (:or oct-digit #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F))

(define-lex-abbrev dec-prefix (:: #\0 (:or #\d #\D)))

(define-lex-abbrev ten-number (:: (:? (:: #\0 (:or #\d #\D))) (:+ ten-digit)))
(define-lex-abbrev ten-digit (:or oct-digit #\9))

;; Float Lexer Patterns
(define-lex-abbrev float-literal (:or float-pt float-exp))
(define-lex-abbrev float-pt (:or (:: int-part fraction-part) (:: int-part #\.)))
(define-lex-abbrev float-exp (:: (:or float-pt int-part) exponent))

(define-lex-abbrev fraction-part (:: #\. int-part))

(define-lex-abbrev int-part (:+ ten-digit))

(define-lex-abbrev exponent (:: (:or #\e #\E) (:** 0 1 (:or #\+ #\-)) int-part))
