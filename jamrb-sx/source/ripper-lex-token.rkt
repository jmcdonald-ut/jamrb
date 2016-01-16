; ripper-lex-token.rkt
; By Jonathon McDonald
#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-lex-abbrev pair-line-column 
  (:: (:= 1 #\[) 
      (:+ numeric) 
      (:* whitespace) 
      (:= 1 #\,) 
      (:* whitespace) 
      (:+ numeric)
      (:= 1 #\])))

(define-lex-abbrev token-type
  (:: (:= 1 #\:)
      (:+ (:: (:+ alphabetic) (:= 1 #\_) (:+ alphabetic)))))

(define-lex-abbrev value
  (:: (:= 1 #\")
      (:* (:or (:& any-char (complement #\")) (:: #\\ #\")))
      (:= 1 #\")))

(define-lex-abbrev ripper-token
  (:: (:= 1 #\[)
      (:: (:* whitespace) (:= 1 pair-line-column) (:* whitespace) (:= 1 #\,))
      (:: (:* whitespace) (:= 1 token-type) (:* whitespace) (:= 1 #\,))
      (:: (:* whitespace) (:= 1 value) (:* whitespace))
      (:= 1 #\])))