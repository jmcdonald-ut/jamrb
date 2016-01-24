;; token.rkt
;;
;; "I must find a way to escape the horrible ravages of youth."
;;
;; By Jonathon McDonald
#lang racket
(provide tokenize 
         prepare-tokenizer
         prepare-tokenizer-with-callback)

;; (number, number, symbol, string) -> '()
;;
;; Returns a list that represents a token. A token includes the line and column, the key which 
;; identifies it, and the value as a string.
(define (tokenize line col key value)
  `(Token ((,line ,col)
           ,(normalize-key key)
           ,value)))

;; (symbol) -> symbol
;;
;; Returns the given symbol prefixed with "on_".
(define (normalize-key key) 
  (string->symbol (string-append "on_" (symbol->string key))))

;; -> fn:(number, number, symbol, string, fn)
;;
;; Prepares a function that will return a pair with the first value being equal to the newly created 
;; token and the second value being equal to the results of `callback`.
(define (prepare-tokenizer)
  (λ (line col key value callback)
    (cons (tokenize line col key value)
          (callback))))

;; (fn) -> fn:(number, number, symbol, string)
;;
;; Prepares a function that will return a pair with the first value being equal to the newly created 
;; token and the second value being equal to the results of `callback`.
(define (prepare-tokenizer-with-callback callback)
  (λ (line col key value)
    (cons (tokenize line col key value)
          (callback))))
