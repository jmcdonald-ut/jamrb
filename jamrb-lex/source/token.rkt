;; token.rkt
;;
;; "I must find a way to escape the horrible ravages of youth."
;;
;; By Jonathon McDonald
#lang racket

(provide tokenize)

(define (normalize-key key) (string->symbol (string-append "on_" (symbol->string key))))

(define (tokenize line col key value)
  `(Token ((,line ,col)
           ,(normalize-key key)
           ,value)))
