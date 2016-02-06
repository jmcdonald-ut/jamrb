;; state.rkt
;;
;; "There's a time and a place for everything, and it's called college."
;;
;; By Jonathon McDonald
#lang racket

;;
;; Identifiers
;;

(provide (contract-out [id-char-terminator? (-> char? boolean?)]))

(define (id-char-terminator? value)
  (match value
    [(or #\! #\?) #t]
    [_ #f]))


(provide (contract-out [id->token-sym (-> string? symbol?)]))

(define (id->token-sym value)
  (match (string-ref value 0)
    [(app char-upper-case? #t) 'const]
    [(app (λ (val) (equal? #\@ val)) #t) 'ivar]
    [_ 'ident]))


(provide (contract-out [id-char-invalid? (->* (char?) (boolean?) boolean?)]))

(define (id-char-invalid? value [first? #f])
  (or (and (char>? value (integer->char 0)) (char<? value #\!))
      (and (char>? value #\!) (char<? value #\0))
      (and (char>? value #\9) (char<? value #\?))
      (and (not first?) (eq? value #\@))
      (and (char>? value #\Z) (char<? value #\_))
      (eq? value #\`)
      (and (char>? value #\z) (char<? value (integer->char 127)))))
