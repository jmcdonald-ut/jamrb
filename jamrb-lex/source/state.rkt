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


;;
;; Tracking
;;

(define method-tracking (make-hash))
(hash-set*! method-tracking
            'spotted? #f
            'with-parens? #f)

(provide (contract-out [track-def! (->* () (boolean?) boolean?)]))

(define (track-def! [spotted? #t])
  (hash-set! method-tracking 'spotted? spotted?)
  spotted?)


(provide (contract-out [def-tracked? (-> boolean?)]))

(define (def-tracked?)
  (hash-ref method-tracking 'spotted?))


(provide (contract-out [def-tracked-with-parens? (-> boolean?)]))

(define (def-tracked-with-parens?)
  (hash-ref method-tracking 'with-parens?))


(provide (contract-out [track-def-with-parens! (->* () (boolean?) boolean?)]))

(define (track-def-with-parens! [with-parens? #f])
  (hash-set! method-tracking 'with-parens? with-parens?)
  with-parens?)


(provide
 (contract-out [reset-method-tracking! (->* () (boolean? boolean?) any/c)]))

(define (reset-method-tracking! [spotted? #f] [with-parens? #f])
  (hash-set*! method-tracking
              'spotted? spotted?
              'with-parens? with-parens?))
