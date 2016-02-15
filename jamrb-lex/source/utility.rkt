;; utilities.rkt
;;
;; "You tried your best and you failed miserably. The lesson is never try."
;;
;; By Jonathon McDonald
#lang racket
(require "state.rkt")

;;
;; Token Utilities
;;

(provide
 (contract-out
  [tokenize (->* (exact-integer? exact-integer? symbol? string?) list?)]))

(define (tokenize line col key value)
  (add-lexed-token! `(Token ((,line ,col) ,(normalize-token-key key) ,value))))


(provide
 (contract-out
  [tokenize-cons
   (-> exact-integer? exact-integer? symbol? string? procedure? pair?)]))

(define (tokenize-cons line col key value callback)
  (cons (tokenize line col key value)
        (callback)))


(provide (contract-out [normalize-token-key (-> symbol? symbol?)]))

(define (normalize-token-key key)
  (string->symbol (string-append "on_" (symbol->string key))))


(provide
 (contract-out
  [tokenize-punct! (-> exact-integer? exact-integer? string? list?)]))

(define (tokenize-punct! line col value)
  (if (punct-is-stackable? value)
      (tokenize-stackable-punct! line col value)
      (tokenize line col (punct->symbol value) value)))


(provide
 (contract-out
  [tokenize-stackable-punct! (-> exact-integer? exact-integer? string? list?)]))

(define (tokenize-stackable-punct! line col value)
  (if (punct-is-open? value)
      (push-punct! value)
      (pop-punct-pair! value))

  (track-def-with-parens! (def-tracked?))
  (tokenize line col (punct->symbol value) value))


(provide (contract-out [tokenize-string-contents! (-> procedure? list?)]))

(define (tokenize-string-contents! callback)
  (define-values (contents sline scol) (reset-string-contents!))
  (if (> (string-length contents) 0)
      (tokenize-cons sline scol 'tstring_content contents callback)
      (callback)))


;;
;; Port Utilities
;;

(provide (contract-out [unget (->* (port?) (exact-positive-integer?) port?)]))

(define (unget port [count 1])
  (define pos (file-position port))
  (cond [(> pos (- 1 count)) (file-position port (- pos count))])

  port)


(provide (contract-out [forward (->* (port?) (exact-integer?) port?)]))

(define (forward port [count 1])
  (read-string count port)
  port)


(provide
 (contract-out
  [unget-with-position! (->* (port? exact-integer? exact-integer?)
                             (exact-positive-integer?)
                             port?)]))

(define (unget-with-position! port line col [count 1])
  (if (> (file-position port) 0)
      (begin
        (set-port-next-location! port line col (file-position port))
        (unget port count))
      port))


(provide
 (contract-out
  [watch-port-position! (-> port? (values exact-integer? exact-integer?))]))

(define (watch-port-position! port)
  (port-count-lines! port)
  (let-values ([(line col total) (port-next-location port)])
    (pretty-write (format "Total read: ~a" total))
    (values line col)))


(provide
 (contract-out
  [prepare-port-rewinder
   (-> port? exact-positive-integer? exact-integer? procedure?)]))

(define (prepare-port-rewinder port line col)
  (curry unget-with-position! port line col))


;;
;; Type Utilities
;;

(provide (contract-out [string->char (-> string? char?)]))

(define (string->char str)
  (car (string->list str)))


;;
;; String/Heredoc Utilities
;;

(provide (contract-out [end-newline? (->* (string?) (boolean?) boolean?)]))

(define (end-newline? str [ignore-spaces? #f])
  (define sanitized-str (if ignore-spaces? (string-trim str #px"[ \t]*") str))
  (define (last-char-nl?)
    (eq? (car (reverse (string->list sanitized-str))) #\newline))

  (define not-empty? (> (string-length sanitized-str) 0))
  (and not-empty? (last-char-nl?)))


(provide (contract-out [string->nl-tail (-> string? string?)]))

(define (string->nl-tail str)
  (define (end-nl?)
    (and (> (string-length str) 0) (eq? (last (string->list str)) #\newline)))

  (define arr (string-split str "\n"))
  (if (or (eq? (length arr) 1) (eq? (length arr) 0) (end-nl?)) "" (last arr)))


(provide (contract-out [escape-embed (-> string? string?)]))

(define (escape-embed str)
  (regexp-replace* #rx"(#{)" str "\\\\#{"))
