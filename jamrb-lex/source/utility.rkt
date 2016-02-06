;; utilities.rkt
;;
;; "You tried your best and you failed miserably. The lesson is never try."
;;
;; By Jonathon McDonald
#lang racket
(provide tokenize tokenize-cons normalize-token-key
         unget unget-with-position! watch-port-position! prepare-port-rewinder)

;;
;; Token Utilities
;;

(define (tokenize line col key value)
  (unless (number? line)
    (raise-argument-error 'tokenize "number?" line))
  (unless (number? col)
    (raise-argument-error 'tokenize "number?" col))
  (unless (symbol? key)
    (raise-argument-error 'tokenize "symbol?" key))
  (unless (string? value)
    (raise-argument-error 'tokenize "string?" value))

  `(Token ((,line ,col) ,(normalize-token-key key) ,value)))

(define (tokenize-cons line col key value callback)
  (unless (procedure? callback)
    (raise-argument-error 'tokenize-cons "procedure?" callback))

  (cons (tokenize line col key value) (callback)))

(define (normalize-token-key key)
  (unless (symbol? key)
    (raise-argument-error 'normalize-token-key "symbol?" key))
  (string->symbol (string-append "on_" (symbol->string key))))

;;
;; Port Utilities
;;

(define (unget port [count 1])
  (unless (port? port)
    (raise-argument-error 'unget "port?" port))
  (unless (exact-positive-integer? count)
    (raise-argument-error 'unget "exact-positive-integer?" count))

  (define pos (file-position port))
  (cond [(> pos (- 1 count)) (file-position port (- pos count))])

  port)

(define (unget-with-position! port line col [count 1])
  (unless (port? port)
    (raise-argument-error 'unget-with-position! "port?" port))
  (unless (exact-positive-integer? line)
    (raise-argument-error 'unget-with-position! "exact-positive-integer?" line))
  (unless (exact-integer? col)
    (raise-argument-error 'unget-with-position! "exact-integer?" col))
  (unless (exact-positive-integer? count)
    (raise-argument-error
     'unget-with-position! "exact-positive-integer?" count))

  (if (> (file-position port) 0)
      (begin
        (set-port-next-location! port line col (file-position port))
        (unget port count))
      port))

(define (watch-port-position! port)
  (port-count-lines! port)
  (let-values ([(line col _) (port-next-location port)])
    (values line col)))

(define (prepare-port-rewinder port line col)
  (unless (port? port)
    (raise-argument-error 'preapre-port-rewinder "port?" port))
  (unless (exact-positive-integer? line)
    (raise-argument-error
     'prepare-port-rewinder "exact-positive-integer?" line))
  (unless (exact-integer? col)
    (raise-argument-error 'prepare-port-rewinder "exact-integer?" col))

  (curry unget-with-position! port line col))
