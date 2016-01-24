;; port.rkt
;;
;; "I'd show you but it's just a bunch of zeroes and ones."
;;
;; port.rkt
#lang racket

(provide unget
         unget-and-set-position!
         watch-port-position!
         prepare-port-rewinder)

;; (port, number?) -> port
;;
;; Rewinds the position of the port back by `count` bytes.  Returns the port.
(define (unget port [count 1])
  (let ([pos (file-position port)])
    (if (> pos (- 1 count))
        (file-position port (- pos count))
        (void)))

  port)

;; (port, number, number, number?) -> port
;;
;; Sets the line, column, and file-position of the current token before rewinding to the position
;; specified by `count`.  Returns the port.
(define (unget-and-set-position! port line col [count 1])
  (set-port-next-location! port line col (file-position port))
  (unget port count)

  port)

;; (port) -> [number, number]
;;
;; Ensures that the port is enabled for line and column counting, and then returns the line and
;; column of the next location in the port.
(define (watch-port-position! port)
  (port-count-lines! port)
  (let-values ([(line col _) (port-next-location port)])
    (values line col)))

;; (port, number, number) -> fn:(number?)
;;
;; Prepares a function that will call `unget-and-set-position!` using the provided port, line, and
;; column.
(define (prepare-port-rewinder port line col)
  (λ ([length 1])
    (unget-and-set-position! port line col length)))
