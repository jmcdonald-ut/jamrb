;; port.rkt
;;
;; "I'd show you but it's just a bunch of zeroes and ones."
;;
;; port.rkt
#lang racket

(provide unget unget-and-set-position!)

; (port, count?) -> port
(define (unget port [count 1])
  (let ([pos (file-position port)])
    (if (> pos (- 1 count))
        (file-position port (- pos count))
        (void)))

  port)

; (port, number, number, number?) -> port
(define (unget-and-set-position! port line col [count 1])
  (set-port-next-location! port line col (file-position port))
  (unget port count)
  port)