;; port.rkt
;;
;; "I'd show you but it's just a bunch of zeroes and ones."
;;
;; port.rkt
#lang racket

(provide unget)

; (port, count?) -> port
(define (unget port [count 1])
  (let ([pos (file-position port)])
    (if (> pos (- 1 count))
        (file-position port (- pos count))
        (void)))

  port)
