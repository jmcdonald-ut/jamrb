;; strings.rkt
;;
;; "Well I'm sorry, the world isn't one big liberal arts college campus!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "port.rkt"
         "token.rkt")

(provide string-opening string-lex)

; TO-DO: Implement support for %q(, %q{, %q[, %Q(, %Q[, %Q{...
(define-lex-abbrev string-opening (:or str-dbl str-single))
(define-lex-abbrev str-dbl (:or #\"))
(define-lex-abbrev str-single (:or #\'))

;; General strategy for handling strings
;;
;;   1. Store String Open Value
;;   2. Return pair (string_open, rest...)
;;     2a. Create new list for contents.
;;     2b. For every character that is not equivalent to string_open, push to list.
;;     2c. If character is equivalent to string_open
;;       |   Not Escaped: Return pair (string_contents, rest...)
;;       |   Else: push to list, jump to step 2b.
;;     2d. Return pair (string_close, rest...) where rest is result of callback.

; (number, number, symbol, string, func) -> '()
(define (tok-con line col key value callback)
  (cons (tokenize line col key value) (callback)))

; (string, number?) -> char
(define (char-at value [index 0]) (list-ref (string->list value) index))

; (io, string, func) -> '()
(define (string-lex port opening line col callback)

  ; TO-DO: Steps 1, 2
  (tok-con line col 'tstring_beg opening
        ; -> '()
        (λ ()
          (internal-string-lex port #f #f "" opening callback))))


(define (internal-string-lex port sline scol contents opening callback)
  ; Explicitally enable line/column reporting for the given port.
  (port-count-lines! port)

  ; Extract the line and column given the current port.
  (define line #f)
  (define col #f)
  (let-values ([(port-line port-col _) (port-next-location port)])
    (cond [(false? sline) (set! sline port-line)])
    (cond [(false? scol) (set! scol port-col)])
    (set! line port-line)
    (set! col port-col))

  (define (complete-string)
    (tok-con line col 'tstring_end opening (λ () (callback port))))

  ; (string) -> void
  (define (append-char-unless-opening str)
    (if (equal? str opening)
        (tok-con sline scol 'tstring_content contents complete-string)
        (internal-string-lex port sline scol (string-append contents str) opening callback)))

  (define internal-lex
    (lexer
     [any-char (append-char-unless-opening lexeme)]
     [(eof) '()]))

  (internal-lex port))
