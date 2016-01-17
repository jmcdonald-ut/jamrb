;; jamr-lex.rkt
;;
;; "Good news, everyone!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         "misc.rkt"
         "numbers.rkt"
         "token.rkt")

(define input (void))
(define test-input "foo = true")

;; `jamrb-lex` performs lexical anlaysis on some sort of input stream.  Upon finishing this will 
;; return either a list of tokens, or an error will be emitted.
(provide jamrb-lex)
(define (jamrb-lex port)
  ; Explicitally enable line/column reporting for the given port.
  (port-count-lines! port)
  
  ; Extract the line and column for debugging purposes.
  (define line #f)
  (define col #f)
  (let-values ([(port-line port-col _) (port-next-location port)])
    (set! line port-line)
    (set! col port-col))

  ; Define our lexer.
  (define lex
    (lexer
     [comment (handle-comment line col lexeme)]
     [space (handle-space line col lexeme)]
     [(eof) '()]))
  
  ; Handles a matched comment.
  (define (handle-comment line col lexeme) 
    (cons (tokenize line col 'comment lexeme) (jamrb-lex port)))
  
  ; Handles a matched arbitrary amount of space.
  (define (handle-space line col lexeme)
    (cons (tokenize line col 'sp lexeme) (jamrb-lex port)))
  
  ; Return the result of lexically analyzing the given port.
  (lex port))

; Read in a file, code, or use the variable test input.
(match (current-command-line-arguments)
  ((vector "-n")
   (set! input (current-input-port)))
  ((vector (or "--test" "--drracket"))
   (set! input test-input))
  ((vector file-name)
   (set! input (open-input-file file-name)))
  ((vector) (set! input (current-input-port))))

(set! input (open-input-string (port->string input)))
(define tokens (jamrb-lex input))
(pretty-print tokens)