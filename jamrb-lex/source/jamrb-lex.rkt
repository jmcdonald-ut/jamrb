;; jamr-lex.rkt
;;
;; "Good news, everyone!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         "misc.rkt"
         "numbers.rkt"
         "operations.rkt"
         "keywords.rkt"
         "identifiers.rkt"
         "strings.rkt"
         "port.rkt"
         "token.rkt")

(define input (void))
(define test-input "foo = true")

;; `jamrb-lex` performs lexical anlaysis on an input stream.  Upon finishing this will
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
  
  (define (rewind length)
    (unget-and-set-position! port line col length))

  ; Define our lexer.
  (define lex
    (lexer
     [comment (tok-con line col 'comment lexeme)]
     [newlines (newline-lex (rewind (string-length lexeme)) jamrb-lex)]
     [space (tok-con line col 'sp lexeme)]
     [int-literal (tok-con line col 'int lexeme)]
     [float-literal (tok-con line col 'float lexeme)]
     [operation (tok-con line col 'op lexeme)]
     [keyword (tok-con line col 'kw lexeme)]
     [period (tok-con line col 'period lexeme)]
     [identifier (tok-con line col 'ident lexeme)]
     [string-opening (string-lex port lexeme line col jamrb-lex)]
     [(eof) '()]))

  ; Tokenizes the value and continues lexical analysis.
  (define (tok-con line col key lexeme)
    (cons (tokenize line col key lexeme) (jamrb-lex port)))

  ; Return the result of lexically analyzing the given port.
  (with-handlers ([exn:fail?
                   (λ (e)
                     (string-append "Invalid syntax at ("
                                    (number->string line)
                                    ", "
                                    (number->string col)
                                    ")"))])
    (lex port)))

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
(pretty-write tokens)
