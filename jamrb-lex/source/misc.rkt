;; misc.rkt
;;
;; "With my last breath, I curse Zoidberg!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "port.rkt"
         "token.rkt")

(provide comment newlines newline-lex space period punct punct->symbol)

; Comments:
(define-lex-abbrev comment (:: (:+ "#")
                               (:* (:- any-char #\newline))
                               (:? #\newline)))

; Newlines:
(define-lex-abbrev newlines (:+ #\newline))

(define (newline-lex port callback [first? #t])
  ; Explicitally enabled line/column reporting for the given port.
  (port-count-lines! port)

  ; Extract the line and column for debugging purposes.
  (define line #f)
  (define col #f)
  (let-values ([(port-line port-col _) (port-next-location port)])
    (set! line port-line)
    (set! col port-col))
  
  (define (rewind)
    (unget-and-set-position! port line col))

  ; Internal lexer that should only be called directly at the end of the function.
  (define internal-lex
    (lexer
     [#\newline (tok-con line col (if first? 'nl 'ignored_nl) lexeme)]
     [any-char (callback (rewind))]
     [(eof) '()]))

  ; (number, number, symbol, string) -> '()
  (define (tok-con line col key lexeme)
    (cons (tokenize line col key lexeme) (newline-lex port callback #f)))

  ; Begin lexically analyzing new lines.
  (internal-lex port))

; Punctuation:
(define-lex-abbrev punct (:or #\. #\, #\( #\) #\{ #\} #\[ #\]))
(define punct-symbol-ht (make-hash))
(hash-set*! punct-symbol-ht 
            "." 'period
            "," 'comma
            "(" 'lparen
            ")" 'rparen
            "{" 'lbrace
            "}" 'rbrace
            "[" 'lbracket
            "]" 'rbracket)

(define (punct->symbol value) (hash-ref punct-symbol-ht value))

; Periods:
(define-lex-abbrev period (:: #\.))

; Spaces:
(define-lex-abbrev space (:+ (:- whitespace #\n)))
