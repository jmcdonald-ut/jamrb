; ripper-lex.rkt
; By Jonathon McDonald
#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "ripper-lex-token.rkt")

(define (ruby-symbol-string->symbol str)
  (string->symbol (regexp-replace* #px"[\\s,:]*" str "")))

(define (pair-line-column-string->pair str)
  (map string->number (regexp-match* #rx"[0-9]+" str)))

(define (value->string str)
  ; This will probably need to be changed to something more sturdy...but for now this will work.
  (define (convert-to-newlines str)
    (regexp-replace* #px"\\\\n" str "\n"))
  
  (define (convert-quotes str)
    (regexp-replace* #rx"\\\\\"" str "\""))
  
  (define (convert-escapes str)
    (regexp-replace* #rx"\\\\\\" str "\\"))
  
  (define (convert* str)
    (convert-escapes (convert-quotes (convert-to-newlines str))))
  
  (convert* (substring str 1 (- (string-length str) 1))))


(define (unget port [count 1])
  (let ([pos (file-position port)])
    (if (> pos (- 1 count))
        (file-position port (- pos count))
        (void))))

(define (token-lexer port)
  (define internal-lexer
    (lexer
     [pair-line-column
      (cons (pair-line-column-string->pair lexeme)
            (token-lexer port))]

     [token-type
      (cons (ruby-symbol-string->symbol lexeme)
            (token-lexer port))]

     [value (cons (value->string lexeme) (token-lexer port))]

     [(:or #\[ #\, whitespace #\newline) (token-lexer port)]

     [(:or (:: #\] (:* whitespace) #\,) #\]) '()]

     [#\newline (token-lexer port)]))

  (internal-lexer port))

(define (ripper-lexer port [lst #f])
  (define inner-lexer
    (lexer
     [ripper-token
      (begin
        (unget port (string-utf-8-length lexeme))
        (let ([token (token-lexer port)])
          (cons `(Token ,token) (inner-lexer port))))]

     [#\[
      (if lst
          (cons (ripper-lexer port '()) lst)
          (ripper-lexer port '()))]

     [#\] '()]

     [(:or whitespace #\newline) (inner-lexer port)]

     [#\, (inner-lexer port)]

     [(eof) '()]))

  (inner-lexer port))

(define (sanitized-input)
  (open-input-string (port->string (current-input-port))))

(pretty-write (ripper-lexer (sanitized-input)))
