;; jamr-lex.rkt
;;
;; "Good news, everyone!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         racket/trace
         "misc.rkt"
         "numbers.rkt"
         "operations.rkt"
         "keywords.rkt"
         "identifiers.rkt"
         "strings.rkt"
         "symbols.rkt"
         "port.rkt"
         "punct.rkt"
         "token.rkt")

(provide jamrb-lex)

(define input (void))
(define test-input "foo = true")

;; (port, fn?) -> '() || string
;;
;; Lexically analyzes the given I/O port against the ruby syntax.
;;
;; Returns a list of tokens in the order they were observed once it reaches the EOF.  If a terminator 
;; character and callback are supplied then upon reaching that character the callback will be invoked 
;; with the port.
(define (jamrb-lex port [embexpr-callback #f])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))
  (define-lex-abbrev lex-term terminator)

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
     [punct (handle-punct! line col lexeme)]
     [string-opening (string-lex port lexeme line col jamrb-lex)]
     [symbeg (handle-sym line col lexeme port jamrb-lex)]
     [embexpr-end (cons (tokenize line col 'embexpr_end lexeme) (embexpr-callback port))]
     [id-start (id-lex (rewind (string-utf-8-length lexeme)) 
                       (λ (port) (jamrb-lex port embexpr-callback)))]
     [(eof) '()]))
  
  (define (handle-punct! line col lexeme)
    (if (embexpr-terminator? lexeme)
        (cons (tokenize line col 'embexpr_end lexeme) (embexpr-callback port))
        (cons (tokenize-punct! line col lexeme) (jamrb-lex port embexpr-callback))))

  ; Tokenizes the value and continues lexical analysis.
  (define (tok-con line col key lexeme)
    (cons (tokenize line col key lexeme)
          (jamrb-lex port embexpr-callback)))

  ; Return the result of lexically analyzing the given port.
  (with-handlers ([exn:fail? (λ (e) (fail-for-invalid-syntax line col))])
    (lex port)))

;; (number, number) -> string
;;
;; Returns a string indicating that there was invalid syntax.
(define (fail-for-invalid-syntax line col)
  (format "InvalidSyntaxError at (~a, ~a)." line col))

;; Parse command line arguments, and set `input` based on conditions set.
(match (current-command-line-arguments)
  ((vector "-n") (set! input (current-input-port)))
  ((vector (or "--test" "--drracket")) (set! input test-input))
  ((vector file-name) (set! input (open-input-file file-name)))
  ((vector) (set! input (current-input-port))))

;; Massage the input into a port.
(set! input (open-input-string (port->string input)))

;; Define a list of tokens as the result from invoking `jamrb-lex` on `input`.
(define tokens (jamrb-lex input))

;; Write the list of tokens to std-out.
(pretty-write tokens)
