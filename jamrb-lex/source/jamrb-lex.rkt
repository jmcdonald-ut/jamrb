;; jamr-lex.rkt
;;
;; "Good news, everyone!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         racket/trace
         "abbrevs.rkt"
         "lexers.rkt"
         "utility.rkt"
         "state.rkt")

(provide jamrb-lex)

(define input (void))
(define test-input "foo = true")

;; (port, fn?) -> '() || string
;;
;; Lexically analyzes the given I/O port against the ruby syntax.
;;
;; Returns a list of tokens in the order they were observed once it reaches the
;; EOF. If a terminator character and callback are supplied then upon reaching
;; that character the callback will be invoked with the port.
(define (jamrb-lex port [embexpr-callback #f])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

  (define (call-self port [embexpr-callback embexpr-callback])
    (jamrb-lex port embexpr-callback))

  (define (call-self-without-port)
    (jamrb-lex port embexpr-callback))

  ; Define our lexer.
  (define lex
    (lexer
     [comment (tok-con line col 'comment (escape-embed lexeme))]
     [newlines (newline-lex (rewind (string-length lexeme)) call-self)]
     [space (tok-con line col 'sp lexeme)]
     [int-literal (tok-con line col 'int lexeme)]
     [float-literal (tok-con line col 'float lexeme)]
     [rx-style (lex-regex (rewind (string-utf-8-length lexeme)) call-self)]
     [op (tok-con line col 'op lexeme)]
     [unary-op (unary-op-lex (rewind (string-length lexeme)) call-self)]
     [keyword (lex-keyword (rewind (string-length lexeme)) call-self)]
     [punct (handle-punct! line col lexeme)]
     [str-beg (string-lex port lexeme line col call-self)]
     [backtick (handle-backtick lexeme)]
     [symbeg (handle-sym line col lexeme port call-self)]
     [heredoc-beg (handle-heredoc (rewind (string-length lexeme)) call-self)]
     [embexpr-end (cons (tokenize line col 'embexpr_end lexeme)
                        (embexpr-callback port))]
     [id-start (id-lex (rewind (string-utf-8-length lexeme)) call-self)]
     [(eof) '()]))

  (define (handle-backtick value)
    (define (lex-string-and-come-home)
      (string-lex-no-open port value jamrb-lex))
    (tokenize-cons line col 'backtick value lex-string-and-come-home))

  (define (handle-punct! line col lexeme)
    (if (embexpr-terminator? lexeme)
        (cons (tokenize line col 'embexpr_end lexeme)
              (embexpr-callback port))
        (cons (tokenize-punct! line col lexeme)
              (jamrb-lex port embexpr-callback))))

  (define (handle-sym line col value port callback)
    (define token (tokenize line col 'symbeg value))
    (match value
      [":\"" (cons token (string-lex-no-open port "\"" callback))]
      [":'" (cons token (string-lex-no-open port "'" callback))]
      [_ (cons token (callback port))]))

  (define (handle-heredoc port callback [heredoc ""])
    (define-values (line col) (watch-port-position! port))
    (define rewind (prepare-port-rewinder port line col))
    (define (fill-contents port)
      (lex-heredoc-beg heredoc)
      (define terminator
        (regexp-replace #rx"<<[-~]?['\"`]?([^'\"`]+)['\"`]?" heredoc "\\1"))
      (lex-string port callback terminator (hdoc-interpolated?) 'heredoc_end))

    (define (call-self-with val)
      (λ () (handle-heredoc port callback val)))

    (define internal-lex
      (lexer
       [heredoc-beg (tokenize-cons line col 'heredoc_beg lexeme
                                   (call-self-with lexeme))]
       [newlines (newline-lex (rewind (string-length lexeme)) fill-contents)]))

    (internal-lex port))

  ; Tokenizes the value and continues lexical analysis.
  (define (tok-con line col key lexeme)
    (cons (tokenize line col key lexeme)
          (jamrb-lex port embexpr-callback)))

  ; Return the result of lexically analyzing the given port.
  ;(with-handlers ([exn:fail? (λ (e) (fail-for-invalid-syntax line col))])
    (lex port))

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
(define noop (jamrb-lex input))

;; Write the list of tokens to std-out.
(pretty-write (lexed-tokens))
