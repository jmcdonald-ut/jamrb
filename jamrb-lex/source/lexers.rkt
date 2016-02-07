;; lexers.rkt
;;
;; "With my last breath, I curse Zoidberg!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex (prefix-in : parser-tools/lex-sre)
         "abbrevs.rkt" "utility.rkt" "state.rkt")

(provide newline-lex id-lex lex-keyword)

(define (newline-lex port callback [first? #t])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

  (define internal-lex
    (lexer
     [#\newline (handle-newline lexeme)]
     [any-char (callback (rewind))]
     [(eof) '()]))

  (define (handle-newline value)
    (let* ([ignore? (or (def-tracked-with-parens?) (not first?))])
      (reset-method-tracking!)
      (tok-con line col (if ignore? 'ignored_nl 'nl) value)))

  (define (tok-con line col key lexeme)
    (cons (tokenize line col key lexeme)
          (newline-lex port callback #f)))

  ; Begin lexically analyzing new lines.
  (internal-lex port))


;; (port, fn) -> '()
;;
;; Processes and tokenizes an identifier.  Once tokenization is complete it will
;; invoke the callback with the port as an argument.  The port should be rewound
;; to the first character.
(define (id-lex port callback [contents ""] [sline #f] [scol #f])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

  (cond [(false? sline) (set! sline line)])
  (cond [(false? scol) (set! scol col)])

  ;; (string) -> string
  ;;
  ;; Sets contents to be equal to memo appended to itself.
  (define (append-contents! memo) (set! contents (string-append contents memo)))

  ;; (string) -> '()
  ;;
  ;; Appends the memo to contents and continues to lex the string.
  (define (append-and-continue! memo)
    (append-contents! memo)
    (id-lex port callback contents sline scol))

  ;; (string, bool?) -> '()
  ;;
  ;; Completes the identifier and continues on tokenizing by passing control
  ;; back to the caller.
  (define (complete-id! memo [should-unget #t])
    (if should-unget
        (rewind (string-length memo))
        (append-contents! memo))

    ;; Raise an exception if the contents string is empty.
    (cond [(eq? (string-length contents) 0) (error "invalid syntax")])

    (cons (tokenize sline scol (id->token-sym contents) contents)
          (callback port)))

  ;; (string) -> '()
  ;;
  ;; Inspects the string (which should simply be a character) and decides
  ;; whether to keep feeding in characters or complete the identifier token.
  (define (match-id-char memo)
    (match (string->char memo)
      [(app char-whitespace? #t) (complete-id! memo)]
      [(app (λ (val)
              (id-char-invalid? val (equal? (string-length contents) 0))) #t)
       (complete-id! memo)]
      [(app id-char-terminator? #t) (complete-id! memo #f)]
      [_ (append-and-continue! memo)]))

  (define lex (lexer [any-char (match-id-char lexeme)]
                     [(eof) (complete-id! "")]))
  (lex port))

(define (lex-keyword port callback)
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

  (define internal-lex
    (lexer
     [single-keyword (handle-keyword lexeme)]
     [any-char (callback (rewind (string-length lexeme)))]
     [(eof) '()]))

  (define (handle-keyword value)
    (track-def! (equal? value "def"))
    (tokenize-cons line col 'kw value (λ () (lex-keyword port callback))))

  (internal-lex port))
