;; lexers.rkt
;;
;; "With my last breath, I curse Zoidberg!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex (prefix-in : parser-tools/lex-sre)
         "abbrevs.rkt" "utility.rkt" "state.rkt")


;;
;; Newlines
;;

(provide newline-lex)

(define (newline-lex port callback [first? #t])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

  (define internal-lex
    (lexer
     [#\newline (handle-newline lexeme)]
     [any-char (callback (rewind))]
     [(eof) '()]))

  (define (handle-newline value)
    (let* ([ignore? (or (def-tracked-with-parens?)
                        (not first?)
                        (last-non-space-token-eq? "|")
                        (last-non-space-token-eq? "do")
                        (last-non-space-token-is? 'on_lbrace)
                        (last-non-space-token-is? 'on_lbracket)
                        (last-non-space-token-is? 'on_lparen)
                        (last-non-space-token-is? 'on_heredoc_end)
                        (last-non-space-token-is? 'on_comma))])
      (reset-method-tracking!)
      (tok-con line col (if ignore? 'ignored_nl 'nl) value)))

  (define (tok-con line col key lexeme)
    (cons (tokenize line col key lexeme)
          (newline-lex port callback #f)))

  ; Begin lexically analyzing new lines.
  (internal-lex port))


;;
;; Ops
;;

(provide unary-op-lex)

(define (unary-op-lex port callback)
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

  (define internal-lex
    (lexer
     [single-unary-op (tokenize-cons line col 'op lexeme
                                     (λ () (callback port)))]))

  (internal-lex port))


;;
;; Identifiers
;;

(provide id-lex)

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

  (define lex (lexer ["::" (complete-id! lexeme)]
                     [any-char (match-id-char lexeme)]
                     [(eof) (complete-id! "")]))
  (lex port))


;;
;; Keywords
;;

(provide lex-keyword)

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


;;
;; Heredocs
;;

(provide lex-heredoc-beg)

; NOTE:  This lexer is only for determining state, do not use it to tokenize.
(define (lex-heredoc-beg heredoc-beg)
  (define internal-port (open-input-string heredoc-beg))

  (define (set-indented-and-cont! bool)
    (set-hdoc-indented! bool)
    (internal-lex internal-port))

  (define internal-lex
    (lexer
     [heredoc-prefix-indent (set-indented-and-cont! #t)]
     [heredoc-prefix-no-indent (set-indented-and-cont! #f)]
     [heredoc-interp (set-hdoc-interpolated! #t)]
     [heredoc-no-interp (set-hdoc-interpolated! #f)]))

  (internal-lex internal-port))


;;
;; Regex
;;

(provide lex-regex)

(define (lex-regex port callback [ending? #f])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

  (define (call-self [port port])
    (lex-regex port callback #t))

  (define internal-lex
    (lexer
     [rx-mark-end (handle-rx-mark lexeme)]
     [rx-mark (handle-rx-mark lexeme)]
     [any-char (lex-string (rewind) call-self "/" #f 'regexp_end)]
     [(eof) '()]))

  (define (handle-rx-mark val)
    (if (or (last-non-space-token-is? 'on_regexp_beg) ending?)
        (tokenize-cons line col 'regexp_end val (λ () (callback port)))
        (tokenize-cons line col 'regexp_beg val call-self)))

  (internal-lex port))


;;
;; Strings
;;

(define (prepare-lexer port callback [interpolate null])
  (lexer
   [embexpr (interpolate port lexeme)]
   [any-char (callback lexeme)]
   [(eof) '()]))


(define (prepare-string-lex-fns terminator heredoc?)
  (define (is-full-terminator? char-string port)
    (define-values (line col) (watch-port-position! port))
    (define rewind (prepare-port-rewinder port line col))

    (define len (string-length terminator))
    (define read-len (- len (string-length char-string)))

    (define in (string-append char-string (read-string read-len port)))
    (define matches? (equal? in terminator))
    (define prefix-valid? (end-newline? (string-contents) (hdoc-indented?)))
    (rewind read-len)

    (and prefix-valid? matches?))

  (define (is-terminator? char-string port)
    (if (and (not heredoc?) (equal? char-string terminator))
        #t
        (if (equal? char-string (char->string (string-ref terminator 0)))
            (is-full-terminator? char-string port)
            #f)))

  (define char-is-escape? (curry equal? "\\"))
  (values is-terminator? char-is-escape?))


(provide lex-string)

(define (lex-string port fn term interp? [term-type 'tstring_end])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))
  (maybe-start-string! line col interp?)
  (define-values (contents sline scol) (string-content-values))
  (define-values (is-terminator? char-is-escape?)
    (prepare-string-lex-fns term (eq? term-type 'heredoc_end)))

  (define call-self
    (λ () (lex-string port fn term interp? term-type)))

  (define (term-with-newline)
    (define next-char (read-char port))
    (define (rewind-unless-eof)
      (unless (eof-object? next-char) (unget port 1))
      #t)
    (if (equal? next-char #\newline)
        (string-append term (char->string next-char))
        (and (rewind-unless-eof) term)))

  (define (get-term)
    (if (equal? term-type 'heredoc_end)
        (term-with-newline)
        term))

  (define (handle-embexpr port value)
    (set! value (string-append "\\" value))
    (define (lex-embexpr)
      (fn port continue-string))

    (define (continue-string port)
      (lex-string port fn term interp? term-type))

    (if interp?
        (tokenize-string-contents!
         (λ () (tokenize-cons line col 'embexpr_beg value lex-embexpr)))
        (add-string-contents! value call-self)))

  (define (complete-string!)
    (forward port (- (string-length term) 1))
    (define full-terminator (get-term))
    (define (call-home)
      (fn port))
    (define (tokenize-terminator)
      (define normal-col (if (eq? term-type 'heredoc_end) 0 col))
      (tokenize-cons line normal-col term-type full-terminator call-home))

    (define chop? (and (eq? term-type 'heredoc_end) (hdoc-indented?)))
    (define (adjust-terminator)
      (let* ([prefix (string->nl-tail (string-contents))]
             [chop-amt (string-length prefix)]
             [new-terminator (string-append prefix full-terminator)])
        (chop-string-contents! chop-amt)
        (set! full-terminator new-terminator)))

    (cond [chop? (adjust-terminator)])

    (if (eq? term-type 'regexp_end)
        (begin (rewind) (tokenize-string-contents! call-home))
        (tokenize-string-contents! tokenize-terminator)))

  (define (handle-escape input)
    (define new-val (string-append input (char->string (read-char port))))
    (add-string-contents! new-val call-self))

  (define (handle-char value)
    (match value
      [(app (λ (val) (is-terminator? val port)) #t) (complete-string!)]
      [(app char-is-escape? #t) (handle-escape value)]
      [_ (add-string-contents! value call-self)]))

  (define internal-lex (prepare-lexer port handle-char handle-embexpr))
  (internal-lex port))


(provide string-lex)

(define (string-lex port opening line col callback)
  (define continue-lex
    (λ ()
      (lex-string port callback
                  (opening->term opening) (interpolated? opening))))
  (tokenize-cons line col 'tstring_beg opening continue-lex))


(provide string-lex-no-open)

(define (string-lex-no-open port opening callback)
  (lex-string port callback (opening->term opening)
              (interpolated? opening)))
