;; state.rkt
;;
;; "There's a time and a place for everything, and it's called college."
;;
;; By Jonathon McDonald
#lang racket

;;
;; Port Line/Column Watching
;;

(define port-state (make-hash))

(hash-set*! port-state
            'line 1
            'next-col 0
            'next-utf-8-col 0)


(provide (contract-out [next-utf-8-col (-> exact-integer? exact-integer?)]))

(define (next-utf-8-col line)
  (if (eq? line (current-line))
      (hash-ref port-state 'next-utf-8-col)
      0))


(define (current-line)
  (hash-ref port-state 'line))


(provide (contract-out [increment-columns! (-> list? any/c)]))

(define (increment-columns! token)
  (let* ([len (string-length (token->value token))]
         [utf8-len (string-utf-8-length (token->value token))]
         [next (hash-ref port-state 'next-col)]
         [next-utf (hash-ref port-state 'next-utf-8-col)])
    (hash-set*! port-state
                'next-col (+ next len)
                'next-utf-8-col (+ next-utf utf8-len))))


(provide (contract-out [update-port-state! (-> list? any/c)]))

(define (update-port-state! token)
  (if (eq? (current-line) (token->line token))
      (increment-columns! token)
      (hash-set*! port-state
                  'line (token->line token)
                  'next-col (string-length (token->value token))
                  'next-utf-8-col (string-utf-8-length (token->value token)))))


;;
;; Tokens
;;

(define internal-lexed-tokens '())


(provide (contract-out [add-lexed-token! (-> list? list?)]))

(define (add-lexed-token! token)
  (update-port-state! token)
  ; Note: Technically a token is simply a list, formatted something like:
  ;   `(Token ((line col) key value))
  ;    (second (second `(Token ((line col) key value)))) ; -> key
  (set! internal-lexed-tokens
        (append internal-lexed-tokens `(,token)))
  token)


(provide (contract-out [lexed-tokens (-> list?)]))

(define (lexed-tokens)
  internal-lexed-tokens)


(provide (contract-out [last-non-space-token (-> list?)]))

(define (last-non-space-token)
  (define not-space? (negate (compose (curry equal? 'on_sp) token-type)))
  (findf not-space? (reverse (lexed-tokens))))


(provide (contract-out [last-non-space-token-is? (-> symbol? boolean?)]))

(define (last-non-space-token-is? key)
  (and (not (empty? (lexed-tokens)))
       (equal? (token-type (last-non-space-token)) key)))


(provide (contract-out [last-non-space-token-eq? (-> string? boolean?)]))

(define (last-non-space-token-eq? value)
  (and (not (empty? (lexed-tokens)))
       (equal? (fetch-token-value (last-non-space-token)) value)))


(provide (contract-out [token-type (-> list? symbol?)]))

(define (token-type token)
  (second (second token)))


(provide (contract-out [fetch-token-value (-> list? string?)]))

(define (fetch-token-value token)
  (third (second token)))


(provide (contract-out [token->line (-> list? exact-integer?)]))

(define (token->line token)
  (first (first (second token))))


(provide (contract-out [token->col (-> list? exact-integer?)]))

(define (token->col token)
  (second (first (second token))))


(provide (contract-out [token->value (-> list? string?)]))

(define (token->value token [denormalized? #t])
  (if denormalized?
      (value->denormalized (third (second token)))
      (third (second token))))


(provide (contract-out [value->denormalized (-> string? string?)]))

(define (value->denormalized value)
  (regexp-replace* "\\\\#{" value "#{"))


;;
;; Identifiers
;;

(provide (contract-out [id-char-terminator? (-> char? boolean?)]))

(define (id-char-terminator? value)
  (match value
    [(or #\! #\?) #t]
    [(and #\: (app (λ (val) (any-punct?)) #t)) #t]
    [_ #f]))


(provide (contract-out [id->token-sym (-> string? symbol?)]))

(define (id->token-sym value)
  (match (string-ref value 0)
    [(app char-upper-case? #t) 'const]
    [(app (λ (val) (equal? #\@ val)) #t) 'ivar]
    [(app (λ (val) (string-suffix? value ":")) #t) 'label]
    [_ 'ident]))


(provide (contract-out [id-char-invalid? (->* (char?) (boolean?) boolean?)]))

(define (id-char-invalid? value [first? #f])
  (or (and (char>? value (integer->char 0)) (char<? value #\!))
      (and (char>? value #\!) (char<? value #\0))
      (and (char>? value #\9) (char<? value #\:))
      (and (char>? value #\:) (char<? value #\?))
      (and (not first?) (eq? value #\@))
      (and (char>? value #\Z) (char<? value #\_))
      (eq? value #\`)
      (and (char>? value #\z) (char<? value (integer->char 127)))))


;;
;; Punctuation
;;

(define punct-stack '())
(define punct-pairs (make-hash))
(define punct-symbols (make-hash))

(hash-set*! punct-pairs
            "<" ">"
            "(" ")"
            "{" "}"
            "[" "]"
            "'" "'"
            "\"" "\"")

(hash-set*! punct-symbols
            "." 'period
            "," 'comma
            ";" 'semicolon
            "(" 'lparen
            ")" 'rparen
            "{" 'lbrace
            "}" 'rbrace
            "[" 'lbracket
            "]" 'rbracket)


(provide (contract-out [punct->close (-> string? string?)]))

(define (punct->close punct)
  (hash-ref punct-pairs punct))


(provide (contract-out [punct-is-open? (-> string? boolean?)]))

(define (punct-is-open? punct)
  (hash-has-key? punct-pairs punct))


(provide (contract-out [punct-is-stackable? (-> string? boolean?)]))

(define (punct-is-stackable? punct)
  (not (or (equal? punct ";") (equal? punct ".") (equal? punct ","))))


(provide (contract-out [any-punct? (-> boolean?)]))

(define (any-punct?)
  (not (empty? punct-stack)))


(provide (contract-out [push-punct! (-> string? any/c)]))

(define (push-punct! punct)
  (set! punct-stack (cons punct punct-stack)))


(provide (contract-out [peek-punct (-> (or/c string? boolean?))]))

(define (peek-punct)
  (if (any-punct?)
      (car punct-stack)
      #f))


(provide (contract-out [in-array? (-> boolean?)]))

(define (in-array?)
  (and (any-punct?) (equal? "[" (peek-punct))))


(provide (contract-out [pop-punct! (-> (or/c string? boolean?))]))

(define (pop-punct!)
  (define result (peek-punct))
  (unless (not result) (set! punct-stack (cdr punct-stack)))
  result)


(provide (contract-out [pop-punct-pair! (-> string? (or/c string? boolean?))]))

(define (pop-punct-pair! close-value)
  (define result (pop-punct!))
  (define actual (punct->close result))

  (cond [(not (equal? actual close-value))
         (raise-syntax-error 'unexpected-token (format "Expecting ~a" actual))])

  result)


(provide (contract-out [punct->symbol (-> string? symbol?)]))

(define (punct->symbol punct)
  (hash-ref punct-symbols punct))


(provide (contract-out [embexpr-terminator? (-> string? boolean?)]))

(define (embexpr-terminator? punct)
  (and (equal? punct "}")
       (or (not (any-punct?)) (not (equal? (peek-punct) "{")))))


(provide (contract-out [backtick? (-> string? boolean?)]))

(define (backtick? punct)
  (equal? punct "`"))


;;
;; Strings
;;

(provide (contract-out [opening->term (-> string? string?)]))

(define (opening->term opening)
  (define (handle-backtick-or-q)
    (if (backtick? opening)
        opening
        (punct->close (regexp-replace #rx"\\%[Qq]([{<([])" opening "\\1"))))
  (if (punct-is-open? opening)
      (punct->close opening)
      (handle-backtick-or-q)))


(provide (contract-out [interpolated? (-> string? boolean?)]))

(define (interpolated? opening)
  (or (equal? opening "\"")
      (string-prefix? opening "%Q")))


(provide (contract-out [char->string (-> char? string?)]))

(define (char->string ch)
  (list->string `(,ch)))


;;
;; Tracking
;;

(define method-tracking (make-hash))

(hash-set*! method-tracking
            'spotted? #f
            'with-parens? #f)


(provide (contract-out [track-def! (->* () (boolean?) boolean?)]))

(define (track-def! [spotted? #t])
  (hash-set! method-tracking 'spotted? spotted?)
  spotted?)


(provide (contract-out [def-tracked? (-> boolean?)]))

(define (def-tracked?)
  (hash-ref method-tracking 'spotted?))


(provide (contract-out [def-tracked-with-parens? (-> boolean?)]))

(define (def-tracked-with-parens?)
  (hash-ref method-tracking 'with-parens?))


(provide (contract-out [track-def-with-parens! (->* () (boolean?) boolean?)]))

(define (track-def-with-parens! [with-parens? #f])
  (hash-set! method-tracking 'with-parens? with-parens?)
  with-parens?)


(provide
 (contract-out [reset-method-tracking! (->* () (boolean? boolean?) any/c)]))

(define (reset-method-tracking! [spotted? #f] [with-parens? #f])
  (hash-set*! method-tracking
              'spotted? spotted?
              'with-parens? with-parens?))


;;
;; Heredocs
;;

(define hdoc-tracking (make-hash))

(hash-set*! hdoc-tracking
            'interpolated? #f
            'indented? #f)


(provide
 (contract-out [reset-hdoc-tracking! (->* () (boolean? boolean?) any/c)]))

(define (reset-hdoc-tracking! [interpolated? #f] [indented? #f])
  (hash-set*! hdoc-tracking
              'interpolated? interpolated?
              'indented? indented?))


(provide (contract-out [hdoc-interpolated? (-> boolean?)]))

(define (hdoc-interpolated?)
  (hash-ref hdoc-tracking 'interpolated?))


(provide (contract-out [hdoc-indented? (-> boolean?)]))

(define (hdoc-indented?)
  (hash-ref hdoc-tracking 'indented?))


(provide (contract-out [set-hdoc-interpolated! (->* () (boolean?) boolean?)]))

(define (set-hdoc-interpolated! [interpolated? #f])
  (hash-set! hdoc-tracking 'interpolated? interpolated?)
  interpolated?)


(provide (contract-out [set-hdoc-indented! (->* () (boolean?) boolean?)]))

(define (set-hdoc-indented! [indented? #f])
  (hash-set! hdoc-tracking 'indented? indented?)
  indented?)


;;
;; Strings
;;

(define active-contents "")
(define active-sline -1)
(define active-scol -1)
(define active-interpolated? #f)


(provide (contract-out [string-contents (-> string?)]))

(define (string-contents)
  active-contents)


(provide
 (contract-out
  [string-content-values (-> (values string? exact-integer? exact-integer?))]))

(define (string-content-values)
  (values active-contents active-sline active-scol))


(provide
 (contract-out
  [start-string! (-> exact-integer? exact-integer? boolean? any/c)]))

(define (start-string! line col interpolated?)
  (set!-values (active-sline active-scol active-interpolated?)
               (values line col interpolated?)))


(provide
 (contract-out
  [maybe-start-string! (-> exact-integer? exact-integer? boolean? any/c)]))

(define (maybe-start-string! line col interpolated?)
  (cond [(or (eq? active-sline -1) (eq? active-scol -1))
         (start-string! line col interpolated?)]))


(provide
 (contract-out
  [reset-string-contents! (-> (values string? exact-integer? exact-integer?))]))

(define (reset-string-contents!)
  (define-values (contents line col) (string-content-values))
  (set!-values (active-contents active-sline active-scol) (values "" -1 -1))
  (values contents line col))


(provide (contract-out (add-string-contents! (-> string? procedure? any/c))))

(define (add-string-contents! new-contents callback)
  (set! active-contents (string-append active-contents new-contents))
  (callback))


(provide (contract-out [chop-string-contents! (-> exact-integer? string?)]))

(define (chop-string-contents! amt)
  (define keep-until (- (string-length (string-contents)) amt))
  (set! active-contents (substring active-contents 0 keep-until))
  (string-contents))


(provide (contract-out (string-interpolated? (-> boolean?))))

(define (string-interpolated?)
  active-interpolated?)
