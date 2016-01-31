;; identifiers.rkt
;;
;; "There's a time and a place for everything, and it's called college."
;;
;; Identifiers will be tough for various reasons.  Some valid names include:
;;
;;   foo
;;   ==
;;   []
;;   +=
;;   exists?
;;   dangerous!
;;   __id__
;;   ❨╯°□°❩╯┻━┻
;;
;; Invalid names however include:
;;
;;   foo??
;;   bar!!
;;   b@r
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/trace
         "port.rkt"
         "token.rkt")

(provide id-start
         id-lex)

;; Defines the lexer abbreviation of a ruby identifier.
;;
;; NOTE: I may have to literally go through an identifer character by character but I think that
;; this is an okay attempt.
(define-lex-abbrev id-start (:- any-char (:or #\{ #\} #\[ #\] #\< #\> #\( #\))))

;; (string) -> char
;;
;; Returns a single character string as an actual character.  Raises an error if the string length is
;; not equal to one.
(define (string->char value)
  (cond [(not (eq? 1 (string-length value))) (raise-argument-error 'value "string:[1,1]?" value)])
  (first (string->list value)))

;; (char) -> bool
;;
;; Returns a value indicating whether the character is an id terminator.
(define (id-char-terminator? val)
  (match val
    [(or #\? #\!) #t]
    [_ #f]))

;; (char) -> bool
;;
;; Returns a value indicating whether the character is invalid for an id.
(define (id-char-invalid? val [first? #f])
  (or (and (char>? val (integer->char 0)) (char<? val #\!))
      (and (char>? val #\!) (char<? val #\0))
      (and (char>? val #\9) (char<? val #\?))
      (and (not first?) (eq? val #\@))
      (and (char>? val #\Z) (char<? val #\_))
      (eq? val #\`)
      (and (char>? val #\z) (char<? val (integer->char 127)))))

;; (string) -> symbol
;;
;; Returns a value based on how the identifier starts.
(define (sym-for-ident string)
    (cond [(<= (string-length string) 0) (error "invalid syntax")])
    (match (string-ref string 0)
      [(app char-upper-case? #t) 'const]
      [(app (λ (val) (equal? #\@ val)) #t) 'ivar]
      [_ 'ident]))

;; (port, fn) -> '()
;;
;; Processes and tokenizes an identifier.  Once tokenization is complete it will invoke the callback
;; with the port as an argument.  The port should be rewound to the first character.
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
  ;; Completes the identifier and continues on tokenizing by passing control back to the caller.
  (define (complete-id! memo [should-unget #t])
    (if should-unget
        (rewind (string-length memo))
        (append-contents! memo))

    ;; Raise an exception if the contents string is empty.
    (cond [(eq? (string-length contents) 0) (error "invalid syntax")])

    (cons (tokenize sline scol (sym-for-ident contents) contents) (callback port)))

  ;; (string) -> '()
  ;;
  ;; Inspects the string (which should simply be a character) and decides whether to keep feeding in
  ;; characters or complete the identifier token.
  (define (match-id-char memo)
    (match (string->char memo)
      [(app char-whitespace? #t) (complete-id! memo)]
      [(app (λ (val) (id-char-invalid? val (equal? (string-length contents) 0))) #t) (complete-id! memo)]
      [(app id-char-terminator? #t) (complete-id! memo #f)]
      [_ (append-and-continue! memo)]))

  (define lex (lexer [any-char (match-id-char lexeme)] [(eof) (complete-id! "")]))
  (lex port))
