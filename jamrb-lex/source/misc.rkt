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

(provide comment
         newlines
         newline-lex
         space
         punct
         punct->symbol)

;; Defines the lexer abberviation for a ruby style comment.
(define-lex-abbrev comment (:: (:+ "#")
                               (:* (:- any-char #\newline))
                               (:? #\newline)))

;; Defines the lexer abberviation for one or more newlines.
(define-lex-abbrev newlines (:+ #\newline))

;; (port, fn, bool?) -> void
;;
;; Scans the port for one or more newlines.  The first newline is tokenized as a significant, while
;; subsequent newlines are tokenized as insignificant.  Once all newlines the callback is invoked,
;; and assumed to continue tokenizing the port.
(define (newline-lex port callback [first? #t])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

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

;; Defines the lexer abbreviation for ruby-specific punctuation.
(define-lex-abbrev punct (:or #\. #\, #\( #\) #\{ #\} #\[ #\]))

;; Defines a hash table that will hold ruby-specific punctuation as the key which maps to its token
;; symbol.
(define punct-symbol-ht (make-hash))

;; Defines the actual key/value pairs for the ruby-specific punctuation.
(hash-set*! punct-symbol-ht
            "." 'period
            "," 'comma
            "(" 'lparen
            ")" 'rparen
            "{" 'lbrace
            "}" 'rbrace
            "[" 'lbracket
            "]" 'rbracket)

;; (string) -> symbol
;;
;; Returns the token symbol for the given punctuation.
(define (punct->symbol value)
  (hash-ref punct-symbol-ht value))

;; Defines a lexer abbreviation for all whitespace except newlines.
(define-lex-abbrev space (:+ (:- whitespace #\newline)))
