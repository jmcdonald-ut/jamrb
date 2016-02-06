;; misc.rkt
;;
;; "With my last breath, I curse Zoidberg!"
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "keywords.rkt"
         "punct.rkt"
         "utility.rkt")

(provide comment
         newlines
         newline-lex
         embexpr-end
         space)

;; Defines the lexer abberviation for a ruby style comment.
(define-lex-abbrev comment (:: (:+ "#")
                               (:* (:- any-char #\newline))
                               (:? #\newline)))

;; Defines the lexer abberviation for one or more newlines.
(define-lex-abbrev newlines (:+ #\newline))

;; Defines the lexer abbreviation for ending an embedded expression.
(define-lex-abbrev embexpr-end (:: #\}))

;; (port, fn, bool?) -> void
;;
;; Scans the port for one or more newlines. The first newline is tokenized as a
;; significant, while subsequent newlines are tokenized as insignificant. Once
;; all newlines the callback is invoked, and assumed to continue tokenizing the
;; port.
(define (newline-lex port callback [first? #t])
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))

  (define internal-lex
    (lexer
     [#\newline (handle-newline lexeme)]
     [any-char (callback (rewind))]
     [(eof) '()]))

  (define (handle-newline value)
    (let* ([ignore? (or (seen-method-with-parens?) (not first?))])
      (set-seen-def! #f)
      (set-seen-method-with-parens! #f)
      (tok-con line col (if ignore? 'ignored_nl 'nl) value)))

  (define (tok-con line col key lexeme)
    (cons (tokenize line col key lexeme)
          (newline-lex port callback #f)))

  ; Begin lexically analyzing new lines.
  (internal-lex port))

;; Defines a lexer abbreviation for all whitespace except newlines.
(define-lex-abbrev space (:+ (:- whitespace #\newline)))
