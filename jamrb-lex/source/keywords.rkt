;; keywords.rkt
;;
;; "Life is about decisisions. Make the wrong ones and you'll wind up face down in a pool of your own
;; blood and urine."
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "misc.rkt"
         "port.rkt"
         "token.rkt")

(provide keyword
         lex-keyword)

;; Defines the lexer abbreviation for a ruby keyword.  The list of ruby keywords can be found at
;; http://ruby-doc.org/core-2.3.0/doc/keywords_rdoc.html
(define-lex-abbrevs
  [keyword (:: (:* whitespace) single-keyword (:+ whitespace))]
  [single-keyword (:or  "__ENCODING__" "__LINE__" "__FILE__" "BEGIN" "END" "alias" "and"
                        "begin" "break" "case" "class" "def" "defined?" "do" "else" "elsif"
                        "end" "ensure" "false" "for" "if" "in" "module" "nil" "not" "or"
                        "redo" "rescue" "retry" "return" "self" "super" "then" "true" "undef"
                        "unless" "until" "when" "while" "yield")])

;; (port, fn) -> '()
;;
;; Returns a list of newline, whitespace, and keyword tokens from the **string**.
(define (lex-keyword port callback)
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))
  (define tok-con (prepare-tokenizer))

  (define internal-lex
    (lexer
     [newlines (newline-lex (rewind (string-length lexeme)) (λ (port) (lex-keyword port callback)))]
     [space (tok-con line col 'sp lexeme (λ () (lex-keyword port callback)))]
     [single-keyword (tok-con line col 'kw lexeme (λ () (lex-keyword port callback)))]
     [any-char (callback (rewind (string-length lexeme)))]
     [(eof) '()]))

  (internal-lex port))
