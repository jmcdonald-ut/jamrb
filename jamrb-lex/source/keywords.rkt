;; keywords.rkt
;;
;; "Life is about decisisions. Make the wrong ones and you'll wind up face down in a pool of your own
;; blood and urine."
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "port.rkt"
         "token.rkt")

(provide keyword
         lex-keyword
         has-seen-def?
         set-seen-def!)

(define seen-def? #f)

;; Defines the lexer abbreviation for a ruby keyword.  The list of ruby keywords can be found at
;; http://ruby-doc.org/core-2.3.0/doc/keywords_rdoc.html
(define-lex-abbrevs
  [keyword (:: single-keyword (:or #\. #\( #\) #\[ #\] whitespace))]
  [single-keyword (:or  "__ENCODING__" "__LINE__" "__FILE__" "BEGIN" "END" "alias" "and"
                        "begin" "break" "case" "class" "def" "defined?" "do" "else" "elsif"
                        "end" "ensure" "false" "for" "if" "in" "module" "nil" "not" "or"
                        "redo" "rescue" "retry" "return" "self" "super" "then" "true" "undef"
                        "unless" "until" "when" "while" "yield")])

;; () -> bool
;;
;; Returns a value indicating whether the +def+ keyword has been seen.
(define (has-seen-def?)
  seen-def?)

;; (bool) -> bool
;;
;; Sets whether or not a +def+ keyword has been seen.  Returns the boolean provided.
(define (set-seen-def! bool)
  (set! seen-def? bool)
  bool)

;; (port, fn) -> '()
;;
;; Returns a list of newline, whitespace, and keyword tokens from the **string**.
(define (lex-keyword port callback)
  (define-values (line col) (watch-port-position! port))
  (define rewind (prepare-port-rewinder port line col))
  (define tok-con (prepare-tokenizer))

  (define internal-lex
    (lexer
     [single-keyword (handle-keyword lexeme)]
     [any-char (callback (rewind (string-length lexeme)))]
     [(eof) '()]))

  (define (handle-keyword value)
    (set-seen-def! (equal? value "def"))
    (tok-con line col 'kw value (λ () (lex-keyword port callback))))

  (internal-lex port))
