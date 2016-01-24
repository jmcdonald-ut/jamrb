;; keywords.rkt
;;
;; "Life is about decisisions. Make the wrong ones and you'll wind up face down in a pool of your own
;; blood and urine."
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide keyword)

;; Defines the lexer abbreviation for a ruby keyword.  The list of ruby keywords can be found at
;; http://ruby-doc.org/core-2.3.0/doc/keywords_rdoc.html
(define-lex-abbrev keyword (:or "__ENCODING__" "__LINE__" "__FILE__" "BEGIN" "END" "alias" "and"
                                "begin" "break" "case" "class" "def" "defined?" "do" "else" "elsif"
                                "end" "ensure" "false" "for" "if" "in" "module" "nil" "not" "or"
                                "redo" "rescue" "retry" "return" "self" "super" "then" "true" "undef"
                                "unless" "until" "when" "while" "yield"))
