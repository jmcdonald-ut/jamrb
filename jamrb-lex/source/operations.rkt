;; operations.rkt
;;
;; "You win again, gravity."
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide operation)

;; Defines the lexer abberviation for the various operations.
(define-lex-abbrev operation (:or #\! #\~ #\+ #\- #\* #\/ #\% #\& #\> #\< #\= #\| #\^
                                  "**" ">>" "<<" "&&" "||" ".." "..." "<=>" "==" "==="
                                  "=~" "!~" "+=" "-=" "*=" "/=" "^=" "%=" "**=" ">=" "<="))
