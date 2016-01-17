;; misc.rkt
;;
;; "With my last breath, I curse Zoidberg!"
;;
;; By Jonathon McDonald
#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide comment newline space)

; Comments:
(define-lex-abbrev comment (:: (:+ "#")
                               (:* (:- any-char #\newline))
                               (:? #\newline)))

; Newlines:
(define-lex-abbrev newline (:: #\newline))

; Spaces:
(define-lex-abbrev space (:+ whitespace))
