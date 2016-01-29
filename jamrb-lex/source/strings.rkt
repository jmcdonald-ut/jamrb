;; strings.rkt
;;
;; "Well I'm sorry, the world isn't one big liberal arts college campus!"
;;
;; General strategy for handling strings
;;
;;   1. Store String Open Value
;;   2. Return pair (string_open, rest...)
;;     2a. Create new list for contents.
;;     2b. For every character that is not equivalent to string_open, push to list.
;;     2c. If character is equivalent to string_open
;;       |   Not Escaped: Return pair (string_contents, rest...)
;;       |   Else: push to list, jump to step 2b.
;;     2d. Return pair (string_close, rest...) where rest is result of callback.
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "port.rkt"
         "token.rkt")

(provide string-lex
         string-lex-no-open
         string-opening)

(define tok-con (prepare-tokenizer))

;; (port, string, fn) -> '()
;;
;; Returns a pair with the first value being a token containing the string `opening` and the second 
;; value being the rest of the tokens scanned.
(define (string-lex port opening line col callback)
  (tok-con line col 'tstring_beg opening
           (λ () (internal-string-lex port opening callback))))

;; (port, string, fn) -> '()
;;
;; Tokenizes the string without creating a token for the string opening.  Returns a list of tokens.
(define (string-lex-no-open port opening callback)
  (internal-string-lex port opening callback))

;; (port, string, fn, string?, bool?, bool?) -> '()
;;
;; Internal string lexer which goes through the contents of a string character by character and 
;; returns a list of the resulting tokens.
;;
;; port: I/O port currently being scanned.
;; opening: string opening character(s).
;; callback: function that is called upon successful string closure.
;; [contents ""]: string contents which should hold a string of all characters scanned.
;; [sline #f]: optional string contents line, will be set if false.
;; [scol #f]: optional string contents column, will be set if false.
(define (internal-string-lex port opening callback [contents ""] [sline #f] [scol #f])
  ; Set current line/column.
  (define-values (line col) (watch-port-position! port))
  
  ; If the initial `sline`, `scol` are not truthy set them. They represent where the contents began.
  (cond [(false? sline) (set! sline line)])
  (cond [(false? scol) (set! scol col)])

  ; Utility function which generates a token for the string end.
  (define (complete-string)
    (tok-con line col 'tstring_end opening 
             (λ () (callback port))))

  ; (string) -> void
  (define (append-char-unless-opening str)
    (if (equal? str opening)
        (tok-con sline scol 'tstring_content contents 
                 complete-string)
        (internal-string-lex port opening callback (string-append contents str) sline scol)))

  ; Internal lexer responsible for scanning the string.
  (define internal-lex
    (lexer
     [any-char (append-char-unless-opening lexeme)]
     [(eof) '()]))

  (internal-lex port))

;; Defines the lexer abbreviation for a string opening. 
;;
;; TO-DO: Support %q, %Q.
(define-lex-abbrev string-opening (:or str-dbl str-single))

;; Defines the individual lexer abbreviations for various string openings.
(define-lex-abbrevs [str-dbl (:or #\")] [str-single (:or #\')])
