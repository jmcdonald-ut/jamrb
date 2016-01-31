;; strings.rkt
;;
;; "Well I'm sorry, the world isn't one big liberal arts college campus!"
;;
;; By Jonathon McDonald
#lang racket
(require racket/trace
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "port.rkt"
         "token.rkt")

(provide string-lex
         string-lex-no-open
         string-opening)

(define tok-con (prepare-tokenizer))

;; (string) -> '():values
;;
;; Taking the terminator, returns a collection of useful values for lexing strings.
(define (prepare-string-lex-fns terminator)
  (let* ([char-is-terminator? (curry equal? terminator)]
         [char-is-escape? (curry equal? "\\")])
    (values char-is-terminator?
            char-is-escape?)))

;; (port, fn, fn?) -> fn
;;
;; Prepares a lexer for a string.
(define (prepare-lexer port callback [interpolate null])
  (lexer
   [embexpr (interpolate port lexeme)]
   [any-char (callback lexeme)]
   [(eof) '()]))

;; (port, fn, string, string?, number?, number?) -> '()
;;
;; Tokenizes string contents, the string terminator, and embedded expressions within the string 
;; contents.
(define (lex-string port callback terminator interpolated? [contents ""] [sline #f] [scol #f])
  (define-values (line col) (watch-port-position! port))
  (define-values (char-is-terminator? char-is-escape?) (prepare-string-lex-fns terminator))
  
  (cond [(false? sline) (set! sline line)])
  (cond [(false? scol) (set! scol col)])
  
  (define (handle-embexpr port value)
    (set! value (string-append "\\" value))
    (define (lex-embexpr)
      (callback port continue-string))
    (define (continue-string port)
      (lex-string port callback terminator interpolated?))
    
    (if interpolated?
        (tok-con line col 'embexpr_beg value lex-embexpr)
        (append-cont! value)))

  ;; (string) -> '()
  ;;
  ;; Appends the character to `contents` and continues to lex, returning the result of lexing.
  (define (append-cont! value)
    (set! contents (string-append contents value))
    (lex-string port callback terminator interpolated? contents sline scol))
  
  ;; () -> '()
  ;;
  ;; Completes the string by creating the string content and string end tokens, and passing the port 
  ;; back to the supplied callback.
  (define (complete-string!)
    (define (call-home)
      (callback port))
    (define (tokenize-terminator)
      (tok-con line col 'tstring_end terminator call-home))
    
    (if (> (string-length contents) 0)
        (tok-con sline scol 'tstring_content contents tokenize-terminator)
        (tokenize-terminator)))

  (define (handle-escape) (void))
  (define (handle-char value)
    (match value
      [(app char-is-terminator? #t) (complete-string!)]
      [(app char-is-escape? #t) (handle-escape)]
      [_ (append-cont! value)]))

  (define internal-lex (prepare-lexer port handle-char handle-embexpr))
  (internal-lex port))
  

;; (port, string, fn) -> '()
;;
;; Returns a pair with the first value being a token containing the string `opening` and the second 
;; value being the rest of the tokens scanned.
(define (string-lex port opening line col callback)
  (define interpolated (equal? opening "\""))
  (tok-con line col 'tstring_beg opening
           (λ () (lex-string port callback opening interpolated))))

;; (port, string, fn) -> '()
;;
;; Tokenizes the string without creating a token for the string opening.  Returns a list of tokens.
(define (string-lex-no-open port opening callback)
  (define interpolated? #t)
  (lex-string port callback opening interpolated?))

;; Defines the lexer abbreviation for a string opening. 
;;
;; TO-DO: Support %q, %Q.
(define-lex-abbrev string-opening (:or str-dbl str-single))

;; Defines the individual lexer abbreviations for string.
(define-lex-abbrevs [str-dbl (:or #\")] [str-single (:or #\')] [embexpr (:: #\# #\{)])
