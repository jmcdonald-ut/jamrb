; test-ripper-lex.rkt
; By Jonathon McDonald
#lang racket

(require rackunit
         rackunit/text-ui
         parser-tools/lex
         "../source/ripper-lex-token.rkt")

(define simple-pair-line-column-lexer (lexer [pair-line-column #t] [any-string #f]))

(define pair-line-column-tests
  (test-suite
   "Tests pair-line-column"

   (test-case
    "Standard line column pair is matched"
    (let* ([input-port (open-input-string "[1, 0]")])
      (check-true (simple-pair-line-column-lexer input-port))))
   
   (test-case
    "Line column pair with large values is matched"
    (let* ([input-port (open-input-string "[1394, 982]")])
      (check-true (simple-pair-line-column-lexer input-port))))
   
   (test-case
    "Line column pair with a negative value is not matched"
    (let* ([input-port (open-input-string "[-1, 0]")])
      (check-false (simple-pair-line-column-lexer input-port))))
   
   (test-case
    "Malformed input is not matched"
    (let* ([input-port (open-input-string "[22, 44")])
      (check-false (simple-pair-line-column-lexer input-port))))))

(run-tests pair-line-column-tests)

(define simple-token-type-lexer (lexer [token-type #t] [any-string #f]))

(define token-type-tests
  (test-suite
   "Tests token-type"
   
   (test-case
    "Standard token is matched"
    (let* ([input-port (open-input-string ":on_keyword")])
      (check-true (simple-token-type-lexer input-port))))
   
   (test-case
    "Token missing symbol indicator is not matched"
    (let* ([input-port (open-input-string "on_keyword")])
      (check-false (simple-token-type-lexer input-port))))
   
   (test-case
    "Token missing underscore is not matched"
    (let* ([input-port (open-input-string ":onkeyword")])
      (check-false (simple-token-type-lexer input-port))))
   
   (test-case
    "Token missing characters after underscore is not matched"
    (let* ([input-port (open-input-string ":on_")])
      (check-false (simple-token-type-lexer input-port))))
   
   (test-case
    "Token with non-alphabetic characters is not matched"
    (let* ([input-port (open-input-string ":on_keyw0rd")])
      (check-false (simple-token-type-lexer input-port))))
   
   (test-case
    "Token with multiple underscores is not matched"
    (let* ([input-port (open-input-string ":on__keyword")])
      (check-false (simple-token-type-lexer input-port))))))

(run-tests token-type-tests)

(define simple-value-lexer (lexer [value (begin #t)] [any-string #f]))

(define value-tests
  (test-suite
   "Tests value"
   
   (test-case
    "Standard value matched properly"
    (let* ([input-port (open-input-string "\"some string\"")])
      (check-true (simple-value-lexer input-port))))
   
   (test-case
    "Value which is numeric is matched properly"
    (let* ([input-port (open-input-string "\"5\"")])
      (check-true (simple-value-lexer input-port))))
   
   (test-case
    "Value which is just whitespace is matched properly"
    (let* ([input-port (open-input-string "\"     \"")])
      (check-true (simple-value-lexer input-port))))
   
   (test-case
    "Value with quotation marks is matched properly"
    (let* ([input-port (open-input-string "\" \\\" \"")])
      (check-true (simple-value-lexer input-port))))
   
   (test-case
    "Value which is premateurely closed is not matched"
    (let* ([input-port (open-input-string "\" so this is good \", but what's the big idea?\"")])
      (check-false (simple-value-lexer input-port))))
   
   (test-case
    "Value which is not closed properly is not matched"
    (let* ([input-port (open-input-string "\"some string")])
      (check-false (simple-value-lexer input-port))))))

(run-tests value-tests)

(define simple-ripper-token-lexer (lexer [ripper-token #t] [any-string #f]))

(define ripper-token-tests
  (test-suite
   "Tests ripper-token"
   
   (test-case
    "Standard ripper token is matched"
    (let* ([input-port (open-input-string "[[1, 0], :on_period, \".\"]")])
      (check-true (simple-ripper-token-lexer input-port))))
   
   (test-case
    "Ripper token with whitespace is matched"
    (let* ([input-port (open-input-string "[[51, 0], :on_sp, \"      \"]")])
      (check-true (simple-ripper-token-lexer input-port))))
   
   (test-case
    "Ripper token newline is matched"
    (let* ([input-port (open-input-string "[[59, 60], :on_nl, \"\n\"]")])
      (check-true (simple-ripper-token-lexer input-port))))
   
   (test-case
    "Ripper token with invalid line column pair is not matched"
    (let* ([input-port (open-input-string "[[59, 4E], :on_tstring_content, \"hello world\"]")])
      (check-false (simple-ripper-token-lexer input-port))))
   
   (test-case
    "Ripper token with invalid token-type is not matched"
    (let* ([input-port (open-input-string "[[1, 0], :on, \"def\"]")])
      (check-false (simple-ripper-token-lexer input-port))))
   
   (test-case
    "Ripper token with invalid value is not matched"
    (let* ([input-port (open-input-string "[[1, 0], :on_kw, \"def]")])
      (check-false (simple-ripper-token-lexer input-port))))))

(run-tests ripper-token-tests)