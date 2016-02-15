;; abbrevs.test.rkt
;;
;; "Life is about decisions. Make the wrong ones and you'll wind up face down
;; in a pool of your own blood and urine."
;;
;; By Jonathon McDonald
#lang racket
(require rackunit rackunit/text-ui parser-tools/lex "../source/abbrevs.rkt")

(define abbrev-tests
  (test-suite
   "Tests for abbrevs.rkt"

   (test-case
    "+rx-style+ abbrev matches regex looking sequences"
    (let* ([simple "/abc/"]
           [longer "/[a-zA-Z]*/i"]
           [escaped "/\\//"]
           [invalid "4 / 45"]
           [empty "//"])

      ; This tester expects EXACT matches.
      (define simple-lexer (lexer [rx-style #t] [any-string #f]))
      (define lex (λ (str) (simple-lexer (open-input-string str))))

      (check-true (lex simple))
      (check-true (lex longer))
      (check-true (lex empty))
      (check-true (lex escaped))
      (check-false (lex invalid))))))

(run-tests abbrev-tests)
