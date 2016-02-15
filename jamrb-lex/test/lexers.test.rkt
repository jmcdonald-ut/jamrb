;; lexers.test.rkt
;;
;; "It's a show about nothing."
;;
;; By Jonathon McDonald
#lang racket
(require rackunit rackunit/text-ui parser-tools/lex "../source/lexers.rkt")

; Test helper that will terminate lexing.
(define end-lexing (λ ([port #f]) '()))

(define lexers-test
  (test-suite
   "Tests for lexers.rkt"

   (test-case
    "+lex-regex+ properly tokenizes an empty literal"
    (let* ([empty (open-input-string "//")]
           [expected `((Token ((1 0) on_regexp_beg "/"))
                       (Token ((1 1) on_regexp_end "/")))])

      (check-equal? (lex-regex empty end-lexing) expected)))

   (test-case
    "+lex-regex+ tokenizes a simple pattern correctly"
    (let* ([simple (open-input-string "/hello/")]
           [expected `((Token ((1 0) on_regexp_beg "/"))
                       (Token ((1 1) on_tstring_content "hello"))
                       (Token ((1 6) on_regexp_end "/")))])

      (check-equal? (lex-regex simple end-lexing) expected)))

   (test-case
    "+lex-regex+ tokenizes a more complex pattern correctly"
    (let* ([complex (open-input-string "/[a-zA-Z0-9]*(hi)?/")]
           [expected `((Token ((1 0) on_regexp_beg "/"))
                       (Token ((1 1) on_tstring_content "[a-zA-Z0-9]*(hi)?"))
                       (Token ((1 18) on_regexp_end "/")))])

      (check-equal? (lex-regex complex end-lexing) expected)))

   (test-case
    "+lex-regex+ tokenizes a pattern with an escaped slash"
    (let* ([pattern (open-input-string "/\\/\\/ JS Comment/")]
           [expected `((Token ((1 0) on_regexp_beg "/"))
                       (Token ((1 1) on_tstring_content "\\/\\/ JS Comment"))
                       (Token ((1 16) on_regexp_end "/")))])

      (check-equal? (lex-regex pattern end-lexing) expected)))

   (test-case
    "+lex-regex+ tokenizes a pattern with an end delimiter"
    (let* ([pattern (open-input-string "/foo/i")]
           [expected `((Token ((1 0) on_regexp_beg "/"))
                       (Token ((1 1) on_tstring_content "foo"))
                       (Token ((1 4) on_regexp_end "/i")))])

      (check-equal? (lex-regex pattern end-lexing) expected)))

   (test-case
    "+lex-regex+ tokenizes a pattern with an encoding end delimiter"
    (let* ([pattern (open-input-string "/foo/u")]
           [expected `((Token ((1 0) on_regexp_beg "/"))
                       (Token ((1 1) on_tstring_content "foo"))
                       (Token ((1 4) on_regexp_end "/u")))])

      (check-equal? (lex-regex pattern end-lexing) expected)))

   (test-case
    "+lex-regex+ tokenizes a pattern with arbitrary amount of end delimiters"
    (let* ([pattern (open-input-string "/foo/iuesn")]
           [expected `((Token ((1 0) on_regexp_beg "/"))
                       (Token ((1 1) on_tstring_content "foo"))
                       (Token ((1 4) on_regexp_end "/iuesn")))])

      (check-equal? (lex-regex pattern end-lexing) expected)))))

(run-tests lexers-test)
