;; utilities.test.rkt
;;
;; "Science is what we understand well enough to explain to a computer. Art is
;; everything else we do."
;;
;; By Jonathon McDonald
#lang racket
(require rackunit rackunit/text-ui "../source/utility.rkt")

(define utility-tests
  (test-suite
   "Tests for utility.rkt"

   (test-case
    "+tokenize+ enforces parameter contract"
    (let* ([invalid-line-call (λ () (tokenize "h" 0 'foo "value"))]
           [invalid-col-call (λ () (tokenize 1 #f 'foo "value"))]
           [invalid-key-call (λ () (tokenize 1 0 "foo" "value"))]
           [invalid-value-call (λ () (tokenize 1 0 'foo 'value))]
           [valid-call (λ () (tokenize 1 0 'foo "value"))])

      (check-exn exn:fail? invalid-line-call)
      (check-exn exn:fail? invalid-col-call)
      (check-exn exn:fail? invalid-key-call)
      (check-exn exn:fail? invalid-value-call)
      (check-not-exn valid-call)))

   (test-case
    "+tokenize+ returns a list in the correct format"
    (let* ([result (tokenize 1 0 'ident "title")]
           [expected `(Token ((1 0) on_ident "title"))])

      (check-equal? result expected)))

   (test-case
    "+tokenize-cons+ enforces parameter contract"
    (let* ([valid-callback (λ () (void))]
           [invalid-callback-call (λ ()
                                    (tokenize-cons 1 1 'foo "value" "invalid"))]
           [valid-call (λ () (tokenize-cons 1 1 'foo "value" valid-callback))])

      (check-exn exn:fail? invalid-callback-call)
      (check-not-exn valid-call)))

   (test-case
    "+tokenize-cons+ returns a list"
    (let* ([callback (λ () '())]
           [expected `((Token ((1 0) on_ident "title")))]
           [result (tokenize-cons 1 0 'ident "title" callback)])

      (check-equal? result expected)))

   (test-case
    "+normalize-token-key+ returns a symbol prefixed with 'on_'"
    (let* ([result (normalize-token-key 'ident)]
           [expected 'on_ident])

      (check-equal? result expected)))

   (test-case
    "+unget+ enforces parameter contract"
    (let* ([invalid-port-call (λ () (unget "food"))]
           [invalid-count-call (λ () (unget (open-input-string "food") -1))]
           [valid-call (λ () (unget (open-input-string "food") 1))])

      (check-exn exn:fail? invalid-port-call)
      (check-exn exn:fail? invalid-count-call)
      (check-not-exn valid-call)))

   (test-case
    "+unget+ returns the port it is given"
    (let* ([expected (open-input-string "food")]
           [result (unget expected)])

      (check-equal? result expected)))

   (test-case
    "+unget-with-position!+ enforces parameter contract"
    (let* ([simple-port (open-input-string "test")]
           [invalid-port-call (λ () (unget-with-position! "test" 1 0))]
           [invalid-line-call (λ () (unget-with-position! simple-port "1" 0))]
           [invalid-col-call (λ () (unget-with-position! simple-port 1 "0"))]
           [invalid-count-call (λ ()
                                 (unget-with-position! simple-port 1 0 "1"))]
           [valid-call (λ () (unget-with-position! simple-port 1 1))])

      (check-exn exn:fail? invalid-port-call)
      (check-exn exn:fail? invalid-line-call)
      (check-exn exn:fail? invalid-col-call)
      (check-exn exn:fail? invalid-count-call)
      (check-not-exn valid-call)))

   (test-case
    "+unget-with-position!+ returns the port it was given"
    (let* ([expected (open-input-string "test")]
           [result (unget-with-position! expected 1 1)])

      (check-equal? result expected)))

   (test-case
    "+watch-port-position!+ enforces parameter contract"
    (let* ([invalid-port-call (λ () (watch-port-position! "test"))]
           [valid-call (λ ()
                         (watch-port-position! (open-input-string "test")))])

      (check-exn exn:fail? invalid-port-call)
      (check-not-exn valid-call)))

   (test-case
    "+watch-port-position!+ returns the current line and column"
    (define simple-port (open-input-string "test"))
    (let-values ([(line col) (watch-port-position! simple-port)])

      (check-equal? line 1)
      (check-equal? col 0)))

   (test-case
    "+prepare-port-rewinder+ enforces parameter contract"
    (let* ([simple-port (open-input-string "test")]
           [invalid-port-call (λ () (prepare-port-rewinder "food" 1 1))]
           [invalid-line-call (λ () (prepare-port-rewinder simple-port 0 1))]
           [invalid-col-call (λ () (prepare-port-rewinder simple-port 1 "1"))]
           [valid-call (λ () (prepare-port-rewinder simple-port 1 1))])

      (check-exn exn:fail? invalid-port-call)
      (check-exn exn:fail? invalid-line-call)
      (check-exn exn:fail? invalid-col-call)
      (check-not-exn valid-call)))

   (test-case
    "+prepare-port-rewinder+ returns a procedure"
    (let* ([simple-port (open-input-string "test")]
           [result (prepare-port-rewinder simple-port 1 1)])

      (check-true (procedure? result))))

   (test-case
    "+end-newline?+ enforces parameter contract"
    (let* ([invalid-string-call (λ () (end-newline? 4))]
           [invalid-call-with-null (λ () (end-newline? null))]
           [invalid-ignore-spaces-call (λ () (end-newline? "" "yes"))]
           [valid-call (λ () (end-newline? ""))]
           [valid-call-with-opt (λ () (end-newline? "" #t))])

      (check-exn exn:fail? invalid-string-call)
      (check-exn exn:fail? invalid-call-with-null)
      (check-exn exn:fail? invalid-ignore-spaces-call)
      (check-not-exn valid-call)
      (check-not-exn valid-call-with-opt)))

   (test-case
    "+end-newline?+ returns value indicating whether string end is newline"
    (let* ([nl-str1 "Simple nl str \n"]
           [nl-str2 "So many nl...\n This is another \n\n\n\n"]
           [empty ""]
           [close "Simple nl str \nn"]
           [almost "\\n"])

    (check-true (end-newline? nl-str1))
    (check-true (end-newline? nl-str2))
    (check-false (end-newline? empty))
    (check-false (end-newline? close))
    (check-false (end-newline? almost))))

   (test-case
    "+end-newline?+ by default respects trailing spaces"
    (let* ([str-with-spaces "So I'm space terminated \n "]
           [str-with-tabs "So I'm space terminated \n\t"])

      (check-false (end-newline? str-with-spaces))
      (check-false (end-newline? str-with-tabs))))

   (test-case
    "+end-newline?+ will ignore trailing spaces if specifically set"
    (let* ([str-with-spaces "So I'm space terminated \n "]
           [str-with-tabs "So I'm space termianted \n\t"])

      (check-true (end-newline? str-with-spaces #t))
      (check-true (end-newline? str-with-tabs #t))))

   (test-case
    "+string->nl-tail+ enforces parameter contract"
    (let* ([invalid-str-call (λ () (string->nl-tail #f))]
           [invalid-call-with-null (λ () (string->nl-tail null))]
           [valid-call (λ () (string->nl-tail ""))])

      (check-exn exn:fail? invalid-str-call)
      (check-exn exn:fail? invalid-call-with-null)
      (check-not-exn valid-call)))

   (test-case
    "+string->nl-tail+ returns empty string if no newline"
    (let* ([empty ""]
           [simple "this is a simple string."])

      (check-equal? (string->nl-tail empty) "")
      (check-equal? (string->nl-tail simple) "")))

   (test-case
    "+string->nl-tail+ returns all characters following the last newline"
    (let* ([simple "this is a simple string\n"]
           [simple-with-spaces "this is a simple string\n   "]
           [tricky "this is a less simple\n string...\n ... \n..."])

      (check-equal? (string->nl-tail simple) "")
      (check-equal? (string->nl-tail simple-with-spaces) "   ")
      (check-equal? (string->nl-tail tricky) "...")))))

(run-tests utility-tests)
