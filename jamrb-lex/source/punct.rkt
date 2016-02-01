;; punct.rkt
;;
;; By Jonathon McDonald
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "keywords.rkt"
         "utility.rkt")

(provide punct
         tokenize-punct!
         embexpr-terminator?
         seen-method-with-parens?
         set-seen-method-with-parens!
         punct->symbol)

(define _method-with-parens? #f)

;; Defines the lexer abbreviation for ruby-specific punctuation.
(define-lex-abbrev punct (:or #\. #\, #\( #\) #\{ #\} #\[ #\]))

;; (string) -> symbol
;;
;; Returns the token symbol for the given punctuation.
(define (punct->symbol value)
  (hash-ref punct-symbol-ht value))

;; () -> bool
;;
;; Returns a value indicating whether a method with parens has been declared.
(define (seen-method-with-parens?)
  _method-with-parens?)

;; (bool) -> bool
;;
;; Sets whether a method with parens has been seen, and returns the bool provided.
(define (set-seen-method-with-parens! bool)
  (set! _method-with-parens? bool)
  bool)

;; (string) -> bool
;;
;; Returns a value indicating whether the value is an embedded expression terminator.  A value is an
;; embedded expression terminator if it is "}" and there is no matching "{" at the top of the stack.
(define (embexpr-terminator? value)
  (and (equal? value "}")
       (or (not (punct-stack-any?)) (not (equal? (peek-punct) "{")))))

;; (string) -> bool
;;
;; Returns a value indicating whether the value is opening punctuation.
(define (punct-is-open? value)
  (hash-has-key? punct-pairs-ht value))

;; (string) -> bool
;;
;; Returns a value indicating whether the value should cause a push or pop on the stack.
(define (punct-is-stackable? value)
  (not (or (equal? value ".") (equal? value ","))))

;; (number, number, string) -> '()
;;
;; Returns the token for this punctuation, possibly altering the stack.
(define (tokenize-punct! line col value)
  (if (punct-is-stackable? value)
      (tokenize-stackable-punct! line col value)
      (tokenize line col (punct->symbol value) value)))

;; (number, number, string) -> '()
;;
;; Returns the token after either pushing the value to the stack, or popping the stack.
(define (tokenize-stackable-punct! line col value)
  (if (punct-is-open? value)
      (push-punct! value)
      (pop-punct-pair! value))

  (if (has-seen-def?)
      (set-seen-method-with-parens! #t)
      (set-seen-method-with-parens! #f))

  (tokenize line col (punct->symbol value) value))

;; Initializes a hash table which will contain the punctuation.
(define punct-symbol-ht (make-hash))

;; Defines the values for punctuation.
(hash-set*! punct-symbol-ht
            "." 'period
            "," 'comma
            "(" 'lparen
            ")" 'rparen
            "{" 'lbrace
            "}" 'rbrace
            "[" 'lbracket
            "]" 'rbracket)

;; Initializes a hash table of punctuation pairs.
(define punct-pairs-ht (make-hash))

;; Sets the values of punctuation pairs that we can match against in our punctuation stack.
(hash-set*! punct-pairs-ht
            "(" ")"
            "{" "}"
            "[" "]")

;; Initializes an empty list that we will treat as a stack.
(define punct-stack '())

;; () -> bool
;;
;; Returns a value indicating whether the stack has any characters.
(define (punct-stack-any?) (not (eq? (length punct-stack) 0)))

;; (string) -> '()
;;
;; Pushes to the stack and returns the current list of values in the stack.
(define (push-punct! value)
  (set! punct-stack (cons value punct-stack))
  punct-stack)

;; () -> string
;;
;; Returns the top of the stack.
(define (peek-punct)
  (car punct-stack))

;; () -> string
;;
;; Pops the stack and returns the result.
(define (pop-punct!)
  (define return-val (car punct-stack))
  (set! punct-stack (cdr punct-stack))
  return-val)

;; (string) -> string
;;
;; Pops the stack and returns the result.  Raises an error if the popped value is not a valid pair
;; with `close-value`.
(define (pop-punct-pair! close-value)
  (define return-val (car punct-stack))
  (cond [(not (equal? (hash-ref punct-pairs-ht return-val) close-value))
         (raise-syntax-error 'unexpected-token (format "Expecting ~a" return-val))])

  (set! punct-stack (cdr punct-stack))
  return-val)
