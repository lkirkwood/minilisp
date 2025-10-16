#lang racket

(require "model.rkt")

(define (number-token chars)
  (define-values (total _)
    (for/fold ([total 0]
               [place-value 1])
              ([char chars])
      (define num (- (char->integer char) 48))
      (values (+ total (* place-value num)) (* 10 place-value))))
  total)

(define (identifier-token chars)
  (list->string (reverse chars)))

(define (flush-token-buf tokens buf buf-type)
  (match buf-type
    ['none tokens]
    ['number (cons (number-token buf) tokens)]
    ['identifier (cons (identifier-token buf) tokens)]))

(define (tokenise program-string)
  (let loop ([tokens (list)]
             [token-buf (list)]
             [buffered-type 'none]
             [chars (string->list program-string)])
    (define (push-token)
      (flush-token-buf tokens token-buf buffered-type))

    (if (null? chars)
        (reverse (push-token))

        (let ([char (car chars)]
              [chars (cdr chars)])

          (match char
            [(? single-char-token?) (loop (cons char (push-token)) (list) 'none chars)]

            [(? char-numeric?)
             (if (or (eq? buffered-type 'none) (eq? buffered-type 'number))
                 (loop tokens (cons char token-buf) 'number chars)
                 (error (format "Found number in the middle of a ~a token" buffered-type)))]

            [(or (? char-alphabetic?) #\- #\_)
             (if (or (eq? buffered-type 'none) (eq? buffered-type 'identifier))
                 (loop tokens (cons char token-buf) 'identifier chars)
                 (error (format "Found alphanumeric character in the middle of a ~a token"
                                buffered-type)))]

            [(? char-whitespace?) (loop (push-token) (list) 'none chars)]

            [_
             (error (format "The program being parsed contains an invalid character \"~a\" (U+~a)"
                            char
                            (string-upcase (number->string (char->integer char) 16))))])))))

(define (turing-combinator f)
  ((lambda (x) (x x)) (lambda (g) (f (lambda (arg) ((g g) arg))))))

(define (push stack . elements)
  (append elements stack))

(define (parse-expr expr tokens stack)
  (cond
    [(and (null? tokens) (eq? '$ (car stack))) (car expr)]
    [(null? tokens) (error "The program being parsed ended unexpectedly.")]

    [else
     (let ([token (car tokens)]
           [tokens (cdr tokens)]
           [symbol (car stack)]
           [stack (cdr stack)])
       (match (list token symbol)

         ;; simple expressions
         [(list (? number? number) 'expr) (parse-expr (cons number expr) tokens stack)]

         [(list (? string? identifier) (or 'identifier 'expr))
          (parse-expr (cons (string->symbol identifier) expr) tokens stack)]

         [(list #\∅ 'expr) (parse-expr (cons '(list) expr) tokens stack)]

         ;; creating and exiting inner scopes
         [(list #\( 'expr)
          (let*-values ([(stack) (push stack 'paren-expr #\))]
                        [(sub-expr tokens stack) (parse-expr (list) tokens stack)])
            (parse-expr (cons sub-expr expr) tokens stack))]

         [(list #\λ 'paren-expr)
          (let*-values ([(expr) (cons 'lambda expr)]
                        [(stack) (push stack 'identifier 'end-form 'expr)]
                        [(binding-form tokens stack) (parse-expr (list) tokens stack)])
            (parse-expr (cons binding-form expr) tokens stack))]

         [(list #\≜ 'paren-expr)
          (let*-values ([(expr) (cons 'let expr)]
                        [(stack) (push stack 'identifier 'expr 'end-form 'expr)]
                        [(binding-form tokens stack) (parse-expr (list) tokens stack)])
            (parse-expr (cons (list binding-form) expr) tokens stack))]

         [(list _ 'end-form) (values (reverse expr) (cons token tokens) stack)]

         ;; paren expressions
         [(list (? paren-expr-token?) 'paren-expr)
          (parse-expr (cons (paren-expr-literal token) expr)
                      tokens
                      (apply push stack (paren-expr-symbol token)))]

         [(list _ 'paren-expr) (parse-expr expr (cons token tokens) (push stack 'expr))]

         ;; closing parens
         [(list #\) #\)) (values (reverse expr) tokens stack)]
         [(list _ #\)) (parse-expr expr (cons token tokens) (push stack 'expr #\)))]

         ;; pattern matching
         [(list #\( 'match-clause)
          (let*-values ([(stack) (push stack 'expr 'expr 'end-form)]
                        [(match-clause tokens stack) (parse-expr (list) tokens stack)])
            (parse-expr (cons match-clause expr) tokens (push stack 'match-clause)))]

         [(list #\) 'match-clause) (parse-expr expr tokens stack)]

         [(list #\_ 'expr) (parse-expr (cons '_ expr) tokens stack)]

         ;; something went wrong
         [_ (error (format "Found \"~a\" when looking for a \"~a\"" token symbol))]))]))

(define (parse program-string)
  (parse-expr (list) (tokenise program-string) (list 'expr '$)))

(define-namespace-anchor minilisp)
(define minilisp-ns (namespace-anchor->namespace minilisp))

(define (run program-string)
  (eval (parse program-string) minilisp-ns))

(module+ test
  (require rackunit)

  (check-equal? (tokenise "(+ 123 456)") (list #\( #\+ 123 456 #\)))
  (check-equal? (tokenise "(+ (× 1 42) (− 42 0))")
                (list #\( #\+ #\( #\× 1 42 #\) #\( #\− 42 0 #\) #\)))
  (check-equal? (tokenise "(≜ myident (× 42 100))") (list #\( #\≜ "myident" #\( #\× 42 100 #\) #\)))
  (check-equal? (tokenise "42") (list 42))
  (check-exn exn:fail? (lambda () (tokenise "(⌒)")))

  (check-equal? (run "42") 42)
  (check-equal? (run "(+ 1 42)") 43)
  (check-equal? (run "(+ 1 23 42)") 66)
  (check-equal? (run "(− 123 23)") 100)
  (check-equal? (run "(× 5 25)") 125)
  (check-equal? (run "(× (+ 1 2) (− 7 3))") 12)
  (check-equal? (run "(= 42 1)") #f)
  (check-equal? (run "((λ x (+ x 1)) 5)") 6)
  (check-equal? (run "(≜ x 10 (+ x x))") 20)
  (check-equal?
   (run
    "(≜ Y (λ f ((λ x (f (λ v ((x x) v)))) (λ x (f (λ v ((x x) v))))))
      (≜ factorial (Y (λ f (λ n (? (= n 0) 1 (× n (f (− n 1)))))))
          (factorial 5)))")
   120)

  (check-exn exn:fail? (lambda () (run "+ 42 53")))
  (check-exn exn:fail? (lambda () (run "(+ 42 53 −)")))
  (check-equal? (run "(+ 42 53)") 95)

  (check-exn exn:fail? (lambda () (run "(? (= 42 0) 1 0 999)")))
  (check-equal? (run "(? (= 42 0) 1 0)") 0)

  (define factorial
    (turing-combinator (lambda (self)
                         (lambda (n)
                           (if (zero? n)
                               1
                               (* n (self (sub1 n))))))))

  (check-equal? (factorial 5) 120)
  (check-equal?
   (run "(≜ factorial
        (Ω (λ f (λ n (? (= n 0) 1 (× n (f (− n 1)))))))
        (factorial 5))")
   120)

  (check-equal? (run "(∷ 1 (∷ 2 (∷ 3 ∅)))") (list 1 2 3))
  (check-equal? (run "(← (∷ 42 100))") 42)
  (check-equal? (run "(→ (∷ 42 100))") 100)
  (check-equal? (run "(∘ ∅)") #t)
  (check-equal? (run "(∘ (∷ 1 ∅))") #f)

  (check-equal?
   (run
    "(≜ length
        (Ω (λ f (λ lst (? (∘ lst) 0 (+ 1 (f (→ lst)))))))
        (length (∷ 1 (∷ 2 (∷ 3 ∅)))))")
   3)

  (check-equal?
   (run
    "(≜ first-or-default
        (λ lst (⊢ lst
            (∅ 0)
            ((∷ x _) x)))
        (first-or-default (∷ 42 ∅)))")
   42)

  (check-equal?
   (run
    "(≜ sum
        (Ω (λ f (λ lst (⊢ lst
            (∅ 0)
            ((∷ x xs) (+ x (f xs)))))))
        (sum (∷ 1 (∷ 2 (∷ 3 ∅)))))")
   6)
  (check-equal? (run "(∧ (› 5 3) (‹ 2 4))") #t)
  (check-equal? (run "(∨ (= 1 0) (› 10 5))") #t)
  (check-equal? (run "(¬ (= 5 3))") #t))

(module+ main
  (require racket/cmdline)
  (command-line #:program "minilisp"
                #:args (progfile-path)
                (run (port->string (open-input-file progfile-path)))))
