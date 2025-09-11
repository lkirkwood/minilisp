#lang racket

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

(define (tokenise program-string)
  (let loop ([tokens (list)]
             [token-buf (list)]
             [buffered-type 'none]
             [chars (string->list program-string)])
    (if (null? chars)
        (match buffered-type
          ['none (reverse tokens)]
          ['number (reverse (cons (number-token token-buf) tokens))]
          ['identifier (reverse (cons (identifier-token token-buf) tokens))])

        (let ([char (car chars)])
          (match char
            [(or #\( #\) #\+ #\− #\× #\= #\? #\λ #\≜ #\Ω #\∷ #\← #\→ #\∅ #\∘)
             (match buffered-type
               ['none (loop (cons char tokens) (list) 'none (cdr chars))]
               ['number
                (loop (cons char (cons (number-token token-buf) tokens)) (list) 'none (cdr chars))]
               ['identifier
                (loop (cons char (cons (identifier-token token-buf) tokens))
                      (list)
                      'none
                      (cdr chars))])]

            [(? char-numeric?)
             (if (or (eq? buffered-type 'none) (eq? buffered-type 'number))
                 (loop tokens (cons char token-buf) 'number (cdr chars))
                 (error (format "Found number in the middle of a ~a token" buffered-type)))]

            [(or (? char-alphabetic?) #\- #\_)
             (if (or (eq? buffered-type 'none) (eq? buffered-type 'identifier))
                 (loop tokens (cons char token-buf) 'identifier (cdr chars))
                 (error (format "Found alphanumeric character in the middle of a ~a token"
                                buffered-type)))]

            [(? char-whitespace?)
             (match buffered-type
               ['none (loop tokens (list) 'none (cdr chars))]
               ['number (loop (cons (number-token token-buf) tokens) (list) 'none (cdr chars))]
               ['identifier
                (loop (cons (identifier-token token-buf) tokens) (list) 'none (cdr chars))])]

            [else
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
         [(list #\( 'expr)
          (let*-values ([(stack) (cons 'paren-expr (cons #\) stack))]
                        [(sub-expr tokens stack) (parse-expr (list) tokens stack)])
            (parse-expr (cons sub-expr expr) tokens stack))]

         [(list (? number? number) (or 'pattern 'expr)) (parse-expr (cons number expr) tokens stack)]
         [(list (? string? identifier) (or 'pattern 'identifier 'expr))
          (parse-expr (cons (string->symbol identifier) expr) tokens stack)]

         [(list #\+ 'paren-expr) (parse-expr (cons + expr) tokens (push stack 'expr 'expr))]
         [(list #\− 'paren-expr) (parse-expr (cons - expr) tokens (push stack 'expr 'expr))]
         [(list #\× 'paren-expr) (parse-expr (cons * expr) tokens (push stack 'expr 'expr))]
         [(list #\= 'paren-expr) (parse-expr (cons equal? expr) tokens (push stack 'expr 'expr))]

         [(list #\? 'paren-expr) (parse-expr (cons 'if expr) tokens (push stack 'expr 'expr))]

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

         [(list #\Ω 'paren-expr)
          (parse-expr (cons 'turing-combinator expr) tokens (push stack 'expr))]

         [(list #\∷ 'paren-expr) (parse-expr (cons 'cons expr) tokens (push stack 'expr 'expr))]
         [(list #\← 'paren-expr) (parse-expr (cons 'car expr) tokens (push stack 'expr))]
         [(list #\→ 'paren-expr) (parse-expr (cons 'cdr expr) tokens (push stack 'expr))]

         [(list #\∅ 'expr) (parse-expr (cons 'null expr) tokens stack)]
         [(list #\∘ 'paren-expr) (parse-expr (cons 'null? expr) tokens (push stack 'expr))]

         [(list _ 'paren-expr) (parse-expr expr (cons token tokens) (push stack 'expr))]

         [(list _ 'end-form) (values (reverse expr) (cons token tokens) stack)]

         [(list #\) #\)) (values (reverse expr) tokens stack)]
         [(list _ #\)) (parse-expr expr (cons token tokens) (push stack 'expr #\)))]

         [else (error (format "Found \"~a\" when looking for a \"~a\"" token symbol))]))]))

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
   (run
    "(≜ factorial
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
   3))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line #:program "my-program"
                #:once-each [("-n" "--name") name "Who to say hello to" (set-box! who name)]
                #:args ()
                (printf "hello ~a~n" (unbox who))))
