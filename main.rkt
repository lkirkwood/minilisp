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
        (reverse tokens)

        (let ([char (car chars)])
          (match char
            [(or #\( #\) #\+ #\− #\× #\= #\? #\λ #\≜)
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
                 (raise (format "Found number in the middle of a ~a token" buffered-type)))]

            [(? char-alphabetic?)
             (if (or (eq? buffered-type 'none) (eq? buffered-type 'identifier))
                 (loop tokens (cons char token-buf) 'identifier (cdr chars))
                 (raise (format "Found alphanumeric character in the middle of a ~a token"
                                buffered-type)))]

            [#\space
             (match buffered-type
               ['none (loop tokens (list) 'none (cdr chars))]
               ['number (loop (cons (number-token token-buf) tokens) (list) 'none (cdr chars))]
               ['identifier
                (loop (cons (identifier-token token-buf) tokens) (list) 'none (cdr chars))])])))))

(define (parse-paren-expr tokens stack)
  (let loop ([paren-expr (list)]
             [tokens tokens]
             [stack stack])
    (cond
      [(and (null? tokens) (eq? '$ (car stack))) (car paren-expr)]
      [(null? tokens) (raise "The program being parsed ended unexpectedly.")]

      [else
       (let ([token (car tokens)]
             [tokens (cdr tokens)]
             [symbol (car stack)]
             [stack (cdr stack)])
         (match (list token symbol)
           ;; recursively parse paren-expr
           [(list #\( 'expr)
            (let*-values ([(stack) (cons 'paren-expr (cons #\) stack))]
                          [(sub-paren-expr tokens stack) (parse-paren-expr tokens stack)])
              (loop (cons sub-paren-expr paren-expr) tokens stack))]

           ;; terminals
           [(list (? number? number) 'expr) (loop (cons number paren-expr) tokens stack)]
           [(list (? string? identifier) 'expr)
            (loop (cons (string->symbol identifier) paren-expr) tokens stack)]

           [(list #\+ 'paren-expr) (loop (cons + paren-expr) tokens (cons 'expr (cons 'expr stack)))]
           [(list #\− 'paren-expr) (loop (cons - paren-expr) tokens (cons 'expr (cons 'expr stack)))]
           [(list #\× 'paren-expr) (loop (cons * paren-expr) tokens (cons 'expr (cons 'expr stack)))]
           [(list #\= 'paren-expr)
            (loop (cons equal? paren-expr) tokens (cons 'expr (cons 'expr stack)))]

           [(list #\) #\)) (values (reverse paren-expr) tokens stack)]))])))

(define (parse program-string)
  (parse-paren-expr (tokenise program-string) (list 'expr '$)))

(define (run program-string)
  (eval (parse program-string) (make-base-namespace)))

(module+ test
  (require rackunit)

  (check-equal? (tokenise "(+ 123 456)") (list #\( #\+ 123 456 #\)))
  (check-equal? (tokenise "(+ (× 1 42) (− 42 0))")
                (list #\( #\+ #\( #\× 1 42 #\) #\( #\− 42 0 #\) #\)))
  (check-equal? (tokenise "(≜ myident (× 42 100))") (list #\( #\≜ "myident" #\( #\× 42 100 #\) #\)))

  (check-equal? (run "(+ 1 42)") 43)
  (check-equal? (run "(− 123 23)") 100)
  (check-equal? (run "(× 5 25)") 125)
  (check-equal? (run "(= 42 1)") #f))

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
