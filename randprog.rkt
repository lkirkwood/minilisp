#lang errortrace racket

(require "model.rkt")

(define (random-elem lst)
  (define chosen-pos (random (length lst)))
  (let next ([current-pos 0]
             [lst lst])
    (if (= chosen-pos current-pos)
        (car lst)
        (next (add1 current-pos) (cdr lst)))))

(define CONST-EXPR-CHOICES (list '(number) '(#\( paren-expr #\))))

(define (random-expr bound-idents)
  (random-elem (append CONST-EXPR-CHOICES (map list bound-idents))))

(define (unparse-expr tokens symbol-stack bound-idents)
  (if (empty? symbol-stack)
      (reverse tokens)

      (let ([symbol (car symbol-stack)]
            [symbol-stack (cdr symbol-stack)])
        ;; [displayln (cons symbol symbol-stack)]
        (match symbol

          ['expr (unparse-expr tokens (append (random-expr bound-idents) symbol-stack) bound-idents)]

          ['paren-expr
           (let* ([token (random-elem PAREN-EXPR-TOKENS)]
                  [symbol-stack (cons token symbol-stack)])
             (unparse-expr tokens symbol-stack bound-idents))]

          ['number (unparse-expr (cons (random 42) tokens) symbol-stack bound-idents)]

          ['identifier
           (let* ([ident (string-append "ident-" (number->string (length bound-idents)))])
             (unparse-expr (cons ident tokens) symbol-stack (cons ident bound-idents)))]

          [(? string? ident) (unparse-expr (cons ident tokens) symbol-stack bound-idents)]

          [#\⊢
           (let* ([to-match (car (random-evaluated-expr bound-idents))]
                  [to-match-result (car (random-evaluated-expr bound-idents))]
                  [unused-idents (remove to-match bound-idents)] ;; TODO: add _ support
                  [before (for/list ([_ (random 4)])
                            (append (random-evaluated-expr unused-idents)
                                    (random-evaluated-expr bound-idents)))]
                  [after (for/list ([_ (random 4)])
                           (append (random-evaluated-expr unused-idents)
                                   (random-evaluated-expr bound-idents)))]
                  [match-clauses (append before (list (list to-match to-match-result)) after)]
                  [tokens (append match-clauses (list to-match #\⊢) tokens)])
             (unparse-expr tokens symbol-stack bound-idents))]

          [(? paren-expr-token? token)
           (unparse-expr (cons token tokens)
                         (append (paren-expr-symbol token) symbol-stack)
                         bound-idents)]

          [(? single-char-token? token)
           (unparse-expr (cons token tokens) symbol-stack bound-idents)]))))

(define (random-evaluated-expr bound-idents)
  (unparse-expr (list) (list 'expr) bound-idents))

(define (random-program)
  (random-evaluated-expr (list)))

(define (random-program-string)
  (let ([str-with-parens (format "~a" (random-program))])
    (substring str-with-parens 1 (sub1 (string-length str-with-parens)))))

(module+ test
  (require rackunit)
  (require "interpreter.rkt")

  ;; random element
  (random-seed 6)
  (check-equal? (random-elem (list 0 1 2 3)) 0)

  (random-seed 5)
  (check-equal? (random-elem (list 0 1 2 3)) 1)

  (random-seed 1)
  (check-equal? (random-elem (list 0 1 2 3)) 2)

  (random-seed 3)
  (check-equal? (random-elem (list 0 1 2 3)) 3)

  ;; random program 1
  (random-seed 0)
  (check-equal? (random-program) '(#\( #\∘ 17 #\)))

  (random-seed 0)
  (check-equal? (random-program-string) "( ∘ 17 )")

  ;; random program 2
  (random-seed 42)
  (check-equal? (random-program) '(3))

  (random-seed 42)
  (check-equal? (random-program-string) "3")

  ;; semi-random with match
  ;; (≜ match-list
  ;;    (λ arg (⊢ ?
  ;;              ???
  ;;              ))
  ;;    ???)
  (random-seed 1)
  (displayln (unparse-expr (list #\( "arg" #\λ #\( "match-list" #\≜ #\()
                           (list #\⊢ #\) #\) 'expr #\))
                           (list "match-list" "arg")))

  ;;
  )

(provide random-program)
