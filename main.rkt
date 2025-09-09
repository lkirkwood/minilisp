#lang racket

(define (stack-push stack . elements)
  (append elements stack))

;; TERMINALS:
;; (, ), +, −, =, ?, ×, λ, ≜
(define TERMINALS (set #\( #\) #\+ #\− #\= #\? #\× #\λ #\≜))

(define (number-token chars)
  (define-values (total _)
    (for/fold ([total 0]
               [place-value 1])
              ([char chars])
      (define num (- (char->integer char) 48))
      (values (+ total (* place-value num)) (* 10 place-value))))
  total)

(define (identifier-token chars)
  (string->symbol (list->string (reverse chars))))

(define (tokenise program-string)
  (let loop ([tokens (list)]
             [token-buf (list)]
             [buffered-type null]
             [chars (string->list program-string)])
    (if (null? chars)
        (reverse tokens)

        (match (car chars)
          [(? (curry set-member? TERMINALS) char)
           (match buffered-type
             [(? null?) (loop (cons char tokens) (list) null (cdr chars))]
             ['number
              (loop (cons char (cons (number-token token-buf) tokens)) (list) null (cdr chars))]
             ['identifier
              (loop (cons char (cons (identifier-token token-buf) tokens)) (list) null (cdr chars))])]

          [(? char-numeric? char)
           (if (or (eq? buffered-type null) (eq? buffered-type 'number))
               (loop tokens (cons char token-buf) 'number (cdr chars))
               (raise (format "Found number in the middle of a ~a token" buffered-type)))]

          [(? char-alphabetic? char)
           (if (or (eq? buffered-type null) (eq? buffered-type 'identifier))
               (loop tokens (cons char token-buf) 'identifier (cdr chars))
               (raise (format "Found alphanumeric character in the middle of a ~a token"
                              buffered-type)))]

          [#\space
           (match buffered-type
             [(? null?) (loop tokens (list) null (cdr chars))]
             ['number (loop (cons (number-token token-buf) tokens) (list) null (cdr chars))]
             ['identifier
              (loop (cons (identifier-token token-buf) tokens) (list) null (cdr chars))])]))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.
  (require rackunit)

  (check-equal? (tokenise "(+ 123 456)") (list #\( #\+ 123 456 #\)))
  (check-equal? (tokenise "(+ (× 1 42) (− 42 0))")
                (list #\( #\+ #\( #\× 1 42 #\) #\( #\− 42 0 #\) #\)))
  (check-equal? (tokenise "(≜ myident (× 42 100))") (list #\( #\≜ 'myident #\( #\× 42 100 #\) #\))))

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
