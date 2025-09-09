#lang racket

(define (parse program-string)
  (let loop ([program (list)]
             [stack (list 'S '$)]
             [chars (string->list program-string)])
    (let ([stack-top (car stack)]
          [cur-char (car chars)])
      (if (eq? cur-char stack-top)
          (loop program (cdr stack) (cdr chars))

          (match (list cur-char stack-top)
            [(list #\( 'S) (loop program (append (list #\( 'paren-expr #\)) stack) (cdr chars))]
            [(list #\space _) (loop program stack)]
            [(list #\( 'expr) (loop program (append (list #\( 'paren-expr #\)) stack) (cdr chars))]
            [(list)])))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.
  (require rackunit)

  (check-equal? (+ 2 2) 4))

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
