#lang racket

(module+ main
  (require racket/cmdline)
  (require "interpreter.rkt")
  (command-line #:program "minilisp"
                #:args (progfile-path)
                (run (port->string (open-input-file progfile-path)))))
