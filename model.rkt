#lang racket

(define TERMINAL-CHARS
  (list #\+ #\− #\× #\= #\? #\λ #\≜ #\Ω #\∷ #\← #\→ #\∅ #\∘ #\⊢ #\_ #\‹ #\› #\∧ #\∨ #\¬))

(define SINGLE-CHAR-TOKENS (append TERMINAL-CHARS (list #\( #\))))
(provide TERMINAL-CHARS
         SINGLE-CHAR-TOKENS)
