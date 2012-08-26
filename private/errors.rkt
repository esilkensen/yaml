;;;;;; errors.rkt - YAML errors.    -*- Mode: Racket -*-

#lang typed/racket

(provide (all-defined-out))

(struct: mark
  ([name : Any] [index : Integer] [line : Integer]
   [column : Integer] [buffer : (Vectorof (U Char EOF))]))

(: make-error (Symbol -> ((Option String) String mark -> Nothing)))
(define (make-error type)
  (λ (context problem problem-mark)
    (raise-user-error
     (string->symbol
      (format
       "~a:~a:~a"
       (mark-name problem-mark)
       (mark-line problem-mark)
       (mark-column problem-mark)))
     "~a~a"
     (if (string? context)
         (format "~a;\n " context) "")
     problem)))
