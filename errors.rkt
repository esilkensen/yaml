;;;;;; errors.rkt - YAML errors.    -*- Mode: Racket -*-

#lang typed/racket

(provide (all-defined-out))

(struct: mark
  ([name : Any] [index : Integer] [line : Integer]
   [column : Integer] [buffer : (Vectorof (U Char EOF))]))

(: make-error (Symbol -> ((Option String) String mark -> Nothing)))
(define (make-error type)
  (λ (context problem problem-mark)
    (error type "~a~a\n~a:~a:~a: ~a"
           (if (string? context)
               (format "~a;\n " context) "")
           problem
           (mark-name problem-mark)
           (mark-line problem-mark)
           (mark-column problem-mark)
           (vector-ref
            (mark-buffer problem-mark)
            (mark-index problem-mark)))))
