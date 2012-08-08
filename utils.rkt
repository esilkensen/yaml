;;;;;; utils.rkt - Utilities.    -*- Mode: Racket -*-

#lang racket

(provide (all-defined-out))

(define-syntax-rule (append! dst lst ...)
  (set! dst (append dst lst ...)))

(define-syntax-rule (pop! lst)
  (begin0 (last lst)
    (set! lst (drop-right lst 1))))

