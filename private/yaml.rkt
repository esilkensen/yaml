;;;;;; yaml.rkt - YAML expressions.    -*- Mode: Racket -*-

#lang racket

(provide
 (contract-out
  [yaml? (-> any/c boolean?)]
  [yaml-null (case-> (-> any/c) (-> any/c void?))]))

(define yaml-null (make-parameter 'null))

(define (yaml? v)
  (or (equal? v (yaml-null))
      (string? v)
      (boolean? v)
      (exact-integer? v)
      (inexact-real? v)
      (date? v)
      (and (list? v)
           (not (null? v))
           (andmap yaml? v))
      (and (hash? v)
           (not (zero? (hash-count v)))
           (for/and ([(key val) v])
             (and (yaml? key)
                  (yaml? val))))
      (and (set? v)
           (not (set-empty? v))
           (for/and ([val v])
             (yaml? val)))
      (and (pair? v)
           (yaml? (car v))
           (yaml? (cdr v)))))
