;;;;;; yaml.rkt - YAML expressions.    -*- Mode: Racket -*-

#lang racket

(require racket/generic)

(provide
 (contract-out
  [yaml? (-> any/c boolean?)]
  [yaml-null (case-> (-> any/c) (-> any/c void?))]
  [struct-yaml? (-> any/c boolean?)]
  [gen->yaml (-> struct-yaml?
                 (listof (cons/c string? yaml?)))])
 struct-yaml
 struct-yaml-constructors)

(define yaml-null (make-parameter 'null))

(define (yaml? v)
  (or (equal? v (yaml-null))
      (string? v)
      (boolean? v)
      (exact-integer? v)
      (inexact-real? v)
      (date? v)
      (struct-yaml? v)
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

(define-generics struct-yaml
  (gen->yaml struct-yaml)
  (gen-order struct-yaml))

(define struct-yaml-constructors (make-hash))

(define-syntax (struct-yaml stx)
  (define (build-name id . parts)
    (let ([str (apply string-append
                      (map (λ (p)
                             (if (syntax? p)
                                 (symbol->string (syntax-e p))
                                 (format "~a" p)))
                           parts))])
      (datum->syntax id (string->symbol str) id)))
  (syntax-case stx ()
    [(_ id (field ...) struct-option ...)
     (let ([fs (map (λ (f)
                      `(cons ,(format "~a" (syntax->datum f))
                             ,(build-name #'id #'id "-" f)))
                    (syntax->list #'(field ...)))])
       #`(begin
           (struct id (field ...) struct-option ...
                   #:methods gen:struct-yaml
                   [(define (gen->yaml id)
                      (map (λ (p)  `(,(car p) . ,((cdr p) id)))
                           (list #,@fs)))])
           (hash-set!
            struct-yaml-constructors
            (symbol->string 'id)
            (cons id (map car (list #,@fs))))))]))
