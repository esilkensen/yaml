;;;;;; yaml-expr.rkt - YAML expressions.    -*- Mode: Racket -*-

#lang racket

(require racket/generic)

(provide
 (contract-out
  [yaml? (any/c . -> . boolean?)]
  [yaml-null (case-> (-> any/c) (any/c . -> . void?))]
  [yaml-struct? (any/c . -> . boolean?)]
  [gen->yaml (yaml-struct? . -> . (listof (cons/c string? yaml?)))])
 yaml-struct
 yaml-struct-constructors)

(module+ test (require rackunit racket/date))

(define yaml-null (make-parameter 'null))

(define (yaml? v)
  (or (equal? v (yaml-null))
      (string? v)
      (boolean? v)
      (exact-integer? v)
      (inexact-real? v)
      (date? v)
      (bytes? v)
      (yaml-struct? v)
      (and (list? v)
           (andmap yaml? v))
      (and (hash? v)
           (for/and ([(key val) v])
             (and (yaml? key)
                  (yaml? val))))
      (and (set? v)
           (for/and ([val v])
             (yaml? val)))
      (and (pair? v)
           (yaml? (car v))
           (yaml? (cdr v)))))

(module+ test
  (test-case "yaml?"
    (define exprs (list (yaml-null) "string" #t 1 1.0 (current-date) #"bytes"))
    (check-true (yaml? '()))
    (check-true (yaml? (set)))
    (check-true (yaml? exprs))
    (check-true (yaml? (set exprs)))
    (check-true (yaml? (cons "first" "second")))
    (check-true (yaml? (make-hash)))
    (check-true (yaml? #hash(("key" . "value"))))
    (check-false (yaml? 'yaml?))))

(define-generics yaml-struct
  (gen->yaml yaml-struct)
  (gen-order yaml-struct))

(define yaml-struct-constructors (make-hash))

(define-syntax (yaml-struct stx)
  (define (build-name id . parts)
    (let ([str (apply string-append
                      (map (λ (p)
                             (if (syntax? p)
                                 (symbol->string (syntax-e p))
                                 (format "~a" p)))
                           parts))])
      (datum->syntax id (string->symbol str) id)))
  (syntax-case stx ()
    [(_ id super-id (field ...) struct-option ...)
     (and (identifier? #'id)
          (identifier? #'super-id))
     (let ([fs (map (λ (f)
                      `(cons ,(format "~a" (syntax->datum f))
                             ,(build-name #'id #'id "-" f)))
                    (syntax->list #'(field ...)))])
       (quasisyntax/loc stx
         (begin
           (let ([sid (symbol->string 'super-id)])
             (unless (hash-has-key? yaml-struct-constructors sid)
               (raise-syntax-error
                'yaml-struct
                (format "parent not a yaml-struct\n  at: ~a" 'super-id))))
           (struct id super-id (field ...) struct-option ...
             #:methods gen:yaml-struct
             [(define (gen->yaml id)
                (map (λ (p) `(,(car p) . ,((cdr p) id)))
                     (let ([sid (symbol->string 'super-id)])
                       (append
                        (cdr (hash-ref yaml-struct-constructors sid))
                        (list #,@fs)))))])
           (let* ([sid (symbol->string 'super-id)]
                  [sfs (cdr (hash-ref yaml-struct-constructors sid))])
             (hash-set!
              yaml-struct-constructors
              (symbol->string 'id)
              (cons id (append sfs (list #,@fs))))))))]
    [(_ id (field ...) struct-option ...)
     (identifier? #'id)
     (let ([fs (map (λ (f)
                      `(cons ,(format "~a" (syntax->datum f))
                             ,(build-name #'id #'id "-" f)))
                    (syntax->list #'(field ...)))])
       (quasisyntax/loc stx
         (begin
           (struct id (field ...) struct-option ...
             #:methods gen:yaml-struct
             [(define (gen->yaml id)
                (map (λ (p)  `(,(car p) . ,((cdr p) id)))
                     (list #,@fs)))])
           (hash-set!
            yaml-struct-constructors
            (symbol->string 'id)
            (cons id (list #,@fs))))))]
    [(_ thing . _)
     (raise-syntax-error
      #f "expected an identifier for the structure type name" stx #'thing)]))
