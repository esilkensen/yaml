;;;;;; yaml-expr.rkt - YAML expressions.    -*- Mode: Racket -*-

#lang racket

(require racket/generic)

(provide
 (contract-out
  [yaml? (any/c . -> . boolean?)]
  [yaml-null (parameter/c any/c)]
  [yaml-types (parameter/c (listof (any/c . -> . boolean?)))]
  [yaml-struct? (any/c . -> . boolean?)]
  [gen->yaml (yaml-struct? . -> . (listof (cons/c string? yaml?)))])
 yaml-struct
 yaml-struct-constructors)

(module+ test (require rackunit racket/date))

(define yaml-null (make-parameter 'null))

(define yaml-types (make-parameter '()))

(define (yaml? v)
  (or (equal? v (yaml-null))
      (string? v)
      (bytes? v)
      (boolean? v)
      (exact-integer? v)
      (inexact-real? v)
      (and (list? v)
           (andmap yaml? v))
      (and (hash? v)
           (for/and ([(key val) v])
             (and (yaml? key)
                  (yaml? val))))
      (and (set? v)
           (for/and ([val v])
             (yaml? val)))
      (date? v)
      (and (pair? v)
           (yaml? (car v))
           (yaml? (cdr v)))
      (yaml-struct? v)
      (for/or ([type? (yaml-types)])
        (type? v))))

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

(module+ test
  (yaml-struct player (name hr avg) #:transparent)
  (yaml-struct pitcher player (era) #:transparent)

  (define p1 (player "Carlos González" 34 0.336))
  (check-equal? (player-name p1) "Carlos González")
  (check-equal? (player-hr p1) 34)
  
  (check-equal?
   (gen->yaml p1)
   '(("name" . "Carlos González")
     ("hr" . 34)
     ("avg" . 0.336)))
  
  (define p2 (pitcher "Ubaldo Jiménez" 0 0.104 2.88))
  (check-equal? (player-name p2) "Ubaldo Jiménez")
  (check-equal? (pitcher-era p2) 2.88)

  (check-equal?
   (gen->yaml p2)
   '(("name" . "Ubaldo Jiménez")
     ("hr" . 0)
     ("avg" . 0.104)
     ("era" . 2.88))))
