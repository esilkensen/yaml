;;;;;; yaml.rkt - YAML expressions.    -*- Mode: Racket -*-

#lang racket

(require racket/generic)

(provide
 (contract-out
  [yaml? (-> any/c boolean?)]
  [yaml-null (case-> (-> any/c) (-> any/c void?))]
  [yaml-struct? (-> any/c boolean?)]
  [gen->yaml (-> yaml-struct?
                 (listof (cons/c string? yaml?)))]
  [gen-order (-> yaml-struct? (listof string?))])
 yaml-struct)

(define yaml-null (make-parameter 'null))

(define (yaml? v)
  (or (equal? v (yaml-null))
      (string? v)
      (boolean? v)
      (exact-integer? v)
      (inexact-real? v)
      (date? v)
      (yaml-struct? v)
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

(define-generics yaml-struct
  (gen->yaml yaml-struct)
  (gen-order yaml-struct))

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
    [(_ id (field ...) struct-option ...)
     (let ([fs (map (λ (f)
                      `(cons ,(format "~a" (syntax->datum f))
                             ,(build-name #'id #'id "-" f)))
                    (syntax->list #'(field ...)))])
       #`(struct id (field ...) struct-option ...
                 #:methods gen:yaml-struct
                 [(define (gen->yaml id)
                    (map (λ (p)  `(,(car p) . ,((cdr p) id)))
                         (list #,@fs)))
                  (define (gen-order id)
                    (map car (list #,@fs)))]))]))

;;; idea: create some sort of static hash in this file that new
;;; structs register themselves with in the macro expansion.
;;; then in the constructor/representer/etc. and whatever code,
;;; pull the new structs in during (make) procedures
;;;
;;; will almost certainly have to go back and implement the
;;; multi-constructor stuff from pyyaml
