;;;;;; tokens.rkt - YAML tokens.    -*- Mode: Racket -*-

#lang racket

(provide (all-defined-out))

(struct token (type id start end attrs))

(define-syntax (define-token stx)
  (define (build-name id . parts)
    (let ([str (apply string-append
                      (map (λ (p)
                             (if (syntax? p)
                                 (symbol->string (syntax-e p))
                                 (format "~a" p)))
                           parts))])
      (datum->syntax id (string->symbol str) id)))
  (syntax-case stx ()
    [(_ name field ... id)
     (let ([t (build-name #'name #'name "-token")]
           [t? (build-name #'name #'name "-token?")]
           [fs (map (λ (f)
                      (build-name #'name #'name "-token-" f))
                    (syntax->list #'(field ...)))])
       #`(begin
           (define (#,t start end field ...)
             (let ([attrs (make-hash `((field . ,field) ...))])
               (token 'name id start end attrs)))
           (define (#,t? token)
             (and (token? token)
                  (eq? 'name (token-id token))))
           (define-values (#,@fs)
             (values
              (λ (token)
                (hash-ref (token-attrs token) 'field)) ...))))]))

(define-token directive name value "<directive>")
(define-token document-start "<document start>")
(define-token document-end "<document end>")
(define-token stream-start "<stream start>")
(define-token stream-end "<stream end>")
(define-token block-sequence-start "<block sequence start>")
(define-token block-mapping-start "<block mapping start>")
(define-token block-end "<block end>")
(define-token flow-sequence-start "[")
(define-token flow-mapping-start "{")
(define-token flow-sequence-end "]")
(define-token flow-mapping-end "}")
(define-token key "?")
(define-token value ":")
(define-token block-entry "-")
(define-token flow-entry ",")
(define-token alias value "<alias>")
(define-token anchor value "<anchor>")
(define-token tag value "<tag>")
(define-token scalar value plain style "<scalar>")
