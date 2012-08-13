;;;;;; tokens.rkt - YAML tokens.    -*- Mode: Racket -*-

#lang typed/racket

(require "utils.rkt")

(provide (all-defined-out))

(struct: token
  ([type : Symbol] [id : String] [start : mark] [end : mark]
   [attrs : (HashTable Symbol Any)]) #:transparent)

(: token->string (token -> String))
(define (token->string token)
  (let ([start (token-start token)]
        [end (token-end token)]
        [attrs (token-attrs token)])
    (format "~a-token(~a)"
            (token-type token)
            (string-join
             (map
              (λ: ([attr : Symbol])
                (format "~a=~s" attr (hash-ref attrs attr)))
              (sort (hash-keys attrs)
                    (λ: ([x : Symbol] [y : Symbol])
                      (string<=? (symbol->string x)
                                 (symbol->string y)))))
             ", "))))

(: print-token (token -> Void))
(define (print-token token)
  (displayln (token->string token)))

(define-for-syntax (build-name id . parts)
  (let ([str (apply string-append
                    (map (λ (p)
                           (if (syntax? p)
                               (symbol->string (syntax-e p))
                               (format "~a" p)))
                         parts))])
    (datum->syntax id (string->symbol str) id)))

(define-syntax (define-token stx)
  (syntax-case stx ()
    [(_ name field ... id)
     (let ([t (build-name #'name #'name "-token")]
           [t? (build-name #'name #'name "-token?")]
           [fs (map (λ (f)
                      (build-name #'name #'name "-token-" f))
                    (syntax->list #'(field ...)))])
       #`(begin
           (define:
               (#,t [start : mark] [end : mark] [field : Any] ...) : token
             (let ([attrs (ann (make-hash `((field . ,field) ...))
                               (HashTable Symbol Any))])
               (token 'name id start end attrs)))
           (: #,t? (Any -> Boolean))
           (define (#,t? token)
             (and (token? token)
                  (eq? 'name (token-type token))))
           (define-values (#,@fs)
             (values
              (λ: ([t : token])
                (hash-ref (token-attrs t) 'field)) ...))))]))

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
