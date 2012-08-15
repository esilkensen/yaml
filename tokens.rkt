;;;;;; tokens.rkt - YAML tokens.    -*- Mode: Racket -*-

#lang typed/racket

(require "utils.rkt")

(provide (except-out (all-defined-out) token-strings))

(struct: token ([start : mark] [end : mark]))

(: token-strings (HashTable (Any -> Boolean) (token -> String)))
(define token-strings (make-hash))

(: token->string (token -> String))
(define (token->string token)
  (let loop ([ts (hash-keys token-strings)])
    (if (null? ts)
        (error 'token->string "unexpected token type")
        (if ((car ts) token)
            ((hash-ref token-strings (car ts)) token)
            (loop (cdr ts))))))
             
(: print-token (token -> Void))
(define (print-token token)
  (displayln (token->string token)))

(define-syntax (define-token: stx)
  (define (build-name id . parts)
    (let ([str (apply string-append
                      (map (λ (p)
                             (if (syntax? p)
                                 (symbol->string (syntax-e p))
                                 (format "~a" p)))
                           parts))])
      (datum->syntax id (string->symbol str) id)))
  (syntax-case stx (:)
    [(_ name [field : type] ...)
     (let ([t (build-name #'name #'name "-token")]
           [t? (build-name #'name #'name "-token?")]
           [t->string (build-name #'name #'name "-token->string")]
           [fs (map (λ (f) `(cons ,(format "~a" (syntax->datum f))
                                  ,(build-name #'name #'name "-token-" f)))
                    (sort (syntax->list #'(field ...))
                          (λ (s t)
                            (string<? (format "~a" (syntax->datum s))
                                      (format "~a" (syntax->datum t))))))])
       #`(begin
           (struct: #,t token ([field : type] ...))
           (: #,t->string (token -> String))
           (define (#,t->string t)
             (if (#,t? t)
                 (let* ([attr->string
                         (λ: ([p : (Pairof String (#,t -> Any))])
                           (format "~a=~s" (car p) ((cdr p) t)))]
                        [fields (map attr->string (list #,@fs))])
                   (format "~a(~a)" '#,t (string-join fields ", ")))
                 (error '#,t->string "unexpected token type")))
           (hash-set! token-strings #,t? #,t->string)))]))

(define-token: directive
  [name : String]
  [value : (Option (Pairof Integer Integer))])

(define-token: document-start)

(define-token: document-end)

(define-token: stream-start)

(define-token: stream-end)

(define-token: block-sequence-start)

(define-token: block-mapping-start)

(define-token: block-end)

(define-token: flow-sequence-start)

(define-token: flow-mapping-start)

(define-token: flow-sequence-end)

(define-token: flow-mapping-end)

(define-token: key)

(define-token: value)

(define-token: block-entry)

(define-token: flow-entry)

(define-token: alias
  [value : String])

(define-token: anchor
  [value : String])

(define-token: tag
  [value : (Pairof (Option String) (Option String))])

(define-token: scalar
  [value : String]
  [plain : Boolean]
  [style : Char])
