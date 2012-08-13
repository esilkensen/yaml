;;;;;; nodes.rkt - YAML nodes.    -*- Mode: Racket -*-

#lang racket

(provide (all-defined-out))

(struct node (type tag value start end attrs) #:mutable)

(define (node->string node)
  (cond
   [(node? node)
    (let ([tag (node-tag node)]
          [value (node-value node)]
          [start (node-start node)]
          [end (node-end node)]
          [attrs (node-attrs node)])
      (format "~a-node(~a)"
              (node-type node)
              (string-join
               (map
                (λ (attr)
                  (format "~a=~s" attr (hash-ref attrs attr)))
                (sort (hash-keys attrs)
                      (λ (x y)
                        (string<=? (symbol->string x)
                                   (symbol->string y)))))
               ", ")))]
   [else "no node!"]))

(define (print-node node)
  (displayln (node->string node)))

(define-syntax (define-node stx)
  (define (build-name id . parts)
    (let ([str (apply string-append
                      (map (λ (p)
                             (if (syntax? p)
                                 (symbol->string (syntax-e p))
                                 (format "~a" p)))
                           parts))])
      (datum->syntax id (string->symbol str) id)))
  (syntax-case stx ()
    [(_ name field ...)
     (let ([n (build-name #'name #'name "-node")]
           [n? (build-name #'name #'name "-node?")]
           [fs (map (λ (f)
                      (build-name #'name #'name "-node-" f))
                    (syntax->list #'(field ...)))])
       #`(begin
           (define (#,n tag value start end field ...)
             (let ([attrs (make-hash `((field . ,field) ...))])
               (node 'name start end attrs)))
           (define (#,n? node)
             (and (node? node)
                  (eq? 'name (node-type node))))
           (define-values (#,@fs)
             (values
              (λ (node)
                (hash-ref (node-attrs node) 'field)) ...))))]))

(define-node scalar style)
(define-node collection flow-style)
(define-node sequence flow-style) ; collection
(define-node mapping flow-style) ; collection

(define (any-collection-node? n)
  (or (collection-node? n)
      (sequence-node? n)
      (mapping-node? n)))

