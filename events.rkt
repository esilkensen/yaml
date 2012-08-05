;;;;;; events.rkt - YAML events.    -*- Mode: Racket -*-

#lang racket

(provide (all-defined-out))

(struct event (type start end attrs))

(define (print-event event)
  (cond
   [(event? event)
    (let ([start (event-start event)]
          [end (event-end event)]
          [attrs (event-attrs event)])
      (printf "~a-event(~a)\n"
              (event-type event)
              (string-join
               (map
                (λ (attr)
                  (format "~a=~s" attr (hash-ref attrs attr)))
                (sort (hash-keys attrs)
                      (λ (x y)
                        (string<=? (symbol->string x)
                                   (symbol->string y)))))
               ", ")))]
   [else (printf "no event!\n")]))

(define-syntax (define-event stx)
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
     (let ([e (build-name #'name #'name "-event")]
           [e? (build-name #'name #'name "-event?")]
           [fs (map (λ (f)
                      (build-name #'name #'name "-event-" f))
                    (syntax->list #'(field ...)))])
       #`(begin
           (define (#,e start end field ...)
             (let ([attrs (make-hash `((field . ,field) ...))])
               (event 'name start end attrs)))
           (define (#,e? event)
             (and (event? event)
                  (eq? 'name (event-type event))))
           (define-values (#,@fs)
             (values
              (λ (event)
                (hash-ref (event-attrs event) 'field)) ...))))]))

(define-event node anchor)
(define-event collection-start anchor tag implicit flow-style) ; node
(define-event collection-end)
(define-event stream-start)
(define-event stream-end)
(define-event document-start explicit version tags)
(define-event document-end explicit)
(define-event alias anchor) ; node
(define-event scalar anchor tag implicit value style) ; node
(define-event sequence-start anchor tag implicit flow-style) ; collection-start
(define-event sequence-end) ; collection-end
(define-event mapping-start anchor tag implicit flow-style) ; collection-start
(define-event mapping-end) ; collection-end
