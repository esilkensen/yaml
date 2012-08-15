;;;;;; events.rkt - YAML events.    -*- Mode: Racket -*-

#lang typed/racket

(require "utils.rkt")

(provide (except-out (all-defined-out) event-strings))

(struct: event ([start : mark] [end : mark]))

(: event-strings (HashTable (Any -> Boolean) (event -> String)))
(define event-strings (make-hash))

(: event->string (event -> String))
(define (event->string event)
  (let loop ([es (hash-keys event-strings)])
    (if (null? es)
        (error 'event->string "unexpected event type")
        (if ((car es) event)
            ((hash-ref event-strings (car es)) event)
            (loop (cdr es))))))
             
(: print-event (event -> Void))
(define (print-event event)
  (displayln (event->string event)))

(define-syntax (define-event: stx)
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
     (let ([e (build-name #'name #'name "-event")]
           [e? (build-name #'name #'name "-event?")]
           [e->string (build-name #'name #'name "-event->string")]
           [fs (map (λ (f) `(cons ,(format "~a" (syntax->datum f))
                                  ,(build-name #'name #'name "-event-" f)))
                    (sort (syntax->list #'(field ...))
                          (λ (s t)
                            (string<? (format "~a" (syntax->datum s))
                                      (format "~a" (syntax->datum t))))))])
       #`(begin
           (struct: #,e event ([field : type] ...))
           (: #,e->string (event -> String))
           (define (#,e->string e)
             (if (#,e? e)
                 (let* ([attr->string
                         (λ: ([p : (Pairof String (#,e -> Any))])
                           (format "~a=~s" (car p) ((cdr p) e)))]
                        [fields (map attr->string (list #,@fs))])
                   (format "~a(~a)" '#,e (string-join fields ", ")))
                 (error '#,e->string "unexpected event type")))
           (hash-set! event-strings #,e? #,e->string)))]))

(define-event: stream-start)

(define-event: stream-end)

(define-event: document-start
  [explicit : Boolean]
  [version : (Option (Pairof Integer Integer))]
  [tags : (Option (HashTable String String))])

(define-event: document-end
  [explicit : Boolean])

(define-event: alias
  [anchor : (Option String)])

(define-event: scalar
  [anchor : (Option String)]
  [tag : (Option String)]
  [implicit : (Pairof Boolean Boolean)]
  [value : String]
  [style : (Option Char)])

(define-event: sequence-start
  [anchor : (Option String)]
  [tag : (Option String)]
  [implicit : Boolean]
  [flow-style : Boolean])

(define-event: sequence-end)

(define-event: mapping-start
  [anchor : (Option String)]
  [tag : (Option String)]
  [implicit : Boolean]
  [flow-style : Boolean])

(define-event: mapping-end)

(define-predicate node-event?
  (U alias-event scalar-event))

(define-predicate collection-start-event?
  (U sequence-start-event mapping-start-event))

(define-predicate collection-end-event?
  (U sequence-end-event mapping-end-event))
