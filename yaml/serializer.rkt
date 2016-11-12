;;;;;; serializer.rkt - YAML serializer.    -*- Mode: Racket -*-

#lang racket

(require
 "emitter.rkt"
 "errors.rkt"
 "events.rkt"
 "nodes.rkt"
 "resolver.rkt"
 "utils.rkt")

(provide
 serializer+c%
 (contract-out
  [serializer% serializer+c%]))

(define serializer+c%
  (class/c
   (init-field
    [resolver (instanceof/c resolver+c%)]
    [out output-port?]
    [canonical? boolean?]
    [indent (or/c exact-positive-integer? #f)]
    [width (or/c exact-positive-integer? #f)]
    [allow-unicode? boolean?]
    [line-break (or/c "\n" "\r" "\r\n" #f)]
    [explicit-start? boolean?]
    [explicit-end? boolean?]
    [version (or/c (cons/c exact-integer? exact-integer?) #f)]
    [tags (or/c (hash/c string? string?) #f)])
   [open (->m void?)]
   [close (->m void?)]
   [serialize (node? . ->m . void?)]))

(define ANCHOR-TEMPLATE "id~a")

(define (serializer-error msg) (error 'serializer msg))

(define serializer%
  (class object%
    (init-field
     [resolver (new resolver%)]
     [out (current-output-port)]
     [canonical? #f]
     [indent #f]
     [width #f]
     [allow-unicode? #f]
     [line-break #f]
     [explicit-start? #f]
     [explicit-end? #f]
     [version #f]
     [tags #f])
    
    (super-new)
    
    (define emitter
      (new emitter%
           [out out]
           [canonical? canonical?]
           [indent indent]
           [width width]
           [allow-unicode? allow-unicode?]
           [line-break line-break]))
    (define serialized-nodes (make-hasheq))
    (define anchors (make-hasheq))
    (define last-anchor-id 0)
    (define closed '())
    
    (define/public (open)
      (cond
        [(null? closed)
         (send emitter emit (stream-start-event #f #f))
         (set! closed #f)]
        [closed
         (serializer-error "serializer is closed")]
        [else
         (serializer-error "serializer is open")]))
    
    (define/public (close)
      (cond
        [(null? closed)
         (serializer-error "serializer has not been opened")]
        [(not closed)
         (send emitter emit (stream-end-event #f #f))
         (set! closed #t)]
        [else
         (serializer-error "serializer is closed")]))
    
    (define/public (serialize node)
      (cond
        [(null? closed)
         (serializer-error "serializer has not been opened")]
        [closed
         (serializer-error "serializer is closed")])
      (send emitter emit (document-start-event
                          #f #f explicit-start? version tags))
      (anchor-node node)
      (serialize-node node)
      (send emitter emit (document-end-event #f #f explicit-end?))
      (set! serialized-nodes (make-hasheq))
      (set! anchors (make-hasheq))
      (set! last-anchor-id 0))
    
    (define (anchor-node node)
      (cond
        [(hash-has-key? anchors node)
         (unless (hash-ref anchors node)
           (hash-set! anchors node (generate-anchor node)))]
        [else
         (hash-set! anchors node #f)
         (cond
           [(sequence-node? node)
            (for ([item (sequence-node-value node)])
              (anchor-node item))]
           [(mapping-node? node)
            (for ([kv (mapping-node-value node)])
              (anchor-node (car kv))
              (anchor-node (cdr kv)))])]))
    
    (define (generate-anchor node)
      (set! last-anchor-id (add1 last-anchor-id))
      (let ([str (number->string last-anchor-id)])
        (while (< (string-length str) 3)
          (set! str (string-append "0" str)))
        (format ANCHOR-TEMPLATE str)))
    
    (define (serialize-node node)
      (let ([alias (hash-ref anchors node)])
        (cond
          [(hash-has-key? serialized-nodes node)
           (send emitter emit (alias-event #f #f alias))]
          [else
           (hash-set! serialized-nodes node #t)
           (cond
             [(scalar-node? node)
              (let* ([tag (scalar-node-tag node)]
                     [value (scalar-node-value node)]
                     [style (scalar-node-style node)]
                     [detected-tag
                      (send resolver resolve 'scalar value (cons #t #f))]
                     [default-tag
                       (send resolver resolve 'scalar value (cons #f #t))]
                     [implicit (cons (equal? tag detected-tag)
                                     (equal? tag default-tag))])
                (send emitter emit (scalar-event
                                    #f #f alias tag implicit value style)))]
             [(sequence-node? node)
              (let* ([tag (sequence-node-tag node)]
                     [value (sequence-node-value node)]
                     [implicit
                      (equal? tag (send resolver resolve 'sequence value #t))]
                     [flow-style (sequence-node-flow-style node)])
                (send emitter emit (sequence-start-event
                                    #f #f alias tag implicit flow-style))
                (for ([item value])
                  (serialize-node item))
                (send emitter emit (sequence-end-event #f #f)))]
             [(mapping-node? node)
              (let* ([tag (mapping-node-tag node)]
                     [value (mapping-node-value node)]
                     [implicit
                      (equal? tag (send resolver resolve 'mapping value #t))]
                     [flow-style (mapping-node-flow-style node)])
                (send emitter emit (mapping-start-event
                                    #f #f alias tag implicit flow-style))
                (for ([kv value])
                  (serialize-node (car kv))
                  (serialize-node (cdr kv)))
                (send emitter emit (mapping-end-event #f #f)))])])))))

(module+ test
  (require rackunit)
  (test-case "open"
    (define serializer (new serializer%))
    (send serializer open)
    (check-exn
     #rx"serializer is open"
     (λ () (send serializer open)))
    (send serializer close)
    (check-exn
     #rx"serializer is closed"
     (λ () (send serializer open))))
  (test-case "close"
    (define serializer (new serializer%))
    (check-exn
     #rx"serializer has not been opened"
     (λ () (send serializer close)))
    (check-exn
     #rx"serializer has not been opened"
     (λ () (send serializer serialize #f)))
    (send serializer open)
    (send serializer close)
    (check-exn
     #rx"serializer is closed"
     (λ () (send serializer close)))
    (check-exn
     #rx"serializer is closed"
     (λ () (send serializer serialize #f)))))
