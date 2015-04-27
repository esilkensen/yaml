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
 (contract-out
  [make-serializer
   (()
    (output-port?
     #:canonical boolean?
     #:indent (or/c exact-positive-integer? #f)
     #:width (or/c exact-positive-integer? #f)
     #:allow-unicode boolean?
     #:line-break (or/c "\n" "\r" "\r\n" #f)
     #:explicit-start boolean?
     #:explicit-end boolean?
     #:version (or/c (cons/c exact-integer? exact-integer?) #f)
     #:tags (or/c (hash/c string? string?) #f))
    . ->* .
    ;; serialize
    (node? . -> . void?))]))

(define ANCHOR-TEMPLATE "id~a")

(define (serializer-error msg) (error 'serializer msg))

(define (make-serializer [out (current-output-port)]
                         #:canonical [canonical #f]
                         #:indent [default-indent #f]
                         #:width [default-width #f]
                         #:allow-unicode [allow-unicode #f]
                         #:line-break [line-break #f]
                         #:explicit-start [explicit-start #f]
                         #:explicit-end [explicit-end #f]
                         #:version [version #f]
                         #:tags [tags #f])
  (define emit (make-emitter out
                             #:canonical canonical
                             #:indent default-indent
                             #:width default-width
                             #:allow-unicode allow-unicode
                             #:line-break line-break))
  (define serialized-nodes (make-hasheq))
  (define anchors (make-hasheq))
  (define last-anchor-id 0)
  (define closed '())
  
  (define (open)
    (cond
      [(null? closed)
       (emit (stream-start-event #f #f))
       (set! closed #f)]
      [closed
       (serializer-error "serializer is closed")]
      [else
       (serializer-error "serializer is already opened")]))
  
  (define (close)
    (cond
      [(null? closed)
       (serializer-error "serializer is not opened")]
      [(not closed)
       (emit (stream-end-event #f #f))
       (set! closed #t)]))
  
  (define (serialize node)
    (cond
      [(null? closed)
       (serializer-error "serializer is not opened")]
      [closed
       (serializer-error "serializer is closed")])
    (emit (document-start-event #f #f explicit-start version tags))
    (anchor-node node)
    (serialize-node node)
    (emit (document-end-event #f #f explicit-end))
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
         (emit (alias-event #f #f alias))]
        [else
         (hash-set! serialized-nodes node #t)
         (cond
           [(scalar-node? node)
            (let* ([tag (scalar-node-tag node)]
                   [value (scalar-node-value node)]
                   [style (scalar-node-style node)]
                   [detected-tag
                    (resolve 'scalar value (cons #t #f))]
                   [default-tag
                     (resolve 'scalar value (cons #f #t))]
                   [implicit (cons (equal? tag detected-tag)
                                   (equal? tag default-tag))])
              (emit (scalar-event #f #f alias tag implicit value style)))]
           [(sequence-node? node)
            (let* ([tag (sequence-node-tag node)]
                   [value (sequence-node-value node)]
                   [implicit (equal? tag (resolve 'sequence value #t))]
                   [flow-style (sequence-node-flow-style node)])
              (emit (sequence-start-event #f #f alias tag implicit flow-style))
              (for ([item value])
                (serialize-node item))
              (emit (sequence-end-event #f #f)))]
           [(mapping-node? node)
            (let* ([tag (mapping-node-tag node)]
                   [value (mapping-node-value node)]
                   [implicit (equal? tag (resolve 'mapping value #t))]
                   [flow-style (mapping-node-flow-style node)])
              (emit
               (mapping-start-event #f #f alias tag implicit flow-style))
              (for ([kv value])
                (serialize-node (car kv))
                (serialize-node (cdr kv)))
              (emit (mapping-end-event #f #f)))])])))
  
  (values open close serialize))
