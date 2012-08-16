;;;;;; composer.rkt - YAML composer.    -*- Mode: Racket -*-

#lang racket

(require
 (planet dyoo/while-loop)
 "parser.rkt"
 "resolver.rkt"
 "events.rkt"
 "nodes.rkt"
 "utils.rkt")

(provide make-composer)

(define composer-error (make-error 'composer))

(define (make-composer [in (current-input-port)] #:name [name "<input>"])
  (define-values (check-event? peek-event get-event)
    (make-parser in #:name name))
  
  (define anchors (make-hash))

  (define (check-node?)
    (when (check-event? 'stream-end)
      (get-event))
    (not (check-event? 'stream-end)))

  (define (get-node)
    (unless (check-event? 'stream-end)
      (compose-document)))

  (define (get-single-node)
    (let ([document #f])
      (get-event)
      (unless (check-event? 'stream-end)
        (set! document (compose-document)))
      (unless (check-event? 'stream-end)
        (composer-error
         "expected a single document in the stream"
         "but found another document"
         (event-start (get-event))))
      (get-event)
      document))

  (define (compose-document)
    (get-event)
    (let ([node (compose-node #f #f)])
      (get-event)
      (set! anchors (make-hash))
      node))

  (define (compose-node parent index)
    (cond
     [(check-event? 'alias)
      (let* ([event (get-event)]
             [anchor (any-event-attr 'anchor event)])
        (unless (hash-has-key? anchors anchor)
          (composer-error
           #f
           (format "found undefined alias ~a" anchor)
           (event-start event)))
        (hash-ref anchors anchor))]
     [else
      (let* ([event (peek-event)]
             [anchor (any-event-attr 'anchor event)])
        (when (hash-has-key? anchors anchor)
          (composer-error
           #f
           (format "found duplicate anchor ~a" anchor)
           (event-start event)))
        (let ([node #f])
          (cond
           [(check-event? 'scalar)
            (set! node (compose-scalar-node anchor))]
           [(check-event? 'sequence-start)
            (set! node (compose-sequence-node anchor))]
           [(check-event? 'mapping-start)
            (set! node (compose-mapping-node anchor))])
          node))]))

  (define (compose-scalar-node anchor)
    (let* ([event (get-event)]
           [tag (scalar-event-tag event)])
      (when (or (not tag) (equal? "!" tag))
        (let ([value (scalar-event-value event)]
              [implicit (scalar-event-implicit event)])
          (set! tag (resolve 'scalar value implicit))))
      (let ([value (scalar-event-value event)]
            [start (event-start event)]
            [end (event-end event)]
            [style (scalar-event-style event)])
        (let ([node (scalar-node tag value start end style)])
          (when anchor
            (hash-set! anchors anchor node))
          node))))

  (define (compose-sequence-node anchor)
    (let* ([event (get-event)]
           [tag (any-event-attr 'tag event)])
      (when (or (not tag) (equal? "!" tag))
        (set! tag (resolve 'sequence #f (any-event-attr 'implicit event))))
      (let* ([start (event-start event)]
             [flow-style (any-event-attr 'flow-style event)]
             [node (sequence-node tag '() start #f flow-style)]
             [index 0])
        (when anchor
          (hash-set! anchors anchor node))
        (while (not (check-event? 'sequence-end))
          (let ([value (node-value node)]
                [new (compose-node node index)])
            (set-node-value! node (append value (list new)))
            (set! index (add1 index))))
        (set-node-end! (event-end (get-event)))
        node)))

  (define (compose-mapping-node anchor)
    (let* ([event (get-event)]
           [tag (any-event-attr 'tag event)])
      (when (or (not tag) (equal? "!" tag))
        (set! tag (resolve 'mapping #f (any-event-attr 'implicit event))))
      (let* ([start (event-start event)]
             [flow-style (any-event-attr 'flow-style event)]
             [node (mapping-node tag '() start #f flow-style)])
        (when anchor
          (hash-set! anchors anchor node))
        (while (not (check-event? 'mapping-end))
          (let* ([item-key (compose-node node #f)]
                 [item-value (compose-node node item-key)]
                 [value (node-value node)]
                 [new (cons item-key item-value)])
            (set-node-value! node (append value (list new)))))
        (set-node-end! (event-end (get-event)))
        node)))

  (values check-node? get-node get-single-node))
