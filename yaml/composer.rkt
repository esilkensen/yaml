;;;;;; composer.rkt - YAML composer.    -*- Mode: Racket -*-

#lang racket

(require
 "errors.rkt"
 "events.rkt"
 "nodes.rkt"
 "parser.rkt"
 "resolver.rkt"
 "utils.rkt")

(provide
 (contract-out
  [compose-file (path-string? . -> . (listof node?))]
  [compose-string (string? . -> . (listof node?))]
  [compose-all (() (input-port?) . ->* . (listof node?))]
  [compose (() (input-port?) . ->* . (or/c node? #f))]
  [make-composer
   (()
    (input-port?)
    . ->* .
    (values
     ;; check-node?
     (-> boolean?)
     ;; get-node
     (-> (or/c node? void?))
     ;; get-single-node
     (-> (or/c node? #f))))]))

(define composer-error (make-error 'composer))

(define (compose-file filename)
  (with-input-from-file filename compose-all))

(define (compose-string string)
  (with-input-from-string string compose-all))

(define (compose [in (current-input-port)])
  (define-values (check-node? get-node get-single-node)
    (make-composer in))
  (get-single-node))

(define (compose-all [in (current-input-port)])
  (define-values (check-node? get-node get-single-node)
    (make-composer in))
  (let loop ([nodes '()])
    (if (check-node?)
        (loop (cons (get-node) nodes))
        (reverse nodes))))

(define (make-composer [in (current-input-port)])
  (define-values (check-event? peek-event get-event)
    (make-parser in))
  
  (define anchors (make-hash))
  
  (define (check-node?)
    (when (check-event? stream-start-event?)
      (get-event))
    (not (check-event? stream-end-event?)))
  
  (define (get-node)
    (when (check-event?)
      (unless (check-event? stream-end-event?)
        (compose-document))))
  
  (define (get-single-node)
    (and (check-event?)
         (let ([document #f])
           (get-event)
           (unless (check-event? stream-end-event?)
             (set! document (compose-document)))
           (unless (check-event? stream-end-event?)
             (composer-error
              "expected a single document in the stream"
              "but found another document"
              (event-start (get-event))))
           (get-event)
           document)))
  
  (define (compose-document)
    (get-event)
    (let ([node (compose-node #f #f)])
      (get-event)
      (set! anchors (make-hash))
      node))
  
  (define (compose-node parent index)
    (cond
      [(check-event? alias-event?)
       (let* ([event (get-event)]
              [anchor (any-event-anchor event)])
         (unless (hash-has-key? anchors anchor)
           (composer-error
            #f
            (format "found undefined alias ~a" anchor)
            (event-start event)))
         (hash-ref anchors anchor))]
      [else
       (let* ([event (peek-event)]
              [anchor (any-event-anchor event)])
         (when (hash-has-key? anchors anchor)
           (composer-error
            #f
            (format "found duplicate anchor ~a" anchor)
            (event-start event)))
         (let ([node #f])
           (cond
             [(check-event? scalar-event?)
              (set! node (compose-scalar-node anchor))]
             [(check-event? sequence-start-event?)
              (set! node (compose-sequence-node anchor))]
             [(check-event? mapping-start-event?)
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
        (let ([node (scalar-node start end tag value style)])
          (when anchor
            (hash-set! anchors anchor node))
          node))))
  
  (define (compose-sequence-node anchor)
    (let* ([event (get-event)]
           [tag (any-event-tag event)])
      (when (or (not tag) (equal? "!" tag))
        (set! tag (resolve 'sequence #f (any-event-implicit event))))
      (let* ([start (event-start event)]
             [flow-style (collection-start-event-flow-style event)]
             [node (sequence-node start #f tag '() flow-style)]
             [index 0])
        (when anchor
          (hash-set! anchors anchor node))
        (while (not (check-event? sequence-end-event?))
          (let ([value (sequence-node-value node)]
                [new (compose-node node index)])
            (set-sequence-node-value! node (append value (list new)))
            (set! index (add1 index))))
        (set-node-end! node (event-end (get-event)))
        node)))
  
  (define (compose-mapping-node anchor)
    (let* ([event (get-event)]
           [tag (any-event-tag event)])
      (when (or (not tag) (equal? "!" tag))
        (set! tag (resolve 'mapping #f (any-event-implicit event))))
      (let* ([start (event-start event)]
             [flow-style (collection-start-event-flow-style event)]
             [node (mapping-node start #f tag '() flow-style)])
        (when anchor
          (hash-set! anchors anchor node))
        (while (not (check-event? mapping-end-event?))
          (let* ([item-key (compose-node node #f)]
                 [item-value (compose-node node item-key)]
                 [value (mapping-node-value node)]
                 [new (cons item-key item-value)])
            (set-mapping-node-value! node (append value (list new)))))
        (set-node-end! node (event-end (get-event)))
        node)))
  
  (values check-node? get-node get-single-node))

(module+ test
  (require rackunit)
  (for ([(test-file check-file) (test-files #".compose")])
    (test-case (path->string check-file)
      (for ([node (compose-file test-file)]
            [line (file->lines check-file)])
        (check-equal? (node->string-rec node) line)))))
