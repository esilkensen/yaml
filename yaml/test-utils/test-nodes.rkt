#lang racket

(module+ test (require rackunit))

(module unknown-nodes typed/racket
  (require "../nodes.rkt")
  (provide (all-defined-out))
  (node: unknown))

(module+ test
  (require "../nodes.rkt" (submod ".." unknown-nodes))

  (define unknown (unknown-node #f #f))
  
  (test-case "node-tag"
    (check-exn
     #rx"unexpected node"
     (λ () (node-tag unknown))))

  (test-case "set-node-tag!"
    (set-node-tag! (sequence-node #f #f #f '() #f) #f)
    (set-node-tag! (mapping-node #f #f #f '() #f) #f)
    (check-exn
     #rx"unexpected node"
     (λ () (set-node-tag! unknown "tag"))))
  
  (test-case "print-node-rec"
    (check-exn
     #rx"unexpected node"
     (λ () (print-node-rec unknown)))))
