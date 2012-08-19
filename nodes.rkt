;;;;;; nodes.rkt - YAML nodes.    -*- Mode: Racket -*-

#lang typed/racket

(require "errors.rkt" "struct.rkt")

(provide (all-defined-out))

(yaml-struct: node ([start : mark] [end : (Option mark)]) #:mutable)

(node: scalar
  ([tag : (Option String)]
   [value : String]
   [style : (Option Char)]))
(node: sequence
  ([tag : (Option String)]
   [value : (Listof node)]
   [flow-style : Boolean])
  #:mutable)
(node: mapping
  ([tag : (Option String)]
   [value : (Listof (Pairof node node))]
   [flow-style : Boolean])
  #:mutable)

(: node-tag (node -> (Option String)))
(define (node-tag node)
  (cond
   [(scalar-node? node) (scalar-node-tag node)]
   [(sequence-node? node) (sequence-node-tag node)]
   [(mapping-node? node) (mapping-node-tag node)]
   [else (error 'node-tag "unexpected node: ~a" (node->string node))]))

(: node->string-rec (node -> String))
(define (node->string-rec n)
  (cond
   [(scalar-node? n)
    (node->string n)]
   [(sequence-node? n)
    (format
     "sequence-node(flow-style=~s, tag=~s, value=~a)"
     (sequence-node-flow-style n)
     (sequence-node-tag n)
     (map node->string-rec (sequence-node-value n)))]
   [(mapping-node? n)
    (format
     "mapping-node(flow-style=~s, tag=~s, value=~a)"
     (mapping-node-flow-style n)
     (mapping-node-tag n)
     (map (λ: ([p : (Pairof node node)])
            (cons (node->string-rec (car p))
                  (node->string-rec (cdr p))))
          (mapping-node-value n)))]
   [else (error 'node->string-rec "unexpected node: ~a" (node->string n))]))

(: print-node-rec
   (case-> (node -> Void) (node Output-Port -> Void)))
(define (print-node-rec node [out (current-output-port)])
  (fprintf out "~a\n" (node->string-rec node)))
