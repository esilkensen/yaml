;;;;;; nodes.rkt - YAML nodes.    -*- Mode: Racket -*-

#lang typed/racket

(require "struct.rkt" "utils.rkt")

(provide (all-defined-out))

(yaml-struct: node ([start : mark] [end : (Option mark)]) #:mutable)

(node: scalar
  ([tag : (Option String)]
   [value : String]
   [style : Boolean]))
(node: sequence
  ([tag : (Option String)]
   [value : (Listof node)]
   [flow-style : Boolean]))
(node: mapping
  ([tag : (Option String)]
   [value : (Listof node)]
   [flow-style : Boolean]))
