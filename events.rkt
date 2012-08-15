;;;;;; events.rkt - YAML events.    -*- Mode: Racket -*-

#lang typed/racket

(require "struct.rkt" "utils.rkt")

(provide (all-defined-out))

(yaml-struct: event ([start : mark] [end : mark]))

(event: stream-start)
(event: stream-end)
(event: document-start
  ([explicit : Boolean]
   [version : (Option (Pairof Integer Integer))]
   [tags : (Option (HashTable String String))]))
(event: document-end
  ([explicit : Boolean]))
(event: alias
  ([anchor : (Option String)]))
(event: scalar
  ([anchor : (Option String)]
   [tag : (Option String)]
   [implicit : (Pairof Boolean Boolean)]
   [value : String]
   [style : (Option Char)]))
(event: sequence-start
  ([anchor : (Option String)]
   [tag : (Option String)]
   [implicit : Boolean]
   [flow-style : Boolean]))
(event: sequence-end)
(event: mapping-start
  ([anchor : (Option String)]
   [tag : (Option String)]
   [implicit : Boolean]
   [flow-style : Boolean]))
(event: mapping-end)

(define-predicate node-event?
  (U alias-event scalar-event))
(define-predicate collection-start-event?
  (U sequence-start-event mapping-start-event))
(define-predicate collection-end-event?
  (U sequence-end-event mapping-end-event))
