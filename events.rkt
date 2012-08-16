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

(define-type node-event
  (U alias-event scalar-event))
(define-type collection-start-event
  (U sequence-start-event mapping-start-event))
(define-type collection-end-event
  (U sequence-end-event mapping-end-event))

(define-predicate node-event? node-event)
(define-predicate collection-start-event? collection-start-event)
(define-predicate collection-end-event? collection-end-event)

(: node-event-anchor
   (node-event -> (Option String)))
(define (node-event-anchor event)
  (if (alias-event? event)
      (alias-event-anchor event)
      (scalar-event-anchor event)))

(: collection-start-event-anchor
   (collection-start-event -> (Option String)))
(define (collection-start-event-anchor event)
  (if (sequence-start-event? event)
      (sequence-start-event-anchor event)
      (mapping-start-event-anchor event)))

(: collection-start-event-tag
   (collection-start-event -> (Option String)))
(define (collection-start-event-tag event)
  (if (sequence-start-event? event)
      (sequence-start-event-tag event)
      (mapping-start-event-tag event)))

(: collection-start-event-implicit
   (collection-start-event -> Boolean))
(define (collection-start-event-implicit event)
  (if (sequence-start-event? event)
      (sequence-start-event-implicit event)
      (mapping-start-event-implicit event)))  

(: any-event-tag
   ((U scalar-event collection-start-event) -> (Option String)))
(define (any-event-tag event)
  (if (scalar-event? event)
      (scalar-event-tag event)
      (collection-start-event-tag event)))

(: any-event-anchor
   ((U node-event collection-start-event) -> (Option String)))
(define (any-event-anchor event)
  (if (node-event? event)
      (node-event-anchor event)
      (collection-start-event-anchor event)))
