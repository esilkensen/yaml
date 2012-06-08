;;;;;; parser.rkt - YAML parser.    -*- Mode: Racket -*-

#lang racket

(require "tokens.rkt" "scanner.rkt" "events.rkt")

(provide (all-defined-out))

#|
The following YAML grammar is LL(1) and is parsed by a recursive descent
parser.

stream ::= 
      STREAM-START implicit_document? explicit_document* STREAM-END
implicit_document ::=
      block_node DOCUMENT-END*
explicit_document ::=
      DIRECTIVE* DOCUMENT-START block_node? DOCUMENT-END*
block_node_or_indentless_sequence ::=
      ALIAS
    | properties (block_content | indentless_block_sequence)?
    | block_content
    | indentless_block_sequence
block_node ::= 
      ALIAS
    | properties block_content?
    | block_content
flow_node ::= 
      ALIAS
    | properties flow_content?
    | flow_content
properties ::=
      TAG ANCHOR?
    | ANCHOR TAG?
block_content ::=
      block_collection
    | flow_collection
    | SCALAR
flow_content ::=
      flow_collection
    | SCALAR
block_collection ::=
      block_sequence
    | block_mapping
flow_collection ::=
      flow_sequence
    | flow_mapping
block_sequence ::=
      BLOCK-SEQUENCE-START (BLOCK-ENTRY block_node?)* BLOCK-END
indentless_sequence ::=
      (BLOCK-ENTRY block_node?)+
block_mapping ::=
      BLOCK-MAPPING_START ((KEY block_node_or_indentless_sequence?)?
        (VALUE block_node_or_indentless_sequence?)?)* BLOCK-END
flow_sequence ::=
      FLOW-SEQUENCE-START (flow_sequence_entry FLOW-ENTRY)*
        flow_sequence_entry? FLOW-SEQUENCE-END
flow_sequence_entry ::=
      flow_node
    | KEY flow_node? (VALUE flow_node?)?
flow_mapping ::=
      FLOW-MAPPING-START (flow_mapping_entry FLOW-ENTRY)*
        flow_mapping_entry? FLOW-MAPPING-END
flow_mapping_entry ::=
      flow_node
    | KEY flow_node? (VALUE flow_node?)?

FIRST sets:

stream: 
  { STREAM-START }
explicit_document:
  { DIRECTIVE DOCUMENT-START }
implicit_document:
  FIRST(block_node)
block_node:
  { ALIAS TAG ANCHOR SCALAR BLOCK-SEQUENCE-START BLOCK-MAPPING-START
    FLOW-SEQUENCE-START FLOW-MAPPING-START }
flow_node:
  { ALIAS ANCHOR TAG SCALAR FLOW-SEQUENCE-START FLOW-MAPPING-START }
block_content:
  { BLOCK-SEQUENCE-START BLOCK-MAPPING-START FLOW-SEQUENCE-START
    FLOW-MAPPING-START SCALAR }
flow_content:
  { FLOW-SEQUENCE-START FLOW-MAPPING-START SCALAR }
block_collection:
  { BLOCK-SEQUENCE-START BLOCK-MAPPING-START }
flow_collection:
  { FLOW-SEQUENCE-START FLOW-MAPPING-START }
block_sequence:
  { BLOCK-SEQUENCE-START }
block_mapping:
  { BLOCK-MAPPING-START }
block_node_or_indentless_sequence:
  { ALIAS ANCHOR TAG SCALAR BLOCK-SEQUENCE-START BLOCK-MAPPING-START
    FLOW-SEQUENCE-START FLOW-MAPPING-START BLOCK-ENTRY }
indentless_sequence:
  { ENTRY }
flow_collection:
  { FLOW-SEQUENCE-START FLOW-MAPPING-START }
flow_sequence:
  { FLOW-SEQUENCE-START }
flow_mapping:
  { FLOW-MAPPING-START }
flow_sequence_entry:
  { ALIAS ANCHOR TAG SCALAR FLOW-SEQUENCE-START FLOW-MAPPING-START KEY }
flow_mapping_entry:
  { ALIAS ANCHOR TAG SCALAR FLOW-SEQUENCE-START FLOW-MAPPING-START KEY }
|#

(define (make-parser [in (current-input-port)] #:name [name "<input>"])
  (define-values (check-token? peek-token get-token)
    (make-scanner in #:name name))

  (define DEFAULT-TAGS #hash(("!" . "!") ("!!" . "tag:yaml.org,2002:")))

  (define current-event #f)
  (define yaml-version #f)
  (define tag-handles (make-hash))
  (define states '())
  (define marks '())
  (define state parse-stream-start)

  (define (dispose)
    ;; Reset the state attributes (to clear self-references).
    (set! states '())
    (set! state #f))
