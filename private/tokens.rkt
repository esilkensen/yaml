;;;;;; tokens.rkt - YAML tokens.    -*- Mode: Racket -*-

#lang typed/racket

(require "errors.rkt" "struct.rkt")

(provide (all-defined-out))

(yaml-struct: token ([start : mark] [end : mark]))

(token: directive
  ([name : String]
   [value : (Option (U (Pairof String String)
                       (Pairof Integer Integer)))]))
(token: document-start)
(token: document-end)
(token: stream-start "<stream start>")
(token: stream-end "<stream end>")
(token: block-sequence-start)
(token: block-mapping-start)
(token: block-end)
(token: flow-sequence-start "[")
(token: flow-mapping-start "{")
(token: flow-sequence-end "]")
(token: flow-mapping-end "}")
(token: key "?")
(token: value ":")
(token: block-entry)
(token: flow-entry ",")
(token: alias
  ([value : String]))
(token: anchor
  ([value : String]))
(token: tag
  ([value : (Pairof (Option String) (Option String))]))
(token: scalar
  ([value : String]
   [plain : Boolean]
   [style : (Option Char)]))
