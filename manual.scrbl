#lang scribble/manual
@(require planet/scribble racket/sandbox scribble/eval
          (for-label (except-in racket load) (this-package-in main)))
@(define yaml-evaluator
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket #:requires '((planet esilkensen/yaml)))))))
@(interaction-eval
  #:eval yaml-evaluator
  (current-print pretty-print-handler))

@title{YAML}
@author{@(author+email "Erik Silkensen" "ejs@ccs.neu.edu")}
@defmodule/this-package[main]

This module provides utilities for parsing and emitting data in the YAML
data serialization format to and from Racket values. See the 
@link["http://yaml.org"]{YAML web site} for more information about YAML.
The implementation is ported from @link["http://pyyaml.org"]{PyYAML}.

@section{Examples}

Better start with an example or two.
@interaction[#:eval yaml-evaluator
(write-yaml
 '(#hash(("name" . "Mark McGwire")
         ("hr" . 65)
         ("avg" . 0.278))
   #hash(("name" . "Sammy Sosa")
         ("hr" . 63)
         ("avg" . 0.288)))
 #:flow-style #f)]
 And yeah!
@interaction[#:eval yaml-evaluator
(string->yaml
 (string-append
  "- name: Mark McGwire\n"
  "  hr:   65\n"
  "  avg:  0.278\n"
  "- name: Sammy Sosa\n"
  "  hr:   63\n"
  "  avg:  0.288\n"))]

@section{YAML Expressions}

@defproc[(yaml? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a YAML expression, @racket[#f] otherwise.

This library defines a subset of Racket values that can be represented as
YAML strings, and this predicate checks for such values.
A @deftech{YAML expression} is one of:
@itemize[
  @item{the value of @racket[(yaml-null)]}
  @item{@racket[(or/c boolean? string? exact-integer? inexact-real? date?)]}
  @item{@racket[(cons/c yaml? yaml?)]}
  @item{a nonempty @racket[(listof yaml?)]}
  @item{a nonempty @racket[(hash/c yaml? yaml?)]}
  @item{a nonempty @racket[(set/c yaml?)]}
]
}

@defparam[yaml-null null any/c]{

A parameter that determines the Racket value that corresponds to a YAML
``@tt{null}'' (empty scalar) value. It is @racket['null] by default.
}

@section{Reading YAML}

The following 

@defproc[(read-yaml
           [name any/c 'input]
           [in input-port? (current-input-port)])
         yaml?]{
Read Documentation.
}

@defproc[(read-yaml*
           [name any/c 'input]
           [in input-port? (current-input-port)])
         (listof yaml?)]{
Read-all Documentation.
}

@defproc[(string->yaml
           [str string?])
         yaml?]{
From-string Documentation.
}

@defproc[(string->yaml*
           [str string?])
         (listof yaml?)]{
From-string-all Documentation.
}

@section{Writing YAML}

The following

@defproc[(write-yaml
           [document yaml?]
           [out output-port? (current-output-port)]
           [#:style default-style (or/c char? #f) #f]
           [#:flow-style default-flow-style (or/c boolean? 'best) 'best])
         void?]{
Write Documentation.
}

@defproc[(write-yaml*
           [documents (listof yaml?)]
           [out output-port? (current-output-port)]
           [#:style default-style (or/c char? #f) #f]
           [#:flow-style default-flow-style (or/c boolean? 'best) 'best])
         void?]{
Write-all Documentation.
}

@defproc[(yaml->string
           [document yaml?]
           [#:style default-style (or/c char? #f) #f]
           [#:flow-style default-flow-style (or/c boolean? 'best) 'best])
         string?]{
To-string Documentation.
}

@defproc[(yaml*->string
           [documents (listof yaml?)]
           [#:style default-style (or/c char? #f) #f]
           [#:flow-style default-flow-style (or/c boolean? 'best) 'best])
         string?]{
To-string-all Documentation.
}
