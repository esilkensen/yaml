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

As a quick introduction, this section shows an example of using the module
to go from Racket values to YAML and back again.

The @racket[write-yaml] procedure turns a Racket value into YAML output.
Here it displays a sequence of mappings in @emph{flow style} (by default):

@interaction[#:eval yaml-evaluator
(write-yaml
 '(#hash(("name" . "Mark McGwire")
         ("hr" . 65)
         ("avg" . 0.278))
   #hash(("name" . "Sammy Sosa")
         ("hr" . 63)
         ("avg" . 0.288))))]

The @racket[string->yaml] procedure turns YAML text into a Racket value.
Here it constructs the same sequence of mappings from above, where this
time the YAML is in @emph{block style}:

@interaction[#:eval yaml-evaluator
(string->yaml
 (string-append
  "- name: Mark McGwire\n"
  "  hr:   65\n"
  "  avg:  0.278\n"
  "- name: Sammy Sosa\n"
  "  hr:   63\n"
  "  avg:  0.288\n"))]

The @racket[yaml-struct] macro defines a @racket[struct] that can
be written to and read from YAML:

@interaction[#:eval yaml-evaluator
(yaml-struct player (name hr avg) #:transparent)
(write-yaml (player "Mark McGwire" 65 0.278))
(string->yaml
 "!!struct:player {hr: 65, avg: 0.278, name: Mark McGwire}")]

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
  @item{a @racket[yaml-struct?] whose fields are all @racket[yaml?]}]}

@defparam[yaml-null null any/c]{
A parameter that determines the Racket value that corresponds to a YAML
``@tt{null}'' (empty scalar) value. It is @racket['null] by default.}

@defproc[(yaml-struct? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is an instance of a YAML structure,
@racket[#f] otherwise.}

@defform[(yaml-struct id maybe-super (field ...)
                      struct-option ...)]{
Creates a new structure that can be used as a YAML expression. This
macro expands to a call to @racket[struct] that uses @racket[#:methods]
to register itself with the YAML implementation.
}

@section{Reading YAML}

@defproc[(read-yaml
           [source-name any/c 'input]
           [in input-port? (current-input-port)])
         yaml?]{
Parses the first
@link["http://www.yaml.org/spec/1.2/spec.html#id2800132"]{YAML document}
from @racket[in] and returns the corresponding YAML expression in Racket.
The @racket[source-name] is used to identify the source of the input in
error messages.}

@defproc[(read-yaml*
           [source-name any/c 'input]
           [in input-port? (current-input-port)])
         (listof yaml?)]{
Like @racket[read-yaml], but parses @emph{all}
@link["http://www.yaml.org/spec/1.2/spec.html#id2800132"]{YAML documents}
from @racket[in] and returns a list of the corresponding YAML expressions
in Racket.}

@defproc[(string->yaml
           [str string?])
         yaml?]{
Equivalent to
@racketblock[
(with-input-from-string str
  (λ () (read-yaml 'string)))]}

@defproc[(string->yaml*
           [str string?])
         (listof yaml?)]{
Equivalent to
@racketblock[
(with-input-from-string str
  (λ () (read-yaml* 'string)))]}

@section{Writing YAML}

@defproc[(write-yaml
           [document yaml?]
           [out output-port? (current-output-port)])
         void?]{
Equivalent to
@racketblock[(write-yaml* (list document) out ....)]
Accepts the same keyword arguments as @racket[write-yaml*]
(represented above by @racket[....]).}

@defproc[(write-yaml*
           [documents (listof yaml?)]
           [out output-port? (current-output-port)]
           [#:canonical canonical boolean? #f]
           [#:indent indent exact-positive-integer? 2]
           [#:width width exact-positive-integer? 80]
           [#:explicit-start explicit-start boolean? #f]
           [#:explicit-end explicit-end boolean? #f]
           [#:scalar-style scalar-style (or/c #\" #\' #\| #\> 'plain) 'plain]
           [#:style style (or/c 'block 'flow 'best) 'best])
         void?]{
Writes a sequence of Racket YAML expressions to @racket[out] as YAML
text formatted with the keyword arguments. See the
@link["http://www.yaml.org/spec/1.2/spec.html"]{YAML specification}
for more information on style.}

@defproc[(yaml->string
           [document yaml?])
         string?]{
Equivalent to
@racketblock[
(with-output-to-string
  (λ () (write-yaml document ....)))]
Accepts the same keyword arguments as @racket[write-yaml]
(represented above by @racket[....]).}

@defproc[(yaml*->string
           [documents (listof yaml?)])
         string?]{
Equivalent to
@racketblock[
(with-output-to-string
  (λ () (write-yaml* documents ....)))]
Accepts the same keyword arguments as @racket[write-yaml*]
(represented above by @racket[....]).}
