#lang scribble/manual
@(require racket/sandbox racket/runtime-path scribble/eval
          (for-label (except-in racket load ...) "../main.rkt"))
@(define-runtime-path scribblings ".")
@(define main-path (build-path scribblings "../main.rkt"))
@(define yaml-evaluator
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket #:requires (list main-path))))))
@(interaction-eval
  #:eval yaml-evaluator
  (current-print pretty-print-handler))

@title{YAML}
@author{@(author+email "Erik Silkensen" "eriksilkensen@gmail.com")}
@defmodule[yaml]

This module provides utilities for parsing and emitting data in the YAML
data serialization format. The implementation is hosted at
@link["https://github.com/esilkensen/yaml"]{GitHub} and is based on
@link["http://pyyaml.org"]{PyYAML}. See the
@link["http://yaml.org"]{YAML web site} for more information about YAML.

@section[#:tag "expressions"]{YAML Expressions}

@defproc[(yaml? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a YAML expression, @racket[#f] otherwise.

This module defines a subset of Racket values that can be represented as or
constructed from YAML, and this predicate checks for such values.

The following table shows how the standard YAML tags, in addition to a number
of Racket-specific tags, correspond to values in Racket.

@tabular[#:sep @hspace[2] #:row-properties '(bottom-border ())
(list
 (list @bold{YAML tag} @bold{Racket type})
 (list @link["http://yaml.org/type/map.html"]{@tt{!!map}}
       @racket[(hash/c yaml? yaml?)])
 (list (list @link["http://yaml.org/type/omap.html"]{@tt{!!omap}}
             ", "
             @link["http://yaml.org/type/pairs.html"]{@tt{!!pairs}})
       @racket[(listof (cons/c yaml? yaml?))])
 (list @link["http://yaml.org/type/set.html"]{@tt{!!set}}
       @racket[(set/c yaml?)])
 (list @link["http://yaml.org/type/seq.html"]{@tt{!!seq}}
       @racket[(listof yaml?)])
 (list @link["http://yaml.org/type/binary.html"]{@tt{!!binary}}
       @racket[bytes?])
 (list @link["http://yaml.org/type/bool.html"]{@tt{!!bool}}
       @racket[boolean?])
 (list @link["http://yaml.org/type/float.html"]{@tt{!!float}}
       @racket[inexact-real?])
 (list @link["http://yaml.org/type/int.html"]{@tt{!!int}}
       @racket[exact-integer?])
 (list @link["http://yaml.org/type/null.html"]{@tt{!!null}}
       @racket[yaml-null?])
 (list @link["http://yaml.org/type/str.html"]{@tt{!!str}}
       @racket[string?])
 (list @link["http://yaml.org/type/timestamp.html"]{@tt{!!timestamp}}
       @racket[date?])
 (list @tt{!!racket/pair} @racket[(cons/c yaml? yaml?)])
 (list @tt{!!racket/vector} @racket[(vector/c yaml?)])
 (list @tt{!!racket/symbol} @racket[symbol?]))
]

This module can be @seclink["extending"]{extended} with support for
application-specific tags using custom @emph{representers} and
@emph{constructors}, and this predicate checks for those values as well.
}

@defparam[yaml-null null any/c]{
A parameter that determines the value that corresponds to a YAML
``@tt{null}'' (or empty scalar) value. It is @racket['null] by default.}

@defproc[(yaml-null? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is equal to @racket[(yaml-null)], @racket[#f]
otherwise.

@examples[#:eval yaml-evaluator
(yaml-null? 'null)
(yaml-null? '())
(parameterize ([yaml-null '()])
  (yaml-null? '()))]}

@section[#:tag "reading"]{Reading YAML}

@defproc[(read-yaml
           [in input-port? (current-input-port)]
           [#:allow-undefined? allow-undefined? boolean? #f])
         yaml?]{
Parses a @emph{single}
@link["http://www.yaml.org/spec/1.2/spec.html#id2800132"]{document}
from @racket[in] and returns a corresponding YAML expression. If the input
contains more than one document, an @racket[exn:fail:user] exception is raised.

The @racket[#:allow-undefined?] keyword argument controls whether an undefined
tag should raise an exception (the default behavior), or be interpreted as either
a scalar, sequence, or mapping value.}

@defproc[(read-yaml*
           [in input-port? (current-input-port)])
         (listof yaml?)]{
Like @racket[read-yaml], but parses @emph{all}
@link["http://www.yaml.org/spec/1.2/spec.html#id2800132"]{documents}
from @racket[in] and returns a list of the corresponding YAML expressions.
This function accepts all keyword arguments accepted by @racket[read-yaml],
which have been elided above to simplify the presentation.
}

@deftogether[(@defproc[(string->yaml
                        [str string?])
                       yaml?]
              @defproc[(string->yaml*
                        [str string?])
                       (listof yaml?)]
              @defproc[(file->yaml
                        [path path-string?]
                        [#:mode mode-flag (or/c 'binary 'text) 'binary])
                       yaml?]
              @defproc[(file->yaml*
                        [path path-string?]
                        [#:mode mode-flag (or/c 'binary 'text) 'binary])
                       (listof yaml?)])]{
Wrappers around @racket[read-yaml] and @racket[read-yaml*] using
@racket[with-input-from-string] and @racket[with-input-from-file].
These functions accept all keyword arguments accepted by @racket[read-yaml] and
@racket[read-yaml*], which have been elided above to simplify the presentation.

@examples[#:eval yaml-evaluator
(string->yaml "!invoice {id: 1, total: 251.42}")
(string->yaml "!invoice {id: 1, total: 251.42}"
              #:allow-undefined? #t)
(string->yaml*
 (string-append
  "# Ranking of 1998 home runs\n"
  "---\n"
  "- Mark McGwire\n"
  "- Sammy Sosa\n"
  "- Ken Griffey\n"
  "\n"
  "# Team ranking\n"
  "---\n"
  "- Chicago Cubs\n"
  "- St. Louis Cardinals\n"))
]
}

@section[#:tag "writing"]{Writing YAML}

@defproc[(write-yaml
           [document yaml?]
           [out output-port? (current-output-port)]
           [#:canonical? canonical? boolean? #f]
           [#:indent indent exact-positive-integer? 2]
           [#:width width exact-positive-integer? 80]
           [#:allow-unicode? boolean? #f]
           [#:explicit-start? explicit-start? boolean? #f]
           [#:explicit-end? explicit-end? boolean? #f]
           [#:scalar-style scalar-style (or/c #\" #\' #\| #\> 'plain) 'plain]
           [#:style style (or/c 'block 'flow 'best) 'best]
           [#:sort-mapping mapping-less-than?
            (or/c (any/c any/c . -> . any/c) #f) #f]
           [#:sort-mapping-key mapping-extract-key
            (any/c . -> . any/c) identity])
         void?]{
Writes a YAML expression to @racket[out] as text formatted with the keyword
arguments.

The @racket[#:sort-mapping] argument @racket[mapping-less-than?] can be used
to @racket[sort] the elements of mappings. When it is a function, for any
@racket[mapping] the sorting procedure is essentially
@racketblock[
(sort (hash->list mapping)
      mapping-less-than?
      #:key mapping-extract-key)
]

@examples[#:eval yaml-evaluator
(write-yaml 42 #:canonical? #t)
(write-yaml
 '#hash(("Apple" . 3.99) ("Orange" . 2.15) ("Cherry" . 4.12))
 #:style 'block
 #:sort-mapping string<?
 #:sort-mapping-key car)
]
}

@defproc[(write-yaml*
           [documents (listof yaml?)]
           [out output-port? (current-output-port)])
         void?]{
Like @racket[write-yaml], but writes a @emph{sequence} of YAML expressions to
@racket[out] as text formatted with the same keyword arguments.
This function accepts all keyword arguments accepted by @racket[write-yaml],
which have been elided above to simplify the presentation.
}

@deftogether[(@defproc[(yaml->string
                        [document yaml?])
                       string?]
              @defproc[(yaml*->string
                        [documents (listof yaml?)])
                       string?]
              @defproc[(yaml->file
                        [document yaml?]
                        [path path-string?])
                       void?]
              @defproc[(yaml*->file
                        [documents (listof yaml?)]
                        [path path-string?])
                       void?])]{
Wrappers around @racket[write-yaml] and @racket[write-yaml*] using
@racket[with-output-to-string] and @racket[with-output-to-file].
These functions accept all keyword arguments accepted by @racket[write-yaml]
and @racket[write-yaml*], which have been elided above to simplify the
presentation.
}

@section[#:tag "extending"]{Extending YAML}

This module provides a simple API for extending the set of Racket values that
can be written to or read from YAML. The idea is to write functions that
represent Racket values as and construct them from YAML @emph{nodes}.

As a running example throughout this section, we will assume the definition of
a @racket[struct] for baseball players, and show how to read and write instances
of it in YAML.
@interaction[#:eval yaml-evaluator
(struct player (name hr avg) #:transparent)
]

@deftogether[(@defproc[(node? [v any/c]) boolean?]
              @defproc[(scalar-node? [v any/c]) boolean?]
              @defproc[(sequence-node? [v any/c]) boolean?]
              @defproc[(mapping-node? [v any/c]) boolean?])]{
Returns @racket[#t] if @racket[v] is a YAML
(@link["http://yaml.org/spec/1.1/#scalar/information model"]{scalar},
@link["http://yaml.org/spec/1.1/#sequence/information model"]{sequence}, or
@link["http://yaml.org/spec/1.1/#mapping/information model"]{mapping})
@link["http://yaml.org/spec/1.1/#node/information model"]{node},
@racket[#f] otherwise.
}

@subsection[#:tag "represent"]{Representers}

@defparam[yaml-representers representers (listof yaml-representer?)]{
A parameter that configures user-defined @emph{representers} to use when
writing YAML. It is @racket['()] by default.
}

@defproc[(yaml-representer? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a YAML representer, @racket[#f] otherwise.
}

@defproc[(yaml-representer
           [type? (any/c . -> . boolean?)]
           [represent (any/c . -> . node?)])
         yaml-representer?]{
Returns a representer that turns values of a given @racket[type?] into nodes.

@examples[#:eval yaml-evaluator
(define (represent-player p)
  (define mapping (make-hash))
  (hash-set! mapping "name" (player-name p))
  (hash-set! mapping "hr" (player-hr p))
  (hash-set! mapping "avg" (player-avg p))
  (represent-mapping "!player" mapping))
(define player-representer
  (yaml-representer player? represent-player))
(parameterize ([yaml-representers (list player-representer)])
  (write-yaml (player "Mark McGwire" 65 0.278)))
]
}

@deftogether[(@defproc[(represent-scalar [tag string?] [str string?])
                       scalar-node?]
              @defproc[(represent-sequence [tag string?] [lst (listof yaml?)])
                       sequence-node?]
              @defproc[(represent-mapping [tag string?]
                                          [hash (hash/c yaml? yaml?)])
                       mapping-node?])]{
Returns a value as a YAML (scalar, sequence, or mapping) node with a given
@racket[tag].
}

@subsection[#:tag "construct"]{Constructors}

@defparam[yaml-constructors constructors
          (listof yaml-constructor?)]{
A parameter that configures user-defined @emph{constructors} to use when
reading YAML. It is @racket['()] by default.
}

@defproc[(yaml-constructor? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a YAML constructor, @racket[#f] otherwise.
}

@defproc[(yaml-constructor
           [type? (any/c . -> . boolean?)]
           [tag string?]
           [construct (node? . -> . yaml?)])
         yaml-constructor?]{
Returns a constructor that turns nodes into values of a given @racket[type?].

@examples[#:eval yaml-evaluator
(define (construct-player node)
  (define mapping (construct-mapping node))
  (player (hash-ref mapping "name")
          (hash-ref mapping "hr")
          (hash-ref mapping "avg")))
(define player-constructor
  (yaml-constructor player? "!player" construct-player))
(parameterize ([yaml-constructors (list player-constructor)])
  (string->yaml "!player {name: Sammy Sosa, hr: 63, avg: 0.288}"))
]
}

@defproc[(yaml-multi-constructor? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a YAML multi-constructor, @racket[#f]
otherwise.

Note that @racket[yaml-multi-constructor?] is a subtype of
@racket[yaml-constructor?].
}

@defproc[(yaml-multi-constructor
           [type? (any/c . -> . boolean?)]
           [tag-prefix string?]
           [construct (string? node? . -> . yaml?)])
         yaml-multi-constructor?]{
Returns a multi-constructor that turns nodes into values of a given
@racket[type?].
}

@deftogether[(@defproc[(construct-scalar [node scalar-node?])
                       string?]
              @defproc[(construct-sequence [node sequence-node?])
                       (listof yaml?)]
              @defproc[(construct-mapping [node mapping-node?])
                       (hash/c yaml? yaml?)])]{
Constructs a (scalar, sequence, or mapping) value from its YAML @racket[node]
representation.
}
