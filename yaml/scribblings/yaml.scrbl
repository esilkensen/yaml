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

@section{YAML Expressions}

@defproc[(yaml? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a YAML expression, @racket[#f] otherwise.

This module defines a subset of Racket values that can be represented as or
constructed from YAML strings, and this predicate checks for such values.

The following table shows how the standard YAML tags correspond to values in
Racket.

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
       @racket[date?]))
]

The Racket-specific tag @tt{!!racket/pair} corresponds to a
@racket[(cons/c yaml? yaml?)] value, and is also supported.
Finally,
application-specific tags can be implemented with the
@racket[(yaml-representers)] and @racket[(yaml-constructors)] parameters,
and this predicate takes those into account.
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

@section{Reading YAML}

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
           [in input-port? (current-input-port)]
           [#:allow-undefined? allow-undefined? boolean? #f])
         (listof yaml?)]{
Like @racket[read-yaml], but parses @emph{all}
@link["http://www.yaml.org/spec/1.2/spec.html#id2800132"]{documents}
from @racket[in] and returns a list of the corresponding YAML expressions.}

@defproc[(string->yaml
           [str string?]
           [#:allow-undefined? allow-undefined? boolean? #f])
         yaml?]{
Equivalent to
@racketblock[
(with-input-from-string str
  (thunk (read-yaml #:allow-undefined? allow-undefined?)))]

@examples[#:eval yaml-evaluator
(string->yaml "!invoice {id: 1, total: 251.42}")
(string->yaml "!invoice {id: 1, total: 251.42}"
              #:allow-undefined? #t)]}

@defproc[(string->yaml*
           [str string?]
           [#:allow-undefined? allow-undefined? boolean? #f])
         (listof yaml?)]{
Equivalent to
@racketblock[
(with-input-from-string str
  (thunk (read-yaml* #:allow-undefined? allow-undefined?)))]

@examples[#:eval yaml-evaluator
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
  "- St. Louis Cardinals\n"))]}

@defproc[(file->yaml
           [path path-string?]
           [#:allow-undefined? allow-undefined? boolean? #f]
           [#:mode mode-flag (or/c 'binary 'text) 'binary])
         yaml?]{
Equivalent to
@racketblock[
(with-input-from-file path
  (thunk (read-yaml #:allow-undefined? allow-undefined?))
  #:mode mode-flag)]}

@defproc[(file->yaml*
           [path path-string?]
           [#:allow-undefined? allow-undefined? boolean? #f]
           [#:mode mode-flag (or/c 'binary 'text) 'binary])
         (listof yaml?)]{
Equivalent to
@racketblock[
(with-input-from-file path
  (thunk (read-yaml* #:allow-undefined? allow-undefined?))
  #:mode mode-flag)]}

@section{Writing YAML}

@defproc[(write-yaml
           [document yaml?]
           [out output-port? (current-output-port)]
           [#:canonical? canonical? boolean? #f]
           [#:indent indent exact-positive-integer? 2]
           [#:width width exact-positive-integer? 80]
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
           [out output-port? (current-output-port)]
           [#:canonical? canonical? boolean? #f]
           [#:indent indent exact-positive-integer? 2]
           [#:width width exact-positive-integer? 80]
           [#:explicit-start? explicit-start? boolean? #f]
           [#:explicit-end? explicit-end? boolean? #f]
           [#:scalar-style scalar-style (or/c #\" #\' #\| #\> 'plain) 'plain]
           [#:style style (or/c 'block 'flow 'best) 'best]
           [#:sort-mapping mapping-less-than?
            (or/c (any/c any/c . -> . any/c) #f) #f]
           [#:sort-mapping-key mapping-extract-key
            (any/c . -> . any/c) identity])
         void?]{
Like @racket[write-yaml], but writes a @emph{sequence} of YAML expressions to
@racket[out] as text formatted with the same keyword arguments.
}

@defproc[(yaml->string
           [document yaml?]
           [#:canonical? canonical? boolean? #f]
           [#:indent indent exact-positive-integer? 2]
           [#:width width exact-positive-integer? 80]
           [#:explicit-start? explicit-start? boolean? #f]
           [#:explicit-end? explicit-end? boolean? #f]
           [#:scalar-style scalar-style (or/c #\" #\' #\| #\> 'plain) 'plain]
           [#:style style (or/c 'block 'flow 'best) 'best]
           [#:sort-mapping mapping-less-than?
            (or/c (any/c any/c . -> . any/c) #f) #f]
           [#:sort-mapping-key mapping-extract-key
            (any/c . -> . any/c) identity])
         string?]{
A wrapper around @racket[write-yaml] using @racket[with-output-to-string].
}

@defproc[(yaml*->string
           [documents (listof yaml?)]
           [#:canonical? canonical? boolean? #f]
           [#:indent indent exact-positive-integer? 2]
           [#:width width exact-positive-integer? 80]
           [#:explicit-start? explicit-start? boolean? #f]
           [#:explicit-end? explicit-end? boolean? #f]
           [#:scalar-style scalar-style (or/c #\" #\' #\| #\> 'plain) 'plain]
           [#:style style (or/c 'block 'flow 'best) 'best]
           [#:sort-mapping mapping-less-than?
            (or/c (any/c any/c . -> . any/c) #f) #f]
           [#:sort-mapping-key mapping-extract-key
            (any/c . -> . any/c) identity])
         string?]{
A wrapper around @racket[write-yaml*] using @racket[with-output-to-string].
}

@defproc[(yaml->file
           [document yaml?]
           [path path-string?]
           [#:canonical? canonical? boolean? #f]
           [#:indent indent exact-positive-integer? 2]
           [#:width width exact-positive-integer? 80]
           [#:explicit-start? explicit-start? boolean? #f]
           [#:explicit-end? explicit-end? boolean? #f]
           [#:scalar-style scalar-style (or/c #\" #\' #\| #\> 'plain) 'plain]
           [#:style style (or/c 'block 'flow 'best) 'best]
           [#:sort-mapping mapping-less-than?
            (or/c (any/c any/c . -> . any/c) #f) #f]
           [#:sort-mapping-key mapping-extract-key
            (any/c . -> . any/c) identity]
           [#:mode mode-flag (or/c 'binary 'text) 'binary]
           [#:exists exists-flag (or/c 'error 'append 'update
                                       'replace 'truncate 'truncate/replace)
                     'error])
         string?]{
A wrapper around @racket[write-yaml] using @racket[with-output-to-file].
}

@defproc[(yaml*->file
           [documents (listof yaml?)]
           [path path-string?]
           [#:canonical? canonical? boolean? #f]
           [#:indent indent exact-positive-integer? 2]
           [#:width width exact-positive-integer? 80]
           [#:explicit-start? explicit-start? boolean? #f]
           [#:explicit-end? explicit-end? boolean? #f]
           [#:scalar-style scalar-style (or/c #\" #\' #\| #\> 'plain) 'plain]
           [#:style style (or/c 'block 'flow 'best) 'best]
           [#:sort-mapping mapping-less-than?
            (or/c (any/c any/c . -> . any/c) #f) #f]
           [#:sort-mapping-key mapping-extract-key
            (any/c . -> . any/c) identity]
           
           [#:mode mode-flag (or/c 'binary 'text) 'binary]
           [#:exists exists-flag (or/c 'error 'append 'update
                                       'replace 'truncate 'truncate/replace)
                     'error])
         string?]{
A wrapper around @racket[write-yaml*] using @racket[with-output-to-file].
}

@section{Extending YAML}

TODO

@interaction[#:eval yaml-evaluator
(define (represent-vector vec)
  (represent-sequence "!vector" (vector->list vec)))
(define vector-representer (yaml-representer vector? represent-vector))
(parameterize ([yaml-representers (list vector-representer)])
  (write-yaml #(1 2 3) #:style 'flow))]

TODO

@interaction[#:eval yaml-evaluator
(define (construct-vector node)
  (list->vector (construct-sequence node)))
(define vector-constructor
  (yaml-constructor vector? "!vector" construct-vector))
(parameterize ([yaml-constructors (list vector-constructor)])
  (string->yaml "!vector [1, 2, 3]"))]

TODO

@defparam[yaml-representers representers (listof yaml-representer?)]{
A parameter that configures user-defined @emph{representers} to use when
writing YAML. It is @racket['()] by default.}

@defparam[yaml-constructors constructors
(listof (or/c yaml-constructor? yaml-multi-constructor?))]{
A parameter that configures user-defined @emph{constructors} to use when
reading YAML. It is @racket['()] by default.}
