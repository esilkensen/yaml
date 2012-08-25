#lang scribble/manual
@(require planet/scribble
	  (for-label (except-in racket load) (this-package-in main)))

@title{YAML}
@author{@(author+email "Erik Silkensen" "ejs@ccs.neu.edu")}
@defmodule/this-package[main]

This module provides a @link["http://yaml.org"]{YAML} parser and emitter
for Racket, ported from @link["http://pyyaml.org"]{PyYAML}.

@section{Dump}

The following

@defproc[(dump [doc any/c] [out output-port? (current-output-port)] [#:style default-style (or/c char? #f) #f] [#:flow-style default-flow-style (or/c boolean? 'best) 'best]) void?]{
Dump Documentation.
}

@defproc[(dump-all [docs list?] [out output-port? (current-output-port)] [#:style default-style (or/c char? #f) #f] [#:flow-style default-flow-style (or/c boolean? 'best) 'best]) void?]{
Dump-all Documentation.
}

@section{Load}

The following 

@defproc[(load [name any/c 'input] [in input-port? (current-input-port)]) any/c]{
Load Documentation.
}

@defproc[(load-all [name any/c 'input] [in input-port? (current-input-port)]) list?]{
Load-all Documentation.
}

@defproc[(load-file [path path-string?]) any/c]{
Load-file Documentation.
}

@defproc[(load-string [str string?]) any/c]{
Load-string Documentation.
}

@defproc[(load-file/all [path path-string?]) list?]{
@defproc[(load-string/all [str string?]) list?]{
/all Documentation.
}
}
