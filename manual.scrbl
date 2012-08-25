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

@section{Load}

The following 

@defproc[(load [name any/c 'input] [in input-port? (current-input-port)]) any/c]{
Documentation.
}

@defproc[(load-all [name any/c 'input] [in input-port? (current-input-port)]) list?]{
Documentation.
}

@defproc[(load-file [path path-string?]) any/c]{
Documentation.
}

@defproc[(load-string [str string?]) any/c]{
Documentation.
}

@defproc[(load-file/all [path path-string?]) list?]{
@defproc[(load-string/all [str string?]) list?]{
Documentation.
}
}
