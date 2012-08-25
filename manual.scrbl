#lang scribble/manual
@(require planet/scribble
	  (for-label (except-in racket load) (this-package-in main)))

@title{YAML}
@author{@(author+email "Erik Silkensen" "ejs@ccs.neu.edu")}
@defmodule/this-package[main]

This module provides utilities for parsing and emitting data in the YAML
data serialization format to and from Racket values. See the 
@link["http://yaml.org"]{YAML web site} for more information about YAML.
The implementation is ported from @link["http://pyyaml.org"]{PyYAML}.

@section{YAML Expressions}

The following

@defproc[(yaml? [v any/c]) boolean?]{
Expression Documentation.}

@defparam[yaml-null null any/c]{

The parameter}

@section{Generating YAML Text from YAML Expressions}

The following

@defproc[(write-yaml [doc yaml?] [out output-port? (current-output-port)] [#:null null any/c (yaml-null)] [#:style default-style (or/c char? #f) #f] [#:flow-style default-flow-style (or/c boolean? 'best) 'best]) void?]{
Write Documentation.}

@defproc[(write-yaml* [docs (listof yaml?)] [out output-port? (current-output-port)] [#:null null any/c (yaml-null)] [#:style default-style (or/c char? #f) #f] [#:flow-style default-flow-style (or/c boolean? 'best) 'best]) void?]{
Write-all Documentation.}

@defproc[(yaml->string [doc yaml?] [#:null null any/c (yaml-null)] [#:style default-style (or/c char? #f) #f] [#:flow-style default-flow-style (or/c boolean? 'best) 'best]) string?]{
To-string Documentation.}

@defproc[(yaml*->string [docs (listof yaml?)] [#:null null any/c (yaml-null)] [#:style default-style (or/c char? #f) #f] [#:flow-style default-flow-style (or/c boolean? 'best) 'best]) (listof string?)]{
To-string-all Documentation.}

@section{Parsing YAML Text into YAML Expressions}

The following 

@defproc[(read-yaml [name any/c 'input] [in input-port? (current-input-port)] [#:null null any/c (yaml-null)]) yaml?]{
Read Documentation.}

@defproc[(read-yaml* [name any/c 'input] [in input-port? (current-input-port)] [#:null null any/c (yaml-null)]) (listof yaml?)]{
Read-all Documentation.}

@defproc[(string->yaml [str string?] [#:null null any/c (yaml-null)]) yaml?]{
From-string Documentation.}

@defproc[(string->yaml* [str string?] [#:null null any/c (yaml-null)]) (listof yaml?)]{
From-string-all Documentation.}
