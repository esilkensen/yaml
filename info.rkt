#lang setup/infotab

(define collection 'multi)
(define deps '("base" "srfi-lite-lib" "typed-racket-lib"))
(define build-deps
  '("rackunit-lib" "scribble-lib" "racket-doc" "sandbox-lib"
    "cover-coveralls"))
