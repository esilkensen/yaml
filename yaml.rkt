;;;;;; yaml.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require "constructor.rkt" "representer.rkt" "serializer.rkt")

(provide load-file load-string load dump)

(define (load-file filename)
  (with-input-from-file filename load))

(define (load-string string)
  (with-input-from-string string load))

(define (load [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor "<input>" in))
  (get-single-data))

(define (dump obj [out (current-output-port)]
              #:default-style [default-style #f]
              #:default-flow-style [default-flow-style 'None])
  (define-values (open close serialize)
    (make-serializer out))
  (define represent
    (make-representer serialize default-style default-flow-style))
  (open)
  (represent obj)
  (close))
