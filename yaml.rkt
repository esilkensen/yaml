;;;;;; yaml.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require "constructor.rkt" "representer.rkt" "serializer.rkt")

(provide
 dump-file dump-string dump
 load-file load-string load)

(define (dump-file obj filename
                   #:default-style [default-style #f]
                   #:default-flow-style [default-flow-style 'None])
  (with-output-to-file filename
    (λ () (dump obj
                #:default-style default-style
                #:default-flow-style default-flow-style))))

(define (dump-string obj
                     #:default-style [default-style #f]
                     #:default-flow-style [default-flow-style 'None])
  (let ([out (open-output-string)])
    (dump obj out
          #:default-style default-style
          #:default-flow-style default-flow-style)
    (begin0 (get-output-string out)
      (close-output-port out))))

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

(define (load-file filename)
  (with-input-from-file filename
    (λ () (load))))

(define (load-string string)
  (with-input-from-string string
    (λ () (load))))

(define (load [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor "<input>" in))
  (get-single-data))
