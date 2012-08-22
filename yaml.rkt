;;;;;; yaml.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require "constructor.rkt" "representer.rkt" "serializer.rkt")

(provide
 load-file
 load-string
 load-file/all
 load-string/all
 load
 load-all
 dump
 dump-all)

(define (load-file filename)
  (with-input-from-file filename load))

(define (load-string string)
  (with-input-from-string string load))

(define (load-file/all filename)
  (with-input-from-file filename load-all))

(define (load-string/all string)
  (with-input-from-string string load-all))

(define (load [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor "<input>" in))
  (get-single-data))

(define (load-all [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor "<input>" in))
  (let loop ([docs '()])
    (if (check-data?)
        (loop (cons (get-data) docs))
        (reverse docs))))

(define (dump document [out (current-output-port)]
              #:default-style [default-style #f]
              #:default-flow-style [default-flow-style 'None])
  (dump-all (list document) out
            #:default-style default-style
            #:default-flow-style default-flow-style))

(define (dump-all documents [out (current-output-port)]
                  #:default-style [default-style #f]
                  #:default-flow-style [default-flow-style 'None])
  (define-values (open close serialize)
    (make-serializer out))
  (define represent
    (make-representer serialize default-style default-flow-style))
  (open)
  (for ([data documents])
    (represent data))
  (close))
