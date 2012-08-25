;;;;;; main.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "private/constructor.rkt"
 "private/representer.rkt"
 "private/serializer.rkt")

(provide
 (contract-out
  [load-file (-> path-string? any/c)]
  [load-string (-> string? any/c)]
  [load-file/all (-> path-string? list?)]
  [load-string/all (-> string? list?)]
  [load (->* () (input-port? any/c) any/c)]
  [load-all (->* () (input-port? any/c) list?)]
  [dump
   (->* (any/c)
        (output-port?
         #:default-style (or/c #f char?)
         #:default-flow-style (or/c boolean? 'best))
        void?)]
  [dump-all
   (->* (list?)
        (output-port?
         #:default-style (or/c #f char?)
         #:default-flow-style (or/c boolean? 'best))
        void?)]))

(define (load-file path)
  (with-input-from-file path
    (λ () (load path))))

(define (load-string str)
  (with-input-from-string str
    (λ () (load 'string))))

(define (load-file/all path)
  (with-input-from-file path
    (λ () (load-all path))))

(define (load-string/all str)
  (with-input-from-string str
    (λ () (load-all 'string))))

(define (load [name 'input] [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor name in))
  (get-single-data))

(define (load-all [name 'input] [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor name in))
  (let loop ([docs '()])
    (if (check-data?)
        (loop (cons (get-data) docs))
        (reverse docs))))

(define (dump document [out (current-output-port)]
              #:default-style [default-style #f]
              #:default-flow-style [default-flow-style 'best])
  (dump-all (list document) out
            #:default-style default-style
            #:default-flow-style default-flow-style))

(define (dump-all documents [out (current-output-port)]
                  #:default-style [default-style #f]
                  #:default-flow-style [default-flow-style 'best])
  (define-values (open close serialize)
    (make-serializer out))
  (define represent
    (make-representer serialize default-style default-flow-style))
  (open)
  (for ([data documents])
    (represent data))
  (close))
