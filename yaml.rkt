;;;;;; yaml.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "constructor.rkt"
 "representer.rkt"
 "serializer.rkt")

(provide
 (contract-out
  [load-file (-> string? any/c)]
  [load-string (-> string? any/c)]
  [load-file/all (-> string? list?)]
  [load-string/all (-> string? list?)]
  [load (->* (string? input-port?) any/c)]
  [load-all (->* (string? input-port?) list?)]
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

(define (load-file filename)
  (with-input-from-file filename
    (λ () (load filename))))

(define (load-string string)
  (with-input-from-string string
    (λ () (load "<string>"))))

(define (load-file/all filename)
  (with-input-from-file filename
    (λ () (load-all filename))))

(define (load-string/all string)
  (with-input-from-string string
    (λ () (load-all "<string>"))))

(define (load [name "<input>"] [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor name in))
  (get-single-data))

(define (load-all [name "<input>"] [in (current-input-port)])
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
