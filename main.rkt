;;;;;; main.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "private/constructor.rkt"
 "private/representer.rkt"
 "private/serializer.rkt"
 "private/yaml.rkt")

(provide (all-from-out "private/yaml.rkt"))

(provide
 (contract-out [read-yaml (->* () (any/c input-port?) yaml?)]))
(define (read-yaml [name 'input] [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor name in))
  (get-single-data))

(provide
 (contract-out [read-yaml* (->* () (any/c input-port?) (listof yaml?))]))
(define (read-yaml* [name 'input] [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor name in))
  (let loop ([docs '()])
    (if (check-data?)
        (loop (cons (get-data) docs))
        (reverse docs))))

(provide
 (contract-out [string->yaml (-> string? yaml?)]))
(define (string->yaml str)
  (with-input-from-string str
    (λ () (read-yaml 'string))))

(provide
 (contract-out [string->yaml* (-> string? (listof yaml?))]))
(define (string->yaml* str)
  (with-input-from-string str
    (λ () (read-yaml* 'string))))

(provide
 (contract-out
  [write-yaml
   (->* (yaml?)
        (output-port?
         #:style (or/c #f char?)
         #:flow-style (or/c boolean? 'best))
        void?)]))
(define (write-yaml document [out (current-output-port)]
                    #:style [default-style #f]
                    #:flow-style [default-flow-style 'best])
  (write-yaml* (list document) out
               #:style default-style
               #:flow-style default-flow-style))

(provide
 (contract-out
  [write-yaml*
   (->* ((listof yaml?))
        (output-port?
         #:style (or/c #f char?)
         #:flow-style (or/c boolean? 'best))
        void?)]))
(define (write-yaml* documents [out (current-output-port)]
                     #:style [default-style #f]
                     #:flow-style [default-flow-style 'best])
  (define-values (open close serialize)
    (make-serializer out))
  (define represent
    (make-representer serialize default-style default-flow-style))
  (open)
  (for ([data documents])
    (represent data))
  (close))

(provide
 (contract-out
  [yaml->string
   (->* (yaml?)
        (#:style (or/c #f char?)
         #:flow-style (or/c boolean? 'best))
        string?)]))
(define (yaml->string document
                      #:style [default-style #f]
                      #:flow-style [default-flow-style 'best])
  (with-output-to-string
    (λ ()
      (write-yaml document
                  #:style default-style
                  #:flow-style default-flow-style))))

(provide
 (contract-out
  [yaml*->string
   (->* ((listof yaml?))
        (#:style (or/c #f char?)
         #:flow-style (or/c boolean? 'best))
        string?)]))
(define (yaml*->string documents
                       #:style [default-style #f]
                       #:flow-style [default-flow-style 'best])
  (with-output-to-string
    (λ ()
      (write-yaml* documents
                   #:style default-style
                   #:flow-style default-flow-style))))
