;;;;;; main.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "private/constructor.rkt"
 "private/representer.rkt"
 "private/serializer.rkt")

(provide
 (contract-out
  [yaml? (-> any/c boolean?)]
  [yaml-null (case-> (-> any/c) (-> any/c void?))]
  [read-yaml
   (->* () (input-port? any/c #:null any/c) yaml?)]
  [read-yaml*
   (->* () (input-port? any/c #:null any/c) (listof yaml?))]
  [string->yaml
   (->* (string?) (#:null any/c) yaml?)]
  [string->yaml*
   (->* (string?) (#:null any/c) (listof yaml?))]
  [write-yaml
   (->* (yaml?)
        (output-port?
         #:null any/c
         #:style (or/c #f char?)
         #:flow-style (or/c boolean? 'best))
        void?)]
  [write-yaml*
   (->* ((listof yaml?))
        (output-port?
         #:null any/c
         #:style (or/c #f char?)
         #:flow-style (or/c boolean? 'best))
        void?)]
  [yaml->string
   (->* (yaml?)
        (#:null any/c
         #:style (or/c #f char?)
         #:flow-style (or/c boolean? 'best))
        string?)]
  [yaml*->string
   (->* ((listof yaml?))
        (#:null any/c
         #:style (or/c #f char?)
         #:flow-style (or/c boolean? 'best))
        (listof string?))]))

(define yaml-null (make-parameter 'null))

(define (yaml? v)
  (or (equal? v (yaml-null))
      (string? v)
      (boolean? v)
      (exact-integer? v)
      (inexact-real? v)
      (date? v)
      (and (list? v)
           (andmap yaml? v))
      (and (hash? v)
           (for/and ([(key val) v])
             (and (yaml? key)
                  (yaml? val))))
      (and (set? v)
           (for/and ([val v])
             (yaml? val)))
      (and (pair? v)
           (yaml? (car v))
           (yaml? (cdr v)))))

(define (read-yaml [name 'input] [in (current-input-port)]
                   #:null [null (yaml-null)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor name in))
  (get-single-data))

(define (read-yaml* [name 'input] [in (current-input-port)]
                    #:null [null (yaml-null)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor name in))
  (let loop ([docs '()])
    (if (check-data?)
        (loop (cons (get-data) docs))
        (reverse docs))))

(define (string->yaml str #:null [null (yaml-null)])
  (with-input-from-string str
    (λ () (read-yaml 'string #:null null))))

(define (string->yaml* str #:null [null (yaml-null)])
  (with-input-from-string str
    (λ () (read-yaml* 'string #:null null))))

(define (write-yaml document [out (current-output-port)]
                    #:null [null (yaml-null)]
                    #:style [default-style #f]
                    #:flow-style [default-flow-style 'best])
  (write-yaml* (list document) out
               #:null null
               #:style default-style
               #:flow-style default-flow-style))

(define (write-yaml* documents [out (current-output-port)]
                     #:null [null (yaml-null)]
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

(define (yaml->string document
                      #:null [null (yaml-null)]
                      #:style [default-style #f]
                      #:flow-style [default-flow-style 'best])
  (with-output-to-string
    (λ ()
      (write-yaml document
                  #:null null
                  #:style default-style
                  #:flow-style default-flow-style))))

(define (yaml*->string documents
                       #:null [null (yaml-null)]
                       #:style [default-style #f]
                       #:flow-style [default-flow-style 'best])
  (with-output-to-string
    (λ ()
      (write-yaml* documents
                   #:null null
                   #:style default-style
                   #:flow-style default-flow-style))))
