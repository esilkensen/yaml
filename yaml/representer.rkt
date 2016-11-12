;;;;;; representer.rkt - YAML representer.    -*- Mode: Racket -*-

#lang racket

(require
 racket/date
 (except-in srfi/13 string-replace)
 "errors.rkt"
 "nodes.rkt"
 "serializer.rkt"
 "utils.rkt"
 "yaml-expr.rkt")

(provide
 representer+c%
 (contract-out
  [representer% representer+c%]))

(define representer+c%
  (class/c
   (init-field
    [serializer (instanceof/c serializer+c%)]
    [scalar-style (or/c #\" #\' #\| #\> #f)]
    [style (or/c 'block 'flow 'best)]
    [sort-mapping (or/c (any/c any/c . -> . any/c) #f)]
    [sort-mapping-key (any/c . -> . any/c)])
   [represent (yaml? . ->m . void?)]
   [represent-scalar (string? string? . ->m . node?)]
   [represent-sequence (string? list? . ->m . node?)]
   [represent-mapping (string? hash? . ->m . node?)]
   [add (yaml-representer? . ->m . void?)]))

(define (representer-error message)
  (error 'representer message))

(define representer%
  (class object%
    (init-field
     serializer
     [scalar-style #f]
     [style 'best]
     [sort-mapping #f]
     [sort-mapping-key identity])
    
    (super-new)
    
    (define yaml-representers '())
    (define represented-objects (make-hash))
    (define object-keeper '())
    (define alias-key #f)
    
    (define/public (represent data)
      (send serializer serialize (represent-data data))
      (set! represented-objects (make-hash))
      (set! object-keeper '())
      (set! alias-key #f))

    (define (represent-data data)
      (call/cc
       (λ (return)
         (if (ignore-aliases? data)
             (set! alias-key #f)
             (set! alias-key (eq-hash-code data)))
         (when alias-key
           (when (hash-has-key? represented-objects alias-key)
             (return (hash-ref represented-objects alias-key)))
           (append! object-keeper (list data)))
         (let loop ([rs yaml-representers])
           (if (null? rs)
               (represent-str (format "~s" data))
               (if ((yaml-representer-type? (car rs)) data)
                   ((yaml-representer-represent (car rs)) data)
                   (loop (cdr rs))))))))
    
    (define/public (represent-scalar tag value)
      (define node (scalar-node #f #f tag value scalar-style))
      (when alias-key
        (hash-set! represented-objects alias-key node))
      node)

    (define/public (represent-sequence tag sequence)
      (define best-style #t)
      (define value
        (for/list ([item sequence])
          (let-values
              ([(node-item flow-style?)
                (represent-data/flow-style? item)])
            (when flow-style?
              (set! best-style #f))
            node-item)))
      (define node (sequence-node #f #f tag value (get-style best-style)))
      (when alias-key
        (hash-set! represented-objects alias-key node))
      node)
    
    (define/public (represent-mapping tag mapping)
      (define best-style #t)
      (define sequence
        (if sort-mapping
            (sort (hash->list mapping)
                  sort-mapping
                  #:key sort-mapping-key)
            (hash->list mapping)))
      (define value
        (for/list ([item sequence])
          (let-values
              ([(node-key key-flow-style?)
                (represent-data/flow-style? (car item))]
               [(node-value value-flow-style?)
                (represent-data/flow-style? (cdr item))])
            (when (or key-flow-style? value-flow-style?)
              (set! best-style #f))
            (cons node-key node-value))))
      (define node (mapping-node #f #f tag value (get-style best-style)))
      (when alias-key
        (hash-set! represented-objects alias-key node))
      node)
    
    (define (represent-data/flow-style? data)
      (define node (represent-data data))
      (define flow-style?
        (not (and (scalar-node? node)
                  (not (scalar-node-style node)))))
      (values node flow-style?))
    
    (define (get-style best-style)
      (if (eq? 'best style)
          best-style
          (eq? 'flow style)))
    
    (define (ignore-aliases? data)
      (or (yaml-null? data)
          (boolean? data)
          (string? data)
          (number? data)))
    
    (define (represent-null data)
      (represent-scalar "tag:yaml.org,2002:null" "null"))
    
    (define (represent-str data)
      (represent-scalar "tag:yaml.org,2002:str" data))
    
    (define (represent-binary data)
      (represent-scalar "tag:yaml.org,2002:binary" (bytes->string/utf-8 data)))
    
    (define (represent-bool data)
      (represent-scalar "tag:yaml.org,2002:bool" (if data "true" "false")))
    
    (define (represent-int data)
      (represent-scalar "tag:yaml.org,2002:int" (number->string data)))
    
    (define (represent-float data)
      (represent-scalar
       "tag:yaml.org,2002:float"
       (cond
         [(nan? data) ".nan"]
         [(infinite? data)
          (if (equal? data +inf.0) ".inf" "-.inf")]
         [else
          (let ([value (number->string data)])
            (if (and (not (string-index value #\.))
                     (string-index value #\e))
                (string-replace value "e" ".0e" #:all? #f)
                value))])))
    
    (define (represent-list data)
      (represent-sequence "tag:yaml.org,2002:seq" data))

    (define (represent-pair data)
      (let ([value (list (car data) (cdr data))])
        (represent-sequence "tag:yaml.org,2002:racket/pair" value)))
    
    (define (represent-hash data)
      (represent-mapping "tag:yaml.org,2002:map" data))
    
    (define (represent-set data)
      (let ([value (make-hash)])
        (for ([key data])
          (hash-set! value key (yaml-null)))
        (represent-mapping "tag:yaml.org,2002:set" value)))
    
    (define (represent-date data)
      (parameterize ([date-display-format 'iso-8601])
        ;; this isn't perfect, but it'll have to do...
        (let* ([time? (or (not (zero? (date-second data)))
                          (not (zero? (date-minute data)))
                          (not (zero? (date-hour data))))]
               [value (date->string data time?)])
          (when (and (date*? data)
                     (not (zero? (date*-nanosecond data))))
            (let ([microsecond (/ (date*-nanosecond data) 1000)])
              (set! value (format "~a.~a" value microsecond))))
          (represent-scalar "tag:yaml.org,2002:timestamp" value))))
    
    (define/public (add representer)
      (append! yaml-representers (list representer)))

    ;; Scalar Types
    (add (yaml-representer yaml-null? represent-null))
    (add (yaml-representer bytes? represent-binary))
    (add (yaml-representer boolean? represent-bool))
    (add (yaml-representer inexact-real? represent-float))
    (add (yaml-representer exact-integer? represent-int))
    (add (yaml-representer string? represent-str))
    (add (yaml-representer date? represent-date))

    ;; Collection Types
    (add (yaml-representer hash? represent-hash))
    (add (yaml-representer set? represent-set))
    (add (yaml-representer list? represent-list))
    
    ;; Racket Types
    (add (yaml-representer pair? represent-pair))))
