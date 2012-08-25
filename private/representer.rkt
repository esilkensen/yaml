;;;;;; representer.rkt - YAML representer.    -*- Mode: Racket -*-

#lang racket

(require
 racket/date
 (except-in srfi/13 string-replace)
 "errors.rkt"
 "nodes.rkt"
 "utils.rkt"
 "yaml.rkt")

(provide make-representer)

(define (make-representer serialize [default-style #f] [default-flow-style #f])
  (define yaml-representers '())
  (define represented-objects (make-hash))
  (define object-keeper '())
  (define alias-key #f)

  (define (represent data)
    (serialize (represent-data data))
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
       (let loop ([kvs yaml-representers])
         (if (null? kvs)
             (scalar-node #f #f #f data #f)
             (let ([type? (caar kvs)] [repr (cdar kvs)])
               (if (type? data)
                   (repr data)
                   (loop (cdr kvs)))))))))

  (define (represent-scalar tag value [style #f])
    (unless style
      (set! style default-style))
    (let ([node (scalar-node #f #f tag value style)])
      (when alias-key
        (hash-set! represented-objects alias-key node))
      node))

  (define (represent-sequence tag sequence [flow-style #f])
    (let ([value '()]
          [best-style #t])
      (for ([item sequence])
        (let ([node-item (represent-data item)])
          (when (not (and (scalar-node? node-item)
                          (not (scalar-node-style node-item))))
            (set! best-style #f))
          (append! value (list node-item))))
      (unless flow-style
        (if (not (eq? 'best default-flow-style))
            (set! flow-style default-flow-style)
            (set! flow-style best-style)))
      (let ([node (sequence-node #f #f tag value flow-style)])
        (when alias-key
          (hash-set! represented-objects alias-key node))
        node)))

  (define (represent-mapping tag mapping [flow-style #f])
    (let ([value '()]
          [best-style #t])
      (for ([(key val) mapping])
        (let* ([node-key (represent-data key)]
               [node-value (represent-data val)])
          (when (not (and (scalar-node? node-key)
                          (not (scalar-node-style node-key))))
            (set! best-style #f))
          (when (not (and (scalar-node? node-value)
                          (not (scalar-node-style node-value))))
            (set! best-style #f))
          (append! value (list (cons node-key node-value)))))
      (unless flow-style
        (if (not (eq? 'best default-flow-style))
            (set! flow-style default-flow-style)
            (set! flow-style best-style)))
      (let ([node (mapping-node #f #f tag value flow-style)])
        (when alias-key
          (hash-set! represented-objects alias-key node))
        node)))

  (define (ignore-aliases? data)
    (or (equal? (yaml-null) data)
        (boolean? data)
        (string? data)
        (number? data)))

  (define (represent-null data)
    (represent-scalar "tag:yaml.org,2002:null" "null"))

  (define (represent-str data)
    (represent-scalar "tag:yaml.org,2002:str" data))

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

  (define (represent-pair data)
    (let ([value (list (car data) (cdr data))])
      (represent-sequence "tag:yaml.org,2002:racket/pair" value)))

  (define (add-representer! type? representer)
    (append! yaml-representers (list (cons type? representer))))

  (add-representer! (λ (x) (equal? x (yaml-null))) represent-null)
  (add-representer! string? represent-str)
  (add-representer! boolean? represent-bool)
  (add-representer! integer? represent-int)
  (add-representer! real? represent-float)
  (add-representer! list? represent-list)
  (add-representer! hash? represent-hash)
  (add-representer! set? represent-set)
  (add-representer! date? represent-date)
  (add-representer! pair? represent-pair)

  (values represent))
