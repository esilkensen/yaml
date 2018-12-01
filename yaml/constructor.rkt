;;;;;; constructor.rkt - YAML constructor.    -*- Mode: Racket -*-

#lang racket

(require
 racket/date
 net/base64
 (except-in srfi/13 string-replace)
 "composer.rkt"
 "errors.rkt"
 "nodes.rkt"
 "resolver.rkt"
 "utils.rkt"
 "yaml-expr.rkt")

(provide
 constructor+c%
 (contract-out
  [construct-file
   ((path-string?)
    (#:resolver (instanceof/c resolver+c%)
     #:allow-undefined? boolean?)
    . ->* .
    (listof yaml?))]
  [construct-string
   ((string?)
    (#:resolver (instanceof/c resolver+c%)
     #:allow-undefined? boolean?)
    . ->* .
    (listof yaml?))]
  [construct-all
   (()
    (input-port?
     #:resolver (instanceof/c resolver+c%)
     #:allow-undefined? boolean?)
    . ->* .
    (listof yaml?))]
  [constructor% constructor+c%]))

(define constructor+c%
  (class/c
   (init-field
    [in input-port?]
    [resolver (instanceof/c resolver+c%)]
    [allow-undefined? boolean?])
   [check-data? (->m boolean?)]
   [get-data (->m (or/c yaml? void?))]
   [get-single-data (->m (or/c yaml? #f))]
   [construct-scalar (scalar-node? . ->m . string?)]
   [construct-sequence (sequence-node? . ->m . (listof yaml?))]
   [construct-mapping (mapping-node? . ->m . (hash/c yaml? yaml?))]
   [add (yaml-constructor? . ->m . void?)]
   [add-multi (yaml-multi-constructor? . ->m . void?)]))

(define constructor-error (make-error 'constructor))

(define (construct-file filename
                        #:resolver [resolver (new resolver%)]
                        #:allow-undefined? [allow-undefined? #f])
  (with-input-from-file filename
    (λ () (construct-all (current-input-port)
                         #:resolver resolver
                         #:allow-undefined? allow-undefined?))))

(define (construct-string string
                          #:resolver [resolver (new resolver%)]
                          #:allow-undefined? [allow-undefined? #f])
  (with-input-from-string string
    (λ () (construct-all (current-input-port)
                         #:resolver resolver
                         #:allow-undefined? allow-undefined?))))

(define (construct-all [in (current-input-port)]
                       #:resolver [resolver (new resolver%)]
                       #:allow-undefined? [allow-undefined? #f])
  (define constructor (new constructor%
                           [in in]
                           [resolver resolver]
                           [allow-undefined? allow-undefined?]))
  (let loop ([data '()])
    (if (send constructor check-data?)
        (loop (cons (send constructor get-data) data))
        (reverse data))))

(define constructor%
  (class object%
    (init-field
     [in (current-input-port)]
     [resolver (new resolver%)]
     [allow-undefined? #f])
    
    (super-new)
    
    (define composer (new composer% [in in] [resolver resolver]))
    
    (define yaml-constructors (make-hash))
    (define yaml-multi-constructors (make-hash))
    (define constructed-objects (make-hash))
    (define recursive-objects (make-hash))
    
    (define/public (check-data?)
      (send composer check-node?))
    
    (define/public (get-data)
      (when (send composer check-node?)
        (construct-document (send composer get-node))))
    
    (define/public (get-single-data)
      (let ([node (send composer get-single-node)])
        (and node (construct-document node))))
    
    (define (construct-document node)
      (begin0 (construct-object node)
        (set! constructed-objects (make-hash))
        (set! recursive-objects (make-hash))))
    
    (define (construct-object node)
      (if (hash-has-key? constructed-objects node)
          (hash-ref constructed-objects node)
          (let ([constructor #f]
                [tag-suffix #f])
            (when (hash-has-key? recursive-objects node)
              (constructor-error
               #f "found unconstructable recursive node"
               (node-start node)))
            (hash-set! recursive-objects node #f)
            (let ([tag (node-tag node)]
                  [break #f])
              (cond
                [(hash-has-key? yaml-constructors tag)
                 (set! constructor (hash-ref yaml-constructors tag))]
                [else
                 (let loop ([ts (hash-keys yaml-multi-constructors)])
                   (unless (null? ts)
                     (cond
                       [(string-prefix? (car ts) tag)
                        (set! tag-suffix
                              (substring tag (string-length (car ts))))
                        (set! constructor
                              (hash-ref yaml-multi-constructors (car ts)))
                        (set! break #t)]
                       [else (loop (cdr ts))])))
                 (unless break
                   (set! constructor construct-undefined))]))
            (let ([data (if (not tag-suffix)
                            (constructor node)
                            (constructor tag-suffix node))])
              (hash-set! constructed-objects node data)
              (hash-remove! recursive-objects node)
              data))))
    
    (define/public (construct-scalar node)
      (unless (scalar-node? node)
        (constructor-error
         #f (format "expected a scalar node, but found ~a"
                    (node->string-rec node))
         (node-start node)))
      (scalar-node-value node))
    
    (define/public (construct-sequence node)
      (unless (sequence-node? node)
        (constructor-error
         #f (format "expected a sequence node, but found ~a"
                    (node->string-rec node))
         (node-start node)))
      (map (λ (child) (construct-object child)) (sequence-node-value node)))
    
    (define/public (construct-mapping node)
      (unless (mapping-node? node)
        (constructor-error
         #f (format "expected a mapping node, but found ~a"
                    (node->string-rec node))
         (node-start node)))
      (flatten-mapping! node)
      (let ([mapping (make-hash)])
        (for ([kv (mapping-node-value node)])
          (match-let ([(cons key-node value-node) kv])
            (let* ([key (construct-object key-node)]
                   [value (construct-object value-node)])
              (hash-set! mapping key value))))
        mapping))
    
    (define (flatten-mapping! node)
      (let ([merge '()] [index 0])
        (while (< index (length (mapping-node-value node)))
          (let ([value (mapping-node-value node)])
            (match-let ([(cons key-node value-node) (list-ref value index)])
              (cond [(equal? "tag:yaml.org,2002:merge" (node-tag key-node))
                     (set! value (remove (list-ref value index) value))
                     (cond [(mapping-node? value-node)
                            (flatten-mapping! value-node)
                            (append! merge (mapping-node-value value-node))]
                           [(sequence-node? value-node)
                            (let ([submerge '()])
                              (for ([subnode (sequence-node-value value-node)])
                                (unless (mapping-node? subnode)
                                  (constructor-error
                                   "while constructing a mapping"
                                   (format (string-append
                                            "expected a mapping for merging, "
                                            "but found a ~a")
                                           (if (sequence-node? subnode)
                                               "sequence"
                                               "scalar"))
                                   (node-start subnode)))
                                (flatten-mapping! subnode)
                                (append! submerge (mapping-node-value subnode)))
                              (for ([value (reverse submerge)])
                                (append! merge (list value))))]
                           [else
                            (constructor-error
                             "while constructing a mapping"
                             (format (string-append
                                      "expected a mapping or list of mappings "
                                      "for merging, but found a ~s")
                                     (scalar-node-value value-node))
                             (node-start value-node))])
                     (set-mapping-node-value! node value)]
                    [(equal? "tag:yaml.org,2002:value" (node-tag key-node))
                     (set-node-tag! key-node "tag:yaml.org,2002:str")
                     (set! index (add1 index))]
                    [else
                     (set! index (add1 index))]))))
        (unless (null? merge)
          (let ([value (mapping-node-value node)])
            (set-mapping-node-value! node (append merge value))))))
    
    (define (construct-yaml-null node)
      (construct-scalar node)
      (yaml-null))
    
    (define (construct-yaml-bool node)
      (let ([bool (string-downcase (construct-scalar node))])
        (cond
          [(member bool '("yes" "true" "on")) #t]
          [(member bool '("no" "false" "off")) #f]
          [else (constructor-error
                 #f (format "expected a boolean, but found ~a" bool)
                 (node-start node))])))
    
    (define (construct-yaml-int node)
      (define checked-string->number (make-checked-string->number node))
      (let ([value (string-replace
                    (format "~a" (construct-scalar node)) "_" "")]
            [sign 1])
        (let ([ch (string-ref value 0)])
          (when (char=? #\- ch)
            (set! sign -1))
          (when (or (char=? #\- ch) (char=? #\+ ch))
            (set! value (substring value 1))))
        (cond
          [(string=? "0" value) 0]
          [(string-prefix? "0b" value)
           (* sign (checked-string->number (substring value 2) 2))]
          [(string-prefix? "0x" value)
           (* sign (checked-string->number (substring value 2) 16))]
          [(char=? #\0 (string-ref value 0))
           (* sign (checked-string->number value 8))]
          [(string-index value #\:)
           (let ([base 1]
                 [int-value 0]
                 [parts (string-split value ":")])
             (for ([digit (reverse (map checked-string->number parts))])
               (set! int-value (+ int-value (* digit base)))
               (set! base (* base 60)))
             (* sign int-value))]
          [else
           (* sign (checked-string->number value))])))
    
    (define (construct-yaml-float node)
      (define checked-string->number (make-checked-string->number node))
      (let ([value (string-replace
                    (string-downcase
                     (format "~a" (construct-scalar node))) "_" "")]
            [sign 1])
        (let ([ch (string-ref value 0)])
          (when (char=? #\- ch)
            (set! sign -1))
          (when (or (char=? #\- ch) (char=? #\+ ch))
            (set! value (substring value 1))))
        (cond
          [(string=? ".inf" value)
           (* sign +inf.0)]
          [(string=? ".nan" value)
           +nan.0]
          [(string-index value #\:)
           (let ([base 1]
                 [float-value 0.0]
                 [parts (string-split value ":")])
             (for ([digit (reverse (map checked-string->number parts))])
               (set! float-value (* digit base))
               (set! base (* base 60)))
             (* sign float-value))]
          [else
           (* 1.0 sign (checked-string->number value))])))
    
    (define (construct-yaml-binary node)
      (string->bytes/utf-8
       (format "~a" (construct-scalar node))))
    
    (define (construct-yaml-timestamp node)
      (define timestamp-regexp
        (regexp
         (string-append
          "^([0-9][0-9][0-9][0-9])"
          "-([0-9][0-9]?)"
          "-([0-9][0-9]?)"
          "(?:(?:[Tt]|[ \\t]+)"
          "([0-9][0-9]?)"
          ":([0-9][0-9])"
          ":([0-9][0-9])"
          "(?:\\.([0-9]*))?"
          "(?:[ \\t]*(Z|([-+])([0-9][0-9]?)"
          "(?::([0-9][0-9]))?))?)?$")))
      (define checked-string->number (make-checked-string->number node))
      (define values (regexp-match timestamp-regexp (scalar-node-value node)))
      (define year (checked-string->number (list-ref values 1)))
      (define month (checked-string->number (list-ref values 2)))
      (define day (checked-string->number (list-ref values 3)))
      (define (get-hour)
        (and (list-ref values 4)
             (checked-string->number (list-ref values 4))))
      (define (get-minute)
        (and (list-ref values 5)
             (checked-string->number (list-ref values 5))))
      (define (get-second)
        (and (list-ref values 6)
             (checked-string->number (list-ref values 6))))
      (define (get-fraction)
        (let ([value (list-ref values 7)])
          (and value (substring value 0 (min 6 (string-length value))))))
      (define (get-tz-sign)
        (and (list-ref values 9)
             (if (equal? "-" (list-ref values 9)) -1 +1)))
      (define (get-tz-hour)
        (and (list-ref values 10)
             (checked-string->number (list-ref values 10))))
      (define (get-tz-minute)
        (and (list-ref values 11)
             (checked-string->number (list-ref values 11))))
      (if (not (get-hour))
          (seconds->date (find-seconds 0 0 0 day month year #f) #f)
          (let ([hour (get-hour)]
                [minute (get-minute)]
                [second (get-second)]
                [fraction 0])
            (when (get-fraction)
              (set! fraction (get-fraction))
              (while (< (string-length fraction) 6)
                (set! fraction (string-append fraction "0")))
              (set! fraction (* 1000 (checked-string->number fraction))))
            (if (get-tz-sign)
                (let* ([tz-hour (get-tz-hour)]
                       [tz-minute (or (get-tz-minute) 0)]
                       [tz-sign (get-tz-sign)]
                       [offset (* tz-sign
                                  (+ (* tz-hour 60 60)
                                     (* tz-minute 60)))]
                       [base (find-seconds
                              second minute hour day month year #f)]
                       ;; subtract offset to count tz
                       [d0 (seconds->date (- base offset) #f)])
                  (date*
                   (date-second d0) (date-minute d0) (date-hour d0)
                   (date-day d0) (date-month d0) (date-year d0)
                   0 0 #f 0 fraction "UTC"))
                (date*
                 second minute hour day month year 0 0 #f 0 fraction "UTC")))))
    
    (define (construct-yaml-omap node)
      (construct-yaml-omap/pairs node #f))
    
    (define (construct-yaml-pairs node)
      (construct-yaml-omap/pairs node #t))
    
    (define (construct-yaml-omap/pairs node dups?)
      (unless (sequence-node? node)
        (constructor-error
         "while constructing an ordered map"
         (format "expected a sequence, but found ~a"
                 (node->string node))
         (node-start node)))
      (define keys (mutable-set))
      (for/list ([subnode (sequence-node-value node)])
        (unless (mapping-node? subnode)
          (constructor-error
           "while constructing an ordered map"
           (format "expected a mapping of length 1, but found ~a"
                   (node->string subnode))
           (node-start subnode)))
        (unless (= 1 (length (mapping-node-value subnode)))
          (constructor-error
           "while constructing an ordered map"
           (format
            "expected a single mapping item, but found ~a items"
            (length (mapping-node-value subnode)))
           (node-start subnode)))
        (match-let ([(cons key-node value-node)
                     (car (mapping-node-value subnode))])
          (let* ([key (construct-object key-node)]
                 [value (construct-object value-node)])
            (when (and (not dups?) (set-member? keys key))
              (constructor-error
               "while constructing an ordered map"
               (format "found a duplicate key: ~a" key)
               (node-start key-node)))
            (set-add! keys key)
            (cons key value)))))
    
    (define (construct-yaml-set node)
      (list->set (hash-keys (construct-mapping node))))
    
    (define (construct-yaml-str node)
      (construct-scalar node))
    
    (define (construct-yaml-seq node)
      (construct-sequence node))
    
    (define (construct-yaml-map node)
      (construct-mapping node))
    
    (define (construct-racket-pair node)
      (let ([value (sequence-node-value node)])
        (unless (= 2 (length value))
          (constructor-error
           "while constructing a pair"
           (format "expected 2 values, but found ~a" (length value))
           (node-start node)))
        (cons (construct-object (first value))
              (construct-object (second value)))))

    (define (construct-racket-vector node)
      (list->vector (construct-sequence node)))

    (define (construct-racket-symbol node)
      (string->symbol (construct-scalar node)))
    
    (define (construct-undefined node)
      (unless allow-undefined?
        (define message "could not determine a constructor for the tag ~a")
        (constructor-error
         #f (format message (node-tag node)) (node-start node)))
      (cond
        [(mapping-node? node) (construct-mapping node)]
        [(sequence-node? node) (construct-sequence node)]
        [else (construct-scalar node)]))
    
    (define/public (add constructor)
      (define tag (yaml-constructor-tag constructor))
      (define construct (yaml-constructor-construct constructor))
      (add-constructor tag construct))

    (define (add-constructor tag construct)
      (hash-set! yaml-constructors tag construct))
    
    (define/public (add-multi multi-constructor)
      (define tag-prefix (yaml-multi-constructor-tag-prefix multi-constructor))
      (define construct (yaml-multi-constructor-construct multi-constructor))
      (add-multi-constructor tag-prefix construct))

    (define (add-multi-constructor tag-prefix construct)
      (hash-set! yaml-multi-constructors tag-prefix construct))

    ;; Collection Types
    (add-constructor "tag:yaml.org,2002:map" construct-yaml-map)
    (add-constructor "tag:yaml.org,2002:omap" construct-yaml-omap)
    (add-constructor "tag:yaml.org,2002:pairs" construct-yaml-pairs)
    (add-constructor "tag:yaml.org,2002:set" construct-yaml-set)
    (add-constructor "tag:yaml.org,2002:seq" construct-yaml-seq)

    ;; Scalar Types
    (add-constructor "tag:yaml.org,2002:binary" construct-yaml-binary)
    (add-constructor "tag:yaml.org,2002:bool" construct-yaml-bool)
    (add-constructor "tag:yaml.org,2002:float" construct-yaml-float)
    (add-constructor "tag:yaml.org,2002:int" construct-yaml-int)
    (add-constructor "tag:yaml.org,2002:null" construct-yaml-null)
    (add-constructor "tag:yaml.org,2002:str" construct-yaml-str)
    (add-constructor "tag:yaml.org,2002:timestamp" construct-yaml-timestamp)

    ;; Racket Types
    (add-constructor "tag:yaml.org,2002:racket/pair" construct-racket-pair)
    (add-constructor "tag:yaml.org,2002:racket/vector" construct-racket-vector)
    (add-constructor "tag:yaml.org,2002:racket/symbol" construct-racket-symbol)))

(define (make-checked-string->number node)
  (λ (s [radix 10])
    (define n (string->number s radix))
    (if (number? n)
        n
        (constructor-error
         #f (format "expected a number, but found ~a" s)
         (node-start node)))))

(module+ test
  (require rackunit racket/sandbox)
  (define racket-eval
    (parameterize
        ([sandbox-namespace-specs
          (append (sandbox-namespace-specs)
                  '(racket/set))])
      (make-evaluator 'racket)))
  (for ([(test-file check-file) (test-files #".construct")])
    (test-case (path->string check-file)
      (check-equal?
       (construct-file test-file)
       (racket-eval (file->string check-file)))))

  (test-case "construct-yaml-int"
    (check-exn
     #rx"expected a number"
     (λ () (construct-string "!!int foo"))))
  
  (test-case "construct-yaml-bool"
    (check-exn
     #rx"expected a boolean"
     (λ () (construct-string "!!bool foo"))))

  (test-case "construct-yaml-omap/pairs"
    (check-exn
     #rx"expected a sequence"
     (λ () (construct-string "!!omap foo")))
    (check-exn
     #rx"expected a mapping of length 1"
     (λ () (construct-string "!!omap [foo]")))
    (check-exn
     #rx"expected a single mapping item"
     (λ () (construct-string "!!omap [{a: b, c: d}]")))
    (check-exn
     #rx"found a duplicate key"
     (λ () (construct-string "!!omap [a: b, a: c]"))))

  (test-case "construct-yaml-pair"
    (check-exn
     #rx"expected 2 values, but found 1"
     (λ () (construct-string "!!racket/pair [foo]")))
    (check-exn
     #rx"expected 2 values, but found 3"
     (λ () (construct-string "!!racket/pair [foo, bar, baz]"))))

  (test-case "construct-yaml-str"
    (check-exn
     #rx"expected a scalar node"
     (λ () (construct-string "!!str [foo]"))))

  (test-case "construct-yaml-seq"
    (check-exn
     #rx"expected a sequence node"
     (λ () (construct-string "!!seq foo"))))

  (test-case "construct-yaml-map"
    (check-exn
     #rx"expected a mapping node"
     (λ () (construct-string "!!map foo")))
    (check-exn
     #rx"expected a mapping or list of mappings for merging"
     (λ () (construct-string "<< : foo")))
    (check-exn
     #rx"expected a mapping for merging"
     (λ () (construct-string "<< : [foo]")))
    (check-exn
     #rx"expected a mapping for merging"
     (λ () (construct-string "<< : [[foo]]"))))

  (test-case "construct-undefined"
    (check-exn
     #rx"could not determine a constructor for the tag"
     (λ () (construct-string "!foo bar" #:allow-undefined? #f)))
    (check-equal?
     (construct-string "!foo bar" #:allow-undefined? #t)
     '("bar"))
    (check-equal?
     (construct-string "!foo [bar]" #:allow-undefined? #t)
     '(("bar")))
    (check-equal?
     (construct-string "!foo {bar: baz}" #:allow-undefined? #t)
     `(,(make-hash '(("bar" . "baz")))))))
