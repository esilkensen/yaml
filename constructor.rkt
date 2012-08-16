;;;;;; constructor.rkt - YAML constructor.    -*- Mode: Racket -*-

#lang racket

(require
 net/base64
 srfi/13
 (planet dyoo/while-loop)
 "composer.rkt"
 "nodes.rkt"
 "utils.rkt")

(define constructor-error (make-error 'constructor))

(define (make-constructor [in (current-input-port)] #:name [name "<input>"])
  (define-values (check-node? get-node get-single-node)
    (make-composer in #:name name))
  
  (define yaml-constructors (make-hash))
  (define yaml-multi-constructors (make-hash))
  (define constructed-objects (make-hash))
  (define recursive-objects (make-hash))
  (define deep-construct #f)

  (define (check-data?)
    (check-node?))
  
  (define (get-data)
    (when (check-node?)
      (construct-document (get-node))))

  (define (get-single-data)
    (let ([node (get-single-node)])
      (and node (construct-document node))))

  (define (construct-document node)
    (let ([data (construct-object node)])
      (set! constructed-objects (make-hash))
      (set! recursive-objects (make-hash))
      (set! deep-construct #f)
      data))

  (define (construct-object node [deep #f])
    (if (hash-has-key? constructed-objects node)
        (hash-ref constructed-objects node)
        (let ([old-deep #f]
              [constructor #f]
              [tag-suffix #f])
          (when deep
            (set! old-deep deep-construct)
            (set! deep-construct #t))
          (when (hash-has-key? recursive-objects node)
            (constructor-error
             #f "found unconstructable recursive node"
             (node-start node)))
          (hash-set! recursive-objects node #f)
          (cond
           [(hash-has-key? yaml-constructors (node-tag node))
            (set! constructor (hash-ref yaml-constructors (node-tag node)))]
           [else
            (let ([break #f] [tag (node-tag node)])
              (for ([(tag-prefix mc) yaml-multi-constructors])
                (unless break
                  (when (string-prefix? tag-prefix tag)
                    (let ([n (string-length tag-prefix)])
                      (set! tag-suffix (substring tag n))
                      (set! constructor mc)
                      (set! break #t)))))
              (unless break
                (cond
                 [(hash-has-key? yaml-multi-constructors #f)
                  (set! tag-suffix tag)
                  (set! constructor (hash-ref yaml-multi-constructors #f))]
                 [(hash-has-key? yaml-constructors #f)
                  (set! constructor (hash-ref yaml-constructors #f))]
                 [(scalar-node? node)
                  (set! constructor construct-scalar)]
                 [(sequence-node? node)
                  (set! constructor construct-sequence)]
                 [(mapping-node? node)
                  (set! constructor construct-mapping)])))])
          (let ([data (if (not tag-suffix)
                          (constructor node)
                          (constructor tag-suffix node))])
            (hash-set! constructed-objects node data)
            (hash-remove! recursive-objects node)
            (when deep
              (set! deep-construct old-deep))
            data))))

  (define (construct-scalar node)
    (unless (scalar-node? node)
      (constructor-error
       #f (format "expected a scalar node, but found ~a" (node-type node))
       (node-start node)))
    (node-value node))

  (define (construct-sequence node [deep #f])
    (unless (sequence-node? node)
      (constructor-error
       #f (format "expected a sequence node, but found ~a" (node-type node))
       (node-start node)))
    (map
     (λ (child)
       (construct-object child deep))
     (node-value node)))

  (define (construct-mapping node [deep #f])
    (unless (mapping-node? node)
      (constructor-error
       #f (format "expected a mapping node, but found ~a" (node-type node))
       (node-start node)))
    (let ([mapping (make-hash)])
      (for ([key-value (node-value node)])
        (match-let ([(cons key-node value-node) key-value])
          (let* ([key (construct-object key-node deep)]
                 [value (construct-object value-node deep)])
            (hash-set! mapping key value))))
      mapping))

  (define (construct-pairs node [deep #f])
    (unless (mapping-node? node)
      (constructor-error
       #f (format "expected a mapping node, but found ~a" (node-type node))
       (node-start node)))
    (for/list ([key-value (node-value node)])
      (match-let ([(cons key-node value-node) key-value])
        (let* ([key (construct-object key-node deep)]
               [value (construct-object value-node deep)])
          (cons key value)))))

  (define (construct-yaml-null node)
    (construct-scalar node)
    '())

  (define (construct-yaml-bool node)
    (case (string-downcase (construct-scalar node))
      [("yes" "true" "on") #t]
      [("no" "false" "off") #f]
      [else (constructor-error
             #f "expected a boolean, but didn't get it!"
             (node-start node))]))

  (define (construct-yaml-int node)
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
        (* sign (string->number (substring value 2) 2))]
       [(string-prefix? "0x" value)
        (* sign (string->number (substring value 2) 16))]
       [(char=? #\0 (string-ref value 0))
        (* sign (string->number value) 8)]
       [(string-index value #\:)
        (let ([base 1]
              [int-value 0]
              [parts (string-split value ":")])
          (for ([digit (reverse (map string->number parts))])
            (set! int-value (+ int-value (* digit base)))
            (set! base (* base 60)))
          (* sign int-value))]
       [else
        (* sign (string->number value))])))

  (define (construct-yaml-float node)
    (let ([value (string-replace
                  (string-downcase
                   (format "~a" (construct-scalar node)) "_" ""))]
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
          (for ([digit (reverse (map string->number parts))])
            (set! float-value (* digit base))
            (set! base (* base 60)))
          (* sign float-value))]
       [else
        (* 1.0 sign (string->number value))])))

  (define (construct-yaml-binary node)
    (base64-decode
     (string->bytes/utf-8
      (format "~a" (construct-scalar)))))

  (define (construct-yaml-timestamp node)
    ;; TODO
    #f)

  (define (construct-yaml-omap node)
    ;; TODO
    #f)

  (define (construct-yaml-pairs node)
    ;; TODO
    #f)

  (define (construct-yaml-set node)
    ;; TODO
    #f)

  (define (construct-yaml-str node)
    ;; TODO
    #f)

  (define (construct-yaml-seq node)
    ;; TODO
    #f)

  (define (construct-yaml-map node)
    ;; TODO
    #f)

  (define (construct-undefined node)
    ;; TODO
    #f)

  (define (add-constructor! tag constructor)
    (hash-set! yaml-constructors tag constructor))
  
  (define (add-multi-constructor! tag-prefix multi-constructor)
    (hash-set! yaml-multi-constructors tag-prefix multi-constructor))
  
  (add-constructor! "tag:yaml.org,2002:null" construct-yaml-null)
  (add-constructor! "tag:yaml.org,2002:bool" construct-yaml-bool)
  (add-constructor! "tag:yaml.org,2002:int" construct-yaml-int)
  (add-constructor! "tag:yaml.org,2002:float" construct-yaml-float)
  (add-constructor! "tag:yaml.org,2002:binary" construct-yaml-binary)
  (add-constructor! "tag:yaml.org,2002:timestamp" construct-yaml-timestamp)
  (add-constructor! "tag:yaml.org,2002:omap" construct-yaml-omap)
  (add-constructor! "tag:yaml.org,2002:pairs" construct-yaml-pairs)
  (add-constructor! "tag:yaml.org,2002:set" construct-yaml-set)
  (add-constructor! "tag:yaml.org,2002:str" construct-yaml-str)
  (add-constructor! "tag:yaml.org,2002:seq" construct-yaml-seq)
  (add-constructor! "tag:yaml.org,2002:map" construct-yaml-map)
  (add-constructor! #f construct-undefined)

  (values check-data? get-data get-single-data))
