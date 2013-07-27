;;;;;; constructor.rkt - YAML constructor.    -*- Mode: Racket -*-

#lang racket

(require
 racket/date
 net/base64
 (except-in srfi/13 string-replace)
 "composer.rkt"
 "errors.rkt"
 "nodes.rkt"
 "utils.rkt"
 "yaml.rkt")

(provide
 construct-file
 construct-string
 construct-all
 construct
 make-constructor)

(define constructor-error (make-error 'constructor))

(define (construct-file filename)
  (with-input-from-file filename construct-all))

(define (construct-string string)
  (with-input-from-string string construct-all))

(define (construct [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor in))
  (get-single-data))

(define (construct-all [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor in))
  (let loop ([data '()])
    (if (check-data?)
        (loop (cons (get-data) data))
        (reverse data))))

(define (make-constructor [in (current-input-port)])
  (define-values (check-node? get-node get-single-node)
    (make-composer in))
  
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
          (let ([tag (node-tag node)]
                [break #f])
            (cond
              [(hash-has-key? yaml-multi-constructors tag)
               (set! constructor (hash-ref yaml-multi-constructors tag))]
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
                    (set! constructor construct-mapping)]))]))
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
       #f (format "expected a scalar node, but found ~a"
                  (node->string-rec node))
       (node-start node)))
    (scalar-node-value node))
  
  (define (construct-sequence node [deep #f])
    (unless (sequence-node? node)
      (constructor-error
       #f (format "expected a sequence node, but found ~a"
                  (node->string-rec node))
       (node-start node)))
    (map
     (λ (child)
       (construct-object child deep))
     (sequence-node-value node)))
  
  (define (construct-mapping node [deep #f])
    (unless (mapping-node? node)
      (constructor-error
       #f (format "expected a mapping node, but found ~a"
                  (node->string-rec node))
       (node-start node)))
    (let ([mapping (make-hash)])
      (for ([kv (mapping-node-value node)])
        (match-let ([(cons key-node value-node) kv])
          (let* ([key (construct-object key-node deep)]
                 [value (construct-object value-node deep)])
            (hash-set! mapping key value))))
      mapping))
  
  (define (construct-pairs node [deep #f])
    (unless (mapping-node? node)
      (constructor-error
       #f (format "expected a mapping node, but found ~a"
                  (node->string-rec node))
       (node-start node)))
    (for/list ([kv (mapping-node-value node)])
      (match-let ([(cons key-node value-node) kv])
        (let* ([key (construct-object key-node deep)]
               [value (construct-object value-node deep)])
          (cons key value)))))
  
  (define (construct-yaml-null node)
    (construct-scalar node)
    (yaml-null))
  
  (define (construct-yaml-bool node)
    (let ([bool (string-downcase (construct-scalar node))])
      (cond
        [(member bool '("yes" "true" "on")) #t]
        [(member bool '("no" "false" "off")) #f]
        [else (constructor-error
               #f (format "expected a boolean, but got ~a" bool)
               (node-start node))])))
  
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
           (for ([digit (reverse (map string->number parts))])
             (set! float-value (* digit base))
             (set! base (* base 60)))
           (* sign float-value))]
        [else
         (* 1.0 sign (string->number value))])))
  
  (define (construct-yaml-binary node)
    (base64-decode
     (string->bytes/utf-8
      (format "~a" (construct-scalar node)))))
  
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
    (define values (regexp-match timestamp-regexp (scalar-node-value node)))
    (define year (string->number (list-ref values 1)))
    (define month (string->number (list-ref values 2)))
    (define day (string->number (list-ref values 3)))
    (define (get-hour)
      (and (list-ref values 4) (string->number (list-ref values 4))))
    (define (get-minute)
      (and (list-ref values 5) (string->number (list-ref values 5))))
    (define (get-second)
      (and (list-ref values 6) (string->number (list-ref values 6))))
    (define (get-fraction)
      (let ([value (list-ref values 7)])
        (and value (substring value 0 (min 6 (string-length value))))))
    (define (get-tz-sign)
      (and (list-ref values 9) (if (equal? "-" (list-ref values 9)) -1 +1)))
    (define (get-tz-hour)
      (and (list-ref values 10) (string->number (list-ref values 10))))
    (define (get-tz-minute)
      (and (list-ref values 11) (string->number (list-ref values 11))))
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
            (set! fraction (* 1000 (string->number fraction))))
          (if (get-tz-sign)
              (let* ([tz-hour (get-tz-hour)]
                     [tz-minute (or (get-tz-minute) 0)]
                     [tz-sign (get-tz-sign)]
                     [offset (* tz-sign
                                (+ (* tz-hour 60 60)
                                   (* tz-minute 60)))]
                     ;; subtract offset to get to UTC time without tz
                     [base (- (find-seconds
                               second minute hour day month year #f)
                              offset)]
                     ;; subtract offset again to count tz
                     [d0 (seconds->date (- base offset))])
                (date*
                 (date-second d0) (date-minute d0) (date-hour d0)
                 (date-day d0) (date-month d0) (date-year d0)
                 0 0 #f 0 fraction "UTC"))
              (date*
               second minute hour day month year 0 0 #f 0 fraction "UTC")))))
  
  (define (construct-yaml-omap node)
    (unless (sequence-node? node)
      (constructor-error
       "while constructing an ordered map"
       (format "expected a sequence, but found ~a"
               (node->string node))
       (node-start node)))
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
          (cons key value)))))
  
  (define (construct-yaml-pairs node)
    (construct-yaml-omap node))
  
  (define (construct-yaml-set node)
    (list->set (hash-keys (construct-mapping node))))
  
  (define (construct-yaml-str node)
    (construct-scalar node))
  
  (define (construct-yaml-seq node)
    (construct-sequence node))
  
  (define (construct-yaml-map node)
    (construct-mapping node))
  
  (define (construct-yaml-pair node)
    (let ([value (sequence-node-value node)])
      (cons (construct-object (first value))
            (construct-object (second value)))))
  
  (define (construct-undefined node)
    (constructor-error
     #f
     (format "could not determine a constructor for the tag ~a"
             (node-tag node))
     (node-start node)))
  
  (define (construct-yaml-struct id node)
    (unless (hash-has-key? yaml-struct-constructors id)
      (constructor-error
       #f
       (format "unrecognized struct ~a" id)
       (node-start node)))
    (match-let ([(cons make-struct (list (cons fields _) ...))
                 (hash-ref yaml-struct-constructors id)]
                [state (construct-mapping node)])
      (for ([n (mapping-node-value node)])
        (let ([arg-node (car n)]
              [arg (scalar-node-value (car n))])
          (unless (member arg fields)
            (constructor-error
             (format "unrecognized field for struct ~a" id)
             (format "  field: ~a" (pretty-format arg))
             (node-start arg-node)))))
      (for ([f fields])
        (unless (hash-has-key? state f)
          (constructor-error
           (format "missing expected field for struct ~a" id)
           (format "  field: ~a" (pretty-format f))
           (node-start node))))
      (apply make-struct (map (λ (f) (hash-ref state f)) fields))))
  
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
  (add-constructor! "tag:yaml.org,2002:pair" construct-yaml-pair)
  (add-constructor! #f construct-undefined)
  
  (add-multi-constructor! "tag:yaml.org,2002:struct:" construct-yaml-struct)
  
  (values check-data? get-data get-single-data))
