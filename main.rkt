;;;;;; main.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "private/constructor.rkt"
 "private/representer.rkt"
 "private/serializer.rkt"
 "private/yaml.rkt")

(provide
 (contract-out
  [read-yaml (() (input-port?) . ->* . yaml?)]
  [read-yaml* (() (input-port?) . ->* . (listof yaml?))]
  [string->yaml (string? . -> . yaml?)]
  [string->yaml* (string? . -> . (listof yaml?))]
  [write-yaml
   ((yaml?)
    (output-port?
     #:canonical boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start boolean?
     #:explicit-end boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best))
    . ->* . void?)]
  [write-yaml*
   (((listof yaml?))
    (output-port?
     #:canonical boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start boolean?
     #:explicit-end boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best))
    . ->* . void?)]
  [yaml->string
   ((yaml?)
    (#:canonical boolean?
                 #:indent exact-positive-integer?
                 #:width exact-positive-integer?
                 #:explicit-start boolean?
                 #:explicit-end boolean?
                 #:scalar-style (or/c #\" #\' #\| #\> 'plain)
                 #:style (or/c 'block 'flow 'best))
    . ->* . string?)]
  [yaml*->string
   (((listof yaml?))
    (#:canonical boolean?
                 #:indent exact-positive-integer?
                 #:width exact-positive-integer?
                 #:explicit-start boolean?
                 #:explicit-end boolean?
                 #:scalar-style (or/c #\" #\' #\| #\> 'plain)
                 #:style (or/c 'block 'flow 'best))
    . ->* . string?)])
 (except-out
  (all-from-out "private/yaml.rkt")
  gen->yaml
  yaml-struct-constructors))

(define (read-yaml [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor in))
  (get-single-data))

(define (read-yaml* [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor in))
  (let loop ([docs '()])
    (if (check-data?)
        (loop (cons (get-data) docs))
        (reverse docs))))

(define (string->yaml str)
  (with-input-from-string str read-yaml))

(define (string->yaml* str)
  (with-input-from-string str read-yaml*))

(define (write-yaml document [out (current-output-port)]
                    #:canonical [canonical #f]
                    #:indent [indent 2]
                    #:width [width 80]
                    #:explicit-start [explicit-start #f]
                    #:explicit-end [explicit-end #f]
                    #:scalar-style [scalar-style 'plain]
                    #:style [style 'best])
  (write-yaml* (list document) out
               #:canonical canonical
               #:indent indent
               #:width width
               #:explicit-start explicit-start
               #:explicit-end explicit-end
               #:scalar-style scalar-style
               #:style style))

(define (write-yaml* documents [out (current-output-port)]
                     #:canonical [canonical #f]
                     #:indent [indent 2]
                     #:width [width 80]
                     #:explicit-start [explicit-start #f]
                     #:explicit-end [explicit-end #f]
                     #:scalar-style [scalar-style 'plain]
                     #:style [style 'best])
  (define-values (open close serialize)
    (make-serializer out
                     #:canonical canonical
                     #:indent indent
                     #:width width
                     #:explicit-start explicit-start
                     #:explicit-end explicit-end))
  (define represent
    (let ([scalar-style (if (eq? 'plain scalar-style) #f scalar-style)]
          [style (if (eq? 'best style) style (eq? 'flow style))])
      (make-representer serialize #:scalar-style scalar-style #:style style)))
  (open)
  (for ([data documents])
    (represent data))
  (close))

(define (yaml->string document
                      #:canonical [canonical #f]
                      #:indent [indent 2]
                      #:width [width 80]
                      #:explicit-start [explicit-start #f]
                      #:explicit-end [explicit-end #f]
                      #:scalar-style [scalar-style 'plain]
                      #:style [style 'best])
  (with-output-to-string
   (λ () (write-yaml document
                     #:canonical canonical
                     #:indent indent
                     #:width width
                     #:explicit-start explicit-start
                     #:explicit-end explicit-end
                     #:scalar-style scalar-style
                     #:style style))))

(define (yaml*->string documents
                       #:canonical [canonical #f]
                       #:indent [indent 2]
                       #:width [width 80]
                       #:explicit-start [explicit-start #f]
                       #:explicit-end [explicit-end #f]
                       #:scalar-style [scalar-style 'plain]
                       #:style [style 'best])
  (with-output-to-string
   (λ () (write-yaml* documents
                      #:canonical canonical
                      #:indent indent
                      #:width width
                      #:explicit-start explicit-start
                      #:explicit-end explicit-end
                      #:scalar-style scalar-style
                      #:style style))))
