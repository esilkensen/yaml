;;;;;; main.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "private/constructor.rkt"
 "private/representer.rkt"
 "private/serializer.rkt"
 "private/yaml.rkt")

(provide
 (except-out
  (all-from-out "private/yaml.rkt")
  gen->yaml
  yaml-struct-constructors))

(provide
 (contract-out [read-yaml (->* () (any/c input-port?) yaml?)]))
(define (read-yaml [source-name 'input] [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor source-name in))
  (get-single-data))

(provide
 (contract-out [read-yaml* (->* () (any/c input-port?) (listof yaml?))]))
(define (read-yaml* [source-name 'input] [in (current-input-port)])
  (define-values (check-data? get-data get-single-data)
    (make-constructor source-name in))
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
         #:canonical boolean?
         #:indent exact-positive-integer?
         #:width exact-positive-integer?
         #:explicit-start boolean?
         #:explicit-end boolean?
         #:scalar (or/c #\" #\' #\| #\> 'plain)
         #:style (or/c 'block 'flow 'best))
        void?)]))
(define (write-yaml document [out (current-output-port)]
                    #:canonical [canonical #f]
                    #:indent [indent 2]
                    #:width [width 80]
                    #:explicit-start [explicit-start #f]
                    #:explicit-end [explicit-end #f]
                    #:scalar [scalar 'plain]
                    #:style [style 'best])
  (write-yaml* (list document) out
               #:canonical canonical
               #:indent indent
               #:width width
               #:explicit-start explicit-start
               #:explicit-end explicit-end
               #:scalar scalar
               #:style style))

(provide
 (contract-out
  [write-yaml*
   (->* ((listof yaml?))
        (output-port?
         #:canonical boolean?
         #:indent exact-positive-integer?
         #:width exact-positive-integer?
         #:explicit-start boolean?
         #:explicit-end boolean?
         #:scalar (or/c #\" #\' #\| #\> 'plain)
         #:style (or/c 'block 'flow 'best))
        void?)]))
(define (write-yaml* documents [out (current-output-port)]
                     #:canonical [canonical #f]
                     #:indent [indent 2]
                     #:width [width 80]
                     #:explicit-start [explicit-start #f]
                     #:explicit-end [explicit-end #f]
                     #:scalar [scalar 'plain]
                     #:style [style 'best])
  (define-values (open close serialize)
    (make-serializer out
                     #:canonical canonical
                     #:indent indent
                     #:width width
                     #:explicit-start explicit-start
                     #:explicit-end explicit-end))
  (define represent
    (let ([scalar (if (eq? 'plain scalar) #f scalar)]
          [style (if (eq? 'best style) style (eq? 'flow style))])
      (make-representer serialize #:scalar scalar #:style style)))
  (open)
  (for ([data documents])
    (represent data))
  (close))

(provide
 (contract-out
  [yaml->string
   (->* (yaml?)
        (#:canonical boolean?
         #:indent exact-positive-integer?
         #:width exact-positive-integer?
         #:explicit-start boolean?
         #:explicit-end boolean?
         #:scalar (or/c #\" #\' #\| #\> 'plain)
         #:style (or/c 'block 'flow 'best))
        string?)]))
(define (yaml->string document
                      #:canonical [canonical #f]
                      #:indent [indent 2]
                      #:width [width 80]
                      #:explicit-start [explicit-start #f]
                      #:explicit-end [explicit-end #f]
                      #:scalar [scalar 'plain]
                      #:style [style 'best])
  (with-output-to-string
    (λ () (write-yaml document
                      #:canonical canonical
                      #:indent indent
                      #:width width
                      #:explicit-start explicit-start
                      #:explicit-end explicit-end
                      #:scalar scalar
                      #:style style))))

(provide
 (contract-out
  [yaml*->string
   (->* ((listof yaml?))
        (#:canonical boolean?
         #:indent exact-positive-integer?
         #:width exact-positive-integer?
         #:explicit-start boolean?
         #:explicit-end boolean?
         #:scalar (or/c #\" #\' #\| #\> 'plain)
         #:style (or/c 'block 'flow 'best))
        string?)]))
(define (yaml*->string documents
                       #:canonical [canonical #f]
                       #:indent [indent 2]
                       #:width [width 80]
                       #:explicit-start [explicit-start #f]
                       #:explicit-end [explicit-end #f]
                       #:scalar [scalar 'plain]
                       #:style [style 'best])
  (with-output-to-string
    (λ () (write-yaml* documents
                       #:canonical canonical
                       #:indent indent
                       #:width width
                       #:explicit-start explicit-start
                       #:explicit-end explicit-end
                       #:scalar scalar
                       #:style style))))
