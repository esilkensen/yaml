;;;;;; main.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "constructor.rkt"
 "representer.rkt"
 "serializer.rkt"
 "yaml-expr.rkt")

(provide
 (contract-out
  [read-yaml (() (input-port?) . ->* . yaml?)]
  [read-yaml* (() (input-port?) . ->* . (listof yaml?))]
  [string->yaml (string? . -> . yaml?)]
  [string->yaml* (string? . -> . (listof yaml?))]
  [file->yaml
   ((path-string?)
    (#:mode (or/c 'binary 'text))
    . ->* . yaml?)]
  [file->yaml*
   ((path-string?)
    (#:mode (or/c 'binary 'text))
    . ->* . (listof yaml?))]
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
    . ->* . string?)]
  [yaml->file
   ((yaml? path-string?)
    (#:mode (or/c 'binary 'text)
     #:exists (or/c 'error 'append 'update 'replace
                    'truncate 'truncate/replace)
     #:canonical boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start boolean?
     #:explicit-end boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best))
    . ->* . string?)]
  [yaml*->file
   (((listof yaml?) path-string?)
    (#:mode (or/c 'binary 'text)
     #:exists (or/c 'error 'append 'update 'replace
                    'truncate 'truncate/replace)
     #:canonical boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start boolean?
     #:explicit-end boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best))
    . ->* . string?)])
 
 (except-out
  (all-from-out "yaml-expr.rkt")
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

(define (file->yaml path #:mode [mode-flag 'binary])
  (with-input-from-file path read-yaml #:mode mode-flag))

(define (file->yaml* path #:mode [mode-flag 'binary])
  (with-input-from-file path read-yaml* #:mode mode-flag))

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

(define (yaml->file document path
                    #:mode [mode-flag 'binary]
                    #:exists [exists-flag 'error]
                    #:canonical [canonical #f]
                    #:indent [indent 2]
                    #:width [width 80]
                    #:explicit-start [explicit-start #f]
                    #:explicit-end [explicit-end #f]
                    #:scalar-style [scalar-style 'plain]
                    #:style [style 'best])
  (with-output-to-file
    path
    (λ () (write-yaml document
                      #:canonical canonical
                      #:indent indent
                      #:width width
                      #:explicit-start explicit-start
                      #:explicit-end explicit-end
                      #:scalar-style scalar-style
                      #:style style))
    #:mode mode-flag
    #:exists exists-flag))

(define (yaml*->file documents path
                     #:mode [mode-flag 'binary]
                     #:exists [exists-flag 'error]
                     #:canonical [canonical #f]
                     #:indent [indent 2]
                     #:width [width 80]
                     #:explicit-start [explicit-start #f]
                     #:explicit-end [explicit-end #f]
                     #:scalar-style [scalar-style 'plain]
                     #:style [style 'best])
  (with-output-to-file
    path
    (λ () (write-yaml* documents
                       #:canonical canonical
                       #:indent indent
                       #:width width
                       #:explicit-start explicit-start
                       #:explicit-end explicit-end
                       #:scalar-style scalar-style
                       #:style style))
    #:mode mode-flag
    #:exists exists-flag))

(module+ test
  (require rackunit "utils.rkt")
  (for ([(test-file check-file) (test-files #"write")])
    (test-case check-file
      (define docs (file->yaml* test-file))
      (define (docs->string docs)
        (if (= (length docs) 1)
            (yaml->string (first docs))
            (yaml*->string docs)))
      (define in (open-input-file check-file))
      (check-equal? (docs->string docs) (port->string in))
      (close-input-port in))))
