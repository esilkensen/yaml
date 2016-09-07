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
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
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
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
    . ->* . void?)]
  [yaml->string
   ((yaml?)
    (#:canonical boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start boolean?
     #:explicit-end boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
    . ->* . string?)]
  [yaml*->string
   (((listof yaml?))
    (#:canonical boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start boolean?
     #:explicit-end boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
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
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
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
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
    . ->* . string?)])
 
 (except-out
  (all-from-out "yaml-expr.rkt")
  gen->yaml
  yaml-struct-constructors))

(define (read-yaml [in (current-input-port)])
  (construct in))

(define (read-yaml* [in (current-input-port)])
  (construct-all in))

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
                    #:style [style 'best]
                    #:sort-mapping [mapping-less-than? #f]
                    #:sort-mapping-key [mapping-extract-key identity])
  (write-yaml* (list document) out
               #:canonical canonical
               #:indent indent
               #:width width
               #:explicit-start explicit-start
               #:explicit-end explicit-end
               #:scalar-style scalar-style
               #:style style
               #:sort-mapping mapping-less-than?
               #:sort-mapping-key mapping-extract-key))

(define (write-yaml* documents [out (current-output-port)]
                     #:canonical [canonical #f]
                     #:indent [indent 2]
                     #:width [width 80]
                     #:explicit-start [explicit-start #f]
                     #:explicit-end [explicit-end #f]
                     #:scalar-style [scalar-style 'plain]
                     #:style [style 'best]
                     #:sort-mapping [mapping-less-than? #f]
                     #:sort-mapping-key [mapping-extract-key identity])
  (define-values (open close serialize)
    (make-serializer out
                     #:canonical canonical
                     #:indent indent
                     #:width width
                     #:explicit-start explicit-start
                     #:explicit-end explicit-end))
  (define represent
    (make-representer serialize
                      #:scalar-style scalar-style
                      #:style style
                      #:sort-mapping mapping-less-than?
                      #:sort-mapping-key mapping-extract-key))
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
                      #:style [style 'best]
                      #:sort-mapping [mapping-less-than? #f]
                      #:sort-mapping-key [mapping-extract-key identity])
  (with-output-to-string
    (λ () (write-yaml document
                      #:canonical canonical
                      #:indent indent
                      #:width width
                      #:explicit-start explicit-start
                      #:explicit-end explicit-end
                      #:scalar-style scalar-style
                      #:style style
                      #:sort-mapping mapping-less-than?
                      #:sort-mapping-key mapping-extract-key))))

(define (yaml*->string documents
                       #:canonical [canonical #f]
                       #:indent [indent 2]
                       #:width [width 80]
                       #:explicit-start [explicit-start #f]
                       #:explicit-end [explicit-end #f]
                       #:scalar-style [scalar-style 'plain]
                       #:style [style 'best]
                       #:sort-mapping [mapping-less-than? #f]
                       #:sort-mapping-key [mapping-extract-key identity])
  (with-output-to-string
    (λ () (write-yaml* documents
                       #:canonical canonical
                       #:indent indent
                       #:width width
                       #:explicit-start explicit-start
                       #:explicit-end explicit-end
                       #:scalar-style scalar-style
                       #:style style
                       #:sort-mapping mapping-less-than?
                       #:sort-mapping-key mapping-extract-key))))

(define (yaml->file document path
                    #:mode [mode-flag 'binary]
                    #:exists [exists-flag 'error]
                    #:canonical [canonical #f]
                    #:indent [indent 2]
                    #:width [width 80]
                    #:explicit-start [explicit-start #f]
                    #:explicit-end [explicit-end #f]
                    #:scalar-style [scalar-style 'plain]
                    #:style [style 'best]
                    #:sort-mapping [mapping-less-than? #f]
                    #:sort-mapping-key [mapping-extract-key identity])
  (with-output-to-file
    path
    (λ () (write-yaml document
                      #:canonical canonical
                      #:indent indent
                      #:width width
                      #:explicit-start explicit-start
                      #:explicit-end explicit-end
                      #:scalar-style scalar-style
                      #:style style
                      #:sort-mapping mapping-less-than?
                      #:sort-mapping-key mapping-extract-key))
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
                     #:style [style 'best]
                     #:sort-mapping [mapping-less-than? #f]
                     #:sort-mapping-key [mapping-extract-key identity])
  (with-output-to-file
    path
    (λ () (write-yaml* documents
                       #:canonical canonical
                       #:indent indent
                       #:width width
                       #:explicit-start explicit-start
                       #:explicit-end explicit-end
                       #:scalar-style scalar-style
                       #:style style
                       #:sort-mapping mapping-less-than?
                       #:sort-mapping-key mapping-extract-key))
    #:mode mode-flag
    #:exists exists-flag))

(module+ test
  (require rackunit "utils.rkt")
  
  (for ([(yaml-file check-file) (test-files #".construct")])
    (define yml-file (path-replace-extension yaml-file #".yml"))
    (test-case (path->string yaml-file)
      (define docs (file->yaml* yaml-file))
      (define single? (= 1 (length docs)))
      (define (docs->string docs)
        (if single?
            (yaml->string (first docs))
            (yaml*->string docs)))
      (define (string->docs str)
        (if single?
            (list (string->yaml str))
            (string->yaml* str)))
      (define (docs->file path)
        (if single?
            (yaml->file (first docs) path)
            (yaml*->file docs path)))
      (define (file->docs path)
        (if single?
            (list (file->yaml path))
            (file->yaml* path)))
      (check-equal? (string->docs (docs->string docs)) docs)
      (docs->file yml-file)
      (check-equal? (file->docs yml-file) docs)
      (delete-file yml-file)))

  (for ([style '(flow block)])
    (for ([(yaml-file check-file) (test-files (format ".~a" style))])
      (test-case (path->string check-file)
        (check-equal?
         (yaml*->string (file->yaml* yaml-file) #:style style)
         (file->string check-file)))))

  (for ([(yaml-file check-file) (test-files #".block-sort")])
    (test-case (path->string check-file)
      (check-equal?
       (yaml*->string
        (file->yaml* yaml-file)
        #:style 'block
        #:sort-mapping string<?
        #:sort-mapping-key car)
       (file->string check-file))))

  (test-case "yaml-struct"
    (yaml-struct player (name hr avg) #:transparent)
    (define p1 (player "Mark McGwire" 65 0.278))
    (check-equal? (string->yaml (yaml->string p1)) p1)

    (yaml-struct opaque-player (name hr avg))
    (define p2 (opaque-player "Sammy Sosa" 63 0.288))
    (define p3
      (string->yaml
       "!!struct:opaque-player {name: Sammy Sosa, hr: 63, avg: 0.288}"))
    (check-equal? (opaque-player-name p3) (opaque-player-name p2))
    (check-equal? (opaque-player-hr p3) (opaque-player-hr p2))
    (check-equal? (opaque-player-avg p3) (opaque-player-avg p2))
    (check-exn
     #rx"not a transparent struct"
     (λ () (yaml->string p2)))))
