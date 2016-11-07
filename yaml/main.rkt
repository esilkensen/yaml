;;;;;; main.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "constructor.rkt"
 "nodes.rkt"
 "representer.rkt"
 "serializer.rkt"
 "utils.rkt"
 "yaml-expr.rkt")

(provide
 node?
 yaml-constructor
 yaml-constructor?
 yaml-multi-constructor
 yaml-multi-constructor?
 yaml-representer
 yaml-representer?
 (recontract-out
  yaml?
  yaml-struct?
  yaml-null?
  yaml-null
  yaml-constructors
  yaml-representers)
 (contract-out
  [construct-scalar (node? . -> . string?)]
  [construct-sequence (node? . -> . (listof yaml?))]
  [construct-mapping (node? . -> . (hash/c yaml? yaml?))]
  [represent-scalar (string? string? . -> . node?)]
  [represent-sequence (string? list? . -> . node?)]
  [represent-mapping (string? hash? . -> . node?)]
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
    . ->* . string?)]))

;; Reading YAML

(define current-constructor (make-parameter (new constructor%)))

(define (construct-scalar node)
  (send (current-constructor) construct-scalar node))

(define (construct-sequence node)
  (send (current-constructor) construct-sequence node))

(define (construct-mapping node)
  (send (current-constructor) construct-mapping node))

(define (read-yaml [in (current-input-port)])
  (define constructor (new constructor% [in in]))
  (parameterize ([current-constructor constructor])
    (add-constructors! constructor (yaml-constructors))
    (send constructor get-single-data)))

(define (read-yaml* [in (current-input-port)])
  (define constructor (new constructor% [in in]))
  (parameterize ([current-constructor constructor])
    (add-constructors! constructor (yaml-constructors))
    (let loop ([data '()])
      (if (send constructor check-data?)
          (loop (cons (send constructor get-data) data))
          (reverse data)))))

(define (add-constructors! constructor constructors)
  (for ([c constructors])
    (if (yaml-constructor? c)
        (send constructor add c)
        (send constructor add-multi c))))

(define (string->yaml str)
  (with-input-from-string str read-yaml))

(define (string->yaml* str)
  (with-input-from-string str read-yaml*))

(define (file->yaml path #:mode [mode-flag 'binary])
  (with-input-from-file path read-yaml #:mode mode-flag))

(define (file->yaml* path #:mode [mode-flag 'binary])
  (with-input-from-file path read-yaml* #:mode mode-flag))

;; Writing YAML

(define current-representer
  (make-parameter (new representer% [serializer (new serializer%)])))

(define (represent-scalar tag value)
  (send (current-representer) represent-scalar tag value))

(define (represent-sequence tag sequence)
  (send (current-representer) represent-sequence tag sequence))

(define (represent-mapping tag mapping)
  (send (current-representer) represent-mapping tag mapping))

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
  (define serializer
    (new serializer%
         [out out]
         [canonical canonical]
         [indent indent]
         [width width]
         [explicit-start explicit-start]
         [explicit-end explicit-end]))
  (define representer
    (new representer%
         [serializer serializer]
         [scalar-style (if (eq? scalar-style 'plain) #f scalar-style)]
         [style style]
         [sort-mapping mapping-less-than?]
         [sort-mapping-key mapping-extract-key]))
  (parameterize ([current-representer representer])
    (for ([r (yaml-representers)])
      (send representer add r))
    (send serializer open)
    (for ([data documents])
      (send representer represent data))
    (send serializer close)))

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
  (require rackunit)

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
     (λ () (yaml->string p2))))

  (test-case "vectors"
    (define (construct-vector node)
      (list->vector (construct-sequence node)))
    (define vector-constructor
      (yaml-constructor vector? "!vector" construct-vector))
    
    (define (represent-vector vec)
      (represent-sequence "!vector" (vector->list vec)))
    (define vector-representer (yaml-representer vector? represent-vector))
    
    (parameterize ([yaml-constructors (list vector-constructor)]
                   [yaml-representers (list vector-representer)])
      (check-equal?
       (yaml->string #(1 (2) 3) #:style 'flow)
       "!vector [1, [2], 3]\n")
      (check-equal?
       (string->yaml "!vector [1, [2], 3]\n")
       #(1 (2) 3)))))
