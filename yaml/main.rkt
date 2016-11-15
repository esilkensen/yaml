;;;;;; main.rkt - YAML library.    -*- Mode: Racket -*-

#lang racket

(require
 "constructor.rkt"
 "nodes.rkt"
 "representer.rkt"
 "serializer.rkt"
 "utils.rkt"
 (rename-in
  "yaml-expr.rkt"
  [yaml-constructor? yaml-single-constructor?]))

(provide
 node?
 scalar-node?
 sequence-node?
 mapping-node?
 yaml-constructor
 yaml-multi-constructor
 yaml-multi-constructor?
 yaml-representer
 yaml-representer?
 (recontract-out
  yaml?
  yaml-null?
  yaml-null
  yaml-constructors
  yaml-representers)
 (contract-out
  [yaml-constructor? (any/c . -> . boolean?)]
  [construct-scalar (scalar-node? . -> . string?)]
  [construct-sequence (sequence-node? . -> . (listof yaml?))]
  [construct-mapping (mapping-node? . -> . (hash/c yaml? yaml?))]
  [represent-scalar (string? string? . -> . scalar-node?)]
  [represent-sequence (string? (listof yaml?) . -> . sequence-node?)]
  [represent-mapping (string? (hash/c yaml? yaml?) . -> . mapping-node?)]
  [read-yaml
   (() (input-port? #:allow-undefined? boolean?) . ->* . yaml?)]
  [read-yaml*
   (() (input-port? #:allow-undefined? boolean?) . ->* . (listof yaml?))]
  [string->yaml
   ((string?) (#:allow-undefined? boolean?) . ->* . yaml?)]
  [string->yaml*
   ((string?) (#:allow-undefined? boolean?) . ->* . (listof yaml?))]
  [file->yaml
   ((path-string?)
    (#:allow-undefined? boolean?
     #:mode (or/c 'binary 'text))
    . ->* .
    yaml?)]
  [file->yaml*
   ((path-string?)
    (#:allow-undefined? boolean?
     #:mode (or/c 'binary 'text))
    . ->* .
    (listof yaml?))]
  [write-yaml
   ((yaml?)
    (output-port?
     #:canonical? boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start? boolean?
     #:explicit-end? boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
    . ->* . void?)]
  [write-yaml*
   (((listof yaml?))
    (output-port?
     #:canonical? boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start? boolean?
     #:explicit-end? boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
    . ->* . void?)]
  [yaml->string
   ((yaml?)
    (#:canonical? boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start? boolean?
     #:explicit-end? boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
    . ->* . string?)]
  [yaml*->string
   (((listof yaml?))
    (#:canonical? boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start? boolean?
     #:explicit-end? boolean?
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
     #:canonical? boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start? boolean?
     #:explicit-end? boolean?
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
     #:canonical? boolean?
     #:indent exact-positive-integer?
     #:width exact-positive-integer?
     #:explicit-start? boolean?
     #:explicit-end? boolean?
     #:scalar-style (or/c #\" #\' #\| #\> 'plain)
     #:style (or/c 'block 'flow 'best)
     #:sort-mapping (or/c (any/c any/c . -> . any/c) #f)
     #:sort-mapping-key (any/c . -> . any/c))
    . ->* . string?)]))

(define (yaml-constructor? v)
  (or (yaml-single-constructor? v)
      (yaml-multi-constructor? v)))

;; Reading YAML

(define current-constructor (make-parameter (new constructor%)))

(define (construct-scalar node)
  (send (current-constructor) construct-scalar node))

(define (construct-sequence node)
  (send (current-constructor) construct-sequence node))

(define (construct-mapping node)
  (send (current-constructor) construct-mapping node))

(define (read-yaml [in (current-input-port)]
                   #:allow-undefined? [allow-undefined? #f])
  (define constructor
    (new constructor% [in in] [allow-undefined? allow-undefined?]))
  (parameterize ([current-constructor constructor])
    (add-constructors! constructor (yaml-constructors))
    (send constructor get-single-data)))

(define (read-yaml* [in (current-input-port)]
                    #:allow-undefined? [allow-undefined? #f])
  (define constructor
    (new constructor% [in in] [allow-undefined? allow-undefined?]))
  (parameterize ([current-constructor constructor])
    (add-constructors! constructor (yaml-constructors))
    (let loop ([data '()])
      (if (send constructor check-data?)
          (loop (cons (send constructor get-data) data))
          (reverse data)))))

(define (add-constructors! constructor constructors)
  (for ([c constructors])
    (if (yaml-multi-constructor? c)
        (send constructor add-multi c)
        (send constructor add c))))

(define (string->yaml str #:allow-undefined? [allow-undefined? #f])
  (with-input-from-string str
    (thunk (read-yaml #:allow-undefined? allow-undefined?))))

(define (string->yaml* str #:allow-undefined? [allow-undefined? #f])
  (with-input-from-string str
    (thunk (read-yaml* #:allow-undefined? allow-undefined?))))

(define (file->yaml path
                    #:allow-undefined? [allow-undefined? #f]
                    #:mode [mode-flag 'binary])
  (with-input-from-file path
    (thunk (read-yaml #:allow-undefined? allow-undefined?))
    #:mode mode-flag))

(define (file->yaml* path
                     #:allow-undefined? [allow-undefined? #f]
                     #:mode [mode-flag 'binary])
  (with-input-from-file path
    (thunk (read-yaml* #:allow-undefined? allow-undefined?))
    #:mode mode-flag))

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
                    #:canonical? [canonical? #f]
                    #:indent [indent 2]
                    #:width [width 80]
                    #:explicit-start? [explicit-start? #f]
                    #:explicit-end? [explicit-end? #f]
                    #:scalar-style [scalar-style 'plain]
                    #:style [style 'best]
                    #:sort-mapping [mapping-less-than? #f]
                    #:sort-mapping-key [mapping-extract-key identity])
  (write-yaml* (list document) out
               #:canonical? canonical?
               #:indent indent
               #:width width
               #:explicit-start? explicit-start?
               #:explicit-end? explicit-end?
               #:scalar-style scalar-style
               #:style style
               #:sort-mapping mapping-less-than?
               #:sort-mapping-key mapping-extract-key))

(define (write-yaml* documents [out (current-output-port)]
                     #:canonical? [canonical? #f]
                     #:indent [indent 2]
                     #:width [width 80]
                     #:explicit-start? [explicit-start? #f]
                     #:explicit-end? [explicit-end? #f]
                     #:scalar-style [scalar-style 'plain]
                     #:style [style 'best]
                     #:sort-mapping [mapping-less-than? #f]
                     #:sort-mapping-key [mapping-extract-key identity])
  (define serializer
    (new serializer%
         [out out]
         [canonical? canonical?]
         [indent indent]
         [width width]
         [explicit-start? explicit-start?]
         [explicit-end? explicit-end?]))
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
                      #:canonical? [canonical? #f]
                      #:indent [indent 2]
                      #:width [width 80]
                      #:explicit-start? [explicit-start? #f]
                      #:explicit-end? [explicit-end? #f]
                      #:scalar-style [scalar-style 'plain]
                      #:style [style 'best]
                      #:sort-mapping [mapping-less-than? #f]
                      #:sort-mapping-key [mapping-extract-key identity])
  (with-output-to-string
    (thunk (write-yaml document
                       #:canonical? canonical?
                       #:indent indent
                       #:width width
                       #:explicit-start? explicit-start?
                       #:explicit-end? explicit-end?
                       #:scalar-style scalar-style
                       #:style style
                       #:sort-mapping mapping-less-than?
                       #:sort-mapping-key mapping-extract-key))))

(define (yaml*->string documents
                       #:canonical? [canonical? #f]
                       #:indent [indent 2]
                       #:width [width 80]
                       #:explicit-start? [explicit-start? #f]
                       #:explicit-end? [explicit-end? #f]
                       #:scalar-style [scalar-style 'plain]
                       #:style [style 'best]
                       #:sort-mapping [mapping-less-than? #f]
                       #:sort-mapping-key [mapping-extract-key identity])
  (with-output-to-string
    (thunk (write-yaml* documents
                        #:canonical? canonical?
                        #:indent indent
                        #:width width
                        #:explicit-start? explicit-start?
                        #:explicit-end? explicit-end?
                        #:scalar-style scalar-style
                        #:style style
                        #:sort-mapping mapping-less-than?
                        #:sort-mapping-key mapping-extract-key))))

(define (yaml->file document path
                    #:mode [mode-flag 'binary]
                    #:exists [exists-flag 'error]
                    #:canonical? [canonical? #f]
                    #:indent [indent 2]
                    #:width [width 80]
                    #:explicit-start? [explicit-start? #f]
                    #:explicit-end? [explicit-end? #f]
                    #:scalar-style [scalar-style 'plain]
                    #:style [style 'best]
                    #:sort-mapping [mapping-less-than? #f]
                    #:sort-mapping-key [mapping-extract-key identity])
  (with-output-to-file path
    (thunk (write-yaml document
                       #:canonical? canonical?
                       #:indent indent
                       #:width width
                       #:explicit-start? explicit-start?
                       #:explicit-end? explicit-end?
                       #:scalar-style scalar-style
                       #:style style
                       #:sort-mapping mapping-less-than?
                       #:sort-mapping-key mapping-extract-key))
    #:mode mode-flag
    #:exists exists-flag))

(define (yaml*->file documents path
                     #:mode [mode-flag 'binary]
                     #:exists [exists-flag 'error]
                     #:canonical? [canonical? #f]
                     #:indent [indent 2]
                     #:width [width 80]
                     #:explicit-start? [explicit-start? #f]
                     #:explicit-end? [explicit-end? #f]
                     #:scalar-style [scalar-style 'plain]
                     #:style [style 'best]
                     #:sort-mapping [mapping-less-than? #f]
                     #:sort-mapping-key [mapping-extract-key identity])
  (with-output-to-file path
    (thunk (write-yaml* documents
                        #:canonical? canonical?
                        #:indent indent
                        #:width width
                        #:explicit-start? explicit-start?
                        #:explicit-end? explicit-end?
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

  (test-case "posn"
    (struct posn (x y) #:transparent)
    (define (construct-posn node)
      (define mapping (construct-mapping node))
      (posn (hash-ref mapping "x") (hash-ref mapping "y")))
    (define posn-constructor
      (yaml-constructor posn? "!posn" construct-posn))
    
    (define (represent-posn p)
      (define mapping (make-hash))
      (hash-set! mapping "x" (posn-x p))
      (hash-set! mapping "y" (posn-y p))
      (represent-mapping "!posn" mapping))
    (define posn-representer (yaml-representer posn? represent-posn))
    
    (parameterize ([yaml-constructors (list posn-constructor)]
                   [yaml-representers (list posn-representer)])
      (check-equal?
       (yaml->string (posn 3 4)
                     #:style 'flow
                     #:sort-mapping string<?
                     #:sort-mapping-key car)
       "!posn {x: 3, y: 4}\n")
      (check-equal?
       (string->yaml "!posn {x: 3, y: 4}\n")
       (posn 3 4))))

  (test-case "multi-constructor"
    (struct player (name) #:transparent)
    (struct pitcher player (era) #:transparent)
    (define tag-prefix "!baseball:")
    (define (construct-player type node)
      (define mapping (construct-mapping node))
      (cond
        [(equal? "player" type)
         (player (hash-ref mapping "name"))]
        [(equal? "pitcher" type)
         (pitcher (hash-ref mapping "name") (hash-ref mapping "era"))]
        [else (error (format "unexpected type: ~a" type))]))
    (define player-constructor
      (yaml-multi-constructor player? tag-prefix construct-player))
    (define (represent-player p)
      (define type (if (pitcher? p) "pitcher" "player"))
      (define tag (string-append tag-prefix type))
      (define mapping (make-hash))
      (hash-set! mapping "name" (player-name p))
      (when (pitcher? p)
        (hash-set! mapping "era" (pitcher-era p)))
      (represent-mapping tag mapping))
    (define player-representer
      (yaml-representer player? represent-player))
    (parameterize ([yaml-constructors (list player-constructor)]
                   [yaml-representers (list player-representer)])
      (define p1 (player "Troy Tulowitzki"))
      (define p2 (pitcher "Jeff Francis" 3.45))
      (check-equal? (string->yaml (yaml->string p1)) p1)
      (check-equal? (string->yaml (yaml->string p2)) p2))))
