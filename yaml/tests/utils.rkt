#lang racket

(require
 "../scanner.rkt"
 "../tokens.rkt"
 "../parser.rkt"
 "../events.rkt"
 "../emitter.rkt"
 "../constructor.rkt"
 "../main.rkt")

(provide (all-defined-out))

(define (generate-test-files test-file)
  (with-output-to-file
    (path-replace-extension test-file ".scan")
    (thunk
     (for ([t (scan-file test-file)])
       (displayln (token->string t))))
    #:exists 'replace)
  (with-output-to-file
    (path-replace-extension test-file ".parse")
    (thunk
     (for ([e (parse-file test-file)])
       (displayln (event->string e))))
    #:exists 'replace)
  (with-output-to-file
    (path-replace-extension test-file ".emit")
    (thunk
     (define emitter (new emitter%))
     (for ([e (parse-file test-file)])
       (send emitter emit e)))
    #:exists 'replace)
  (with-output-to-file
    (path-replace-extension test-file ".block-sort")
    (thunk
     (write-yaml*
      (file->yaml* test-file)
      #:style 'block
      #:allow-unicode? #t
      #:sort-mapping string<?
      #:sort-mapping-key car))
    #:exists 'replace))
     
