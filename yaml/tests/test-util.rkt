#lang racket

(require
 "../scanner.rkt"
 "../tokens.rkt"
 "../parser.rkt"
 "../events.rkt"
 "../emitter.rkt"
 "../constructor.rkt")

(provide (all-defined-out))

(define (generate-test-files test-file)
  (with-output-to-file
    (path-replace-extension test-file ".scan")
    (thunk
     (for ([t (scan-file test-file)])
       (displayln (token->string t)))))
  (with-output-to-file
    (path-replace-extension test-file ".parse")
    (thunk
     (for ([e (parse-file test-file)])
       (displayln (event->string e)))))
  (with-output-to-file
    (path-replace-extension test-file ".emit")
    (thunk
     (define emit (make-emitter))
     (for ([e (parse-file test-file)])
       (emit e)))))
