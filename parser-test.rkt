;;;;;; parser-test.rkt - RackUnit YAML parser test.    -*- Mode: Racket -*-

#lang racket

(require rackunit "parser.rkt" "events.rkt")

(define-simple-check (check-parser test-file check-file)
  (for ([event (parse-file test-file)]
        [line (read-file check-file)])
    (check-equal? (event->string event) line)))

(define (read-file filename)
  (with-input-from-file filename
    (λ ()
      (let loop ([lines '()])
        (let ([ln (read-line)])
          (if (string? ln)
              (loop (cons ln lines))
              (reverse lines)))))))

(define (test-files [dir "test"] [EXT #"parse"])
  (define (remove-extension file)
    (define fn (if (string? file) file (path->string file)))
    (let* ([ext (filename-extension (string->path fn))]
           [str (if ext (substring fn 0 (- (string-length fn)
                                           (+ 1 (bytes-length ext)))) fn)])
      (if (string? file) str (string->path str))))
  (make-hash
   (map
    (λ (p)
      (let ([path (format "~a/~a" dir (path->string p))])
        (cons (remove-extension path) path)))
    (filter
     (λ (p)
       (let ([path (string->path (format "~a/~a" dir (path->string p)))])
         (and (equal? EXT (filename-extension path))
              (file-exists? (remove-extension path)))))
     (directory-list dir)))))

(test-begin
 (for ([(test-file check-file) (test-files)])
   (check-parser test-file check-file)))
