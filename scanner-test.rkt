;;;;;; scanner-test.rkt - RackUnit YAML scanner test.    -*- Mode: Racket -*-

#lang racket

(require rackunit "scanner.rkt" "tokens.rkt")

(define-simple-check (check-scanner test-file check-file)
  (for ([token (scan-file test-file)]
        [line (read-file check-file)])
    (check-equal? (token->string token) line)))

(define (read-file filename)
  (with-input-from-file filename
    (λ ()
      (let loop ([lines '()])
        (let ([ln (read-line)])
          (if (string? ln)
              (loop (cons ln lines))
              (reverse lines)))))))

(define (test-files [dir "test"] [EXT #"scan"])
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
   (check-scanner test-file check-file)))
