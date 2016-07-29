;;;;;; utils.rkt - Utilities.    -*- Mode: Racket -*-

#lang racket

(provide (all-defined-out))

(define-syntax-rule (append! dst lst ...)
  (set! dst (append dst lst ...)))

(define-syntax-rule (pop! lst)
  (begin0 (last lst)
    (set! lst (drop-right lst 1))))

(define-syntax-rule (while test body ...)
  (let loop () (when test body ... (loop))))

(define (test-files extension [directory "test"])
  (define (remove-extension file)
    (let* ([fn (if (string? file) file (path->string file))]
           [ext (filename-extension (string->path fn))]
           [str (if ext (substring fn 0 (- (string-length fn)
                                           (add1 (bytes-length ext))))
                    fn)])
      (if (string? file) str (string->path str))))
  (define (set-extension file extension)
    (let* ([fn (if (string? file) file (path->string file))]
           [str (format "~a.~a" (remove-extension fn) extension)])
      (if (string? file) str (string->path str))))
  (define test-extension "yaml")
  (make-hash
   (map
    (λ (p)
      (let ([path (format "~a/~a" directory (path->string p))])
        (cons (set-extension path test-extension) path)))
    (filter
     (λ (p)
       (let ([path (string->path
                    (format "~a/~a" directory (path->string p)))])
         (and (equal? extension (filename-extension path))
              (file-exists? (set-extension path test-extension)))))
     (directory-list directory)))))
