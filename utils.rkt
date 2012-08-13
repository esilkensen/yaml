;;;;;; utils.rkt - Utilities.    -*- Mode: Racket -*-

#lang racket

(provide (all-defined-out))

(define-syntax-rule (append! dst lst ...)
  (set! dst (append dst lst ...)))

(define-syntax-rule (pop! lst)
  (begin0 (last lst)
    (set! lst (drop-right lst 1))))

(struct mark (name index line column buffer))

(define (read-file filename)
  (with-input-from-file filename
    (λ ()
      (let loop ([lines '()])
        (let ([ln (read-line)])
          (if (string? ln)
              (loop (cons ln lines))
              (reverse lines)))))))

(define (test-files extension [directory "test"])
  (define (remove-extension file)
    (define fn (if (string? file) file (path->string file)))
    (let* ([ext (filename-extension (string->path fn))]
           [str (if ext (substring fn 0 (- (string-length fn)
                                           (+ 1 (bytes-length ext)))) fn)])
      (if (string? file) str (string->path str))))
  (make-hash
   (map
    (λ (p)
      (let ([path (format "~a/~a" directory (path->string p))])
        (cons (remove-extension path) path)))
    (filter
     (λ (p)
       (let ([path (string->path
                    (format "~a/~a" directory (path->string p)))])
         (and (equal? extension (filename-extension path))
              (file-exists? (remove-extension path)))))
     (directory-list directory)))))
