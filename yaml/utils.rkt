;;;;;; utils.rkt - Utilities.    -*- Mode: Racket -*-

#lang racket

(require racket/runtime-path)

;; TODO: contracts
(provide (all-defined-out))

(define-syntax-rule (append! dst lst ...)
  (set! dst (append dst lst ...)))

(define-syntax-rule (pop! lst)
  (begin0 (last lst)
    (set! lst (drop-right lst 1))))

(define-syntax-rule (while test body ...)
  (let loop () (when test body ... (loop))))

(define-runtime-path yaml-directory ".")

(define (test-files extension [directory "test"])
  (define test-extension #".yaml")
  (define directory-path (build-path yaml-directory directory))
  (make-hash
   (map
    (λ (p)
      (define file (string->path (format "~a/~a" directory-path p)))
      (cons (path-replace-extension file test-extension) file))
    (filter
     (λ (p)
       (define file (string->path (format "~a/~a" directory-path p)))
       (define file-extension (path-get-extension file))
       ;; extension : (U String Bytes)
       ;; file-extension : (Option Bytes)
       (and (equal? (format "~a" extension) (format "~a" file-extension))
            (file-exists? (path-replace-extension file test-extension))))
     (directory-list directory-path)))))
