;;;;;; utils.rkt - Utilities.    -*- Mode: Racket -*-

#lang typed/racket

(provide (all-defined-out))

(struct: mark
  ([name : String] [index : Integer] [line : Integer]
   [column : Integer] [buffer : (Vectorof (U Char EOF))]))

(: make-error (Symbol -> ((Option String) String mark -> Void)))
(define (make-error type)
  (λ (context problem problem-mark)
    (error type "~a~a\n~a:~a:~a: ~a"
           (if (string? context)
               (format "~a;\n " context) "")
           problem
           (mark-name problem-mark)
           (mark-line problem-mark)
           (mark-column problem-mark)
           (vector-ref
            (mark-buffer problem-mark)
            (mark-index problem-mark)))))

(: read-file (String -> (Listof String)))
(define (read-file filename)
  (with-input-from-file filename
    (λ ()
      (let: loop : (Listof String)
            ([lines : (Listof String) '()])
        (let ([ln (read-line)])
          (if (string? ln)
              (loop (cons ln lines))
              (reverse lines)))))))

(: test-files
   (case->
    (Bytes -> (HashTable Path-String String))
    (Bytes String -> (HashTable Path-String String))))
(define (test-files extension [directory "test"])
  (: remove-extension ((U String Path) -> (U String Path)))
  (define (remove-extension file)
    (let*: ([fn : String (if (string? file) file (path->string file))]
            [ext : (Option Bytes) (filename-extension (string->path fn))]
            [str : String (if ext (substring fn 0 (- (string-length fn)
                                                     (add1 (bytes-length ext))))
                              fn)])
      (if (string? file) str (string->path str))))
  (make-hash
   (map
    (λ: ([p : Path])
      (let ([path (format "~a/~a" directory (path->string p))])
        (cons (remove-extension path) path)))
    (filter
     (λ: ([p : Path])
       (let ([path (string->path
                    (format "~a/~a" directory (path->string p)))])
         (and (equal? extension (filename-extension path))
              (file-exists? (remove-extension path)))))
     (directory-list directory)))))
