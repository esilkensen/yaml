;;;;;; struct.rkt - YAML struct macro.    -*- Mode: Racket -*-

#lang typed/racket

(provide yaml-struct:)

(define-for-syntax (build-name id . parts)
  (let ([str (apply string-append
                    (map (λ (p)
                           (if (syntax? p)
                               (symbol->string (syntax-e p))
                               (format "~a" p)))
                         parts))])
    (datum->syntax id (string->symbol str) id)))

(define-syntax (yaml-struct: stx)
  (syntax-case stx (:)
    [(_ name ([field : type] ...) options ...)
     (with-syntax ([s: (build-name #'name #'name ":")]
                   [s-strings (build-name #'name #'name "-strings")]
                   [s->string (build-name #'name #'name "->string")]
                   [print-s (build-name #'name "print-" #'name)])
       #`(begin
           (struct: name ([field : type] ...) options ...)
           (: s-strings (HashTable (Any -> Boolean) (name -> String)))
           (define s-strings (make-hash))
           (: s->string (name -> String))
           (define (s->string name)
             (let loop ([ss (hash-keys s-strings)])
               (if (null? ss)
                   (error 's->string "unexpected ~a type" 'name)
                   (if ((car ss) name)
                       ((hash-ref s-strings (car ss)) name)
                       (loop (cdr ss))))))
           (: print-s (case-> (name -> Void) (name Output-Port -> Void)))
           (define (print-s name [out (current-output-port)])
             (fprintf out "~a\n" (s->string name)))
           (define-syntax (s: stx)
             (syntax-case stx (:)
               [(_ in ([in-field : in-type] (... ...)) in-options (... ...))
                (with-syntax
                    ([t-s (build-name #'in #'in "-" #'name)]
                     [t-s? (build-name #'in #'in "-" #'name "?")]
                     [t-s->string (build-name #'in #'in "-" #'name "->string")])
                  (let ([fs (map
                             (λ (f)
                               `(cons ,(format "~a" (syntax->datum f))
                                      ,(build-name #'in #'in "-" #'name "-" f)))
                             (sort (syntax->list #'(in-field (... ...)))
                                   (λ (s t)
                                     (string<?
                                      (format "~a" (syntax->datum s))
                                      (format "~a" (syntax->datum t))))))])
                    #`(begin
                        (struct: t-s name ([in-field : in-type] (... ...))
                                 in-options (... ...))
                        (: t-s->string (name -> String))
                        (define (t-s->string t)
                          (if (t-s? t)
                              (let* ([attr->string
                                      (λ: ([p : (Pairof String (t-s -> Any))])
                                        (format "~a=~s" (car p) ((cdr p) t)))]
                                     [fields (map attr->string (list #,@fs))])
                                (format "~a(~a)" 't-s (string-join fields ", ")))
                              (let ([msg "unexpected ~a type"])
                                (error 't-s->string msg 'name))))
                        (hash-set! s-strings t-s? t-s->string))))]
               [(_ in in-options (... ...))
                #`(s: in () in-options (... ...))]))))]))
