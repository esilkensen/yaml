;;;;;; resolver.rkt - YAML resolver.    -*- Mode: Racket -*-

#lang racket

(require (planet dyoo/while-loop) "nodes.rkt" "utils.rkt")

(provide resolve add-implicit-resolver!)

(define DEFAULT-SCALAR-TAG "tag:yaml.org,2002:str")
(define DEFAULT-SEQUENCE-TAG "tag:yaml.org,2002:seq")
(define DEFAULT-MAPPING-TAG "tag:yaml.org,2002:map")
(define yaml-implicit-resolvers (make-hash))
(define yaml-path-resolvers (make-hash))
  
(define (resolve kind value implicit)
  (call/cc
   (λ (return)
     (when (and (eq? kind 'scalar)
                (car implicit))
       (let* ([key (if (equal? "" value) "" (string-ref value 0))]
              [resolvers (hash-ref yaml-implicit-resolvers key '())]
              [none (hash-ref yaml-implicit-resolvers #f '())])
         (for ([p (append resolvers none)])
           (match-let ([(cons tag regexp) p])
             (when (regexp-match regexp value)
               (return tag))))
         (set! implicit (cdr implicit))))
     (case kind
       [(scalar) DEFAULT-SCALAR-TAG]
       [(sequence) DEFAULT-SEQUENCE-TAG]
       [(mapping) DEFAULT-MAPPING-TAG]
       [else #f]))))

(define (add-implicit-resolver! tag regexp first)
  (let ([update (λ (v) (append v (list (cons tag regexp))))])
    (for ([ch (if (list? first) first '(#f))])
      (hash-update! yaml-implicit-resolvers ch update '()))))

(add-implicit-resolver!
 "tag:yaml.org,2002:bool"
 (regexp
  (string-append
   "^(?:yes|Yes|YES|no|No|NO"
   "|true|True|TRUE|false|False|FALSE"
   "|on|On|ON|off|Off|OFF)$"))
 (string->list "yYnNtTfFoO"))

(add-implicit-resolver!
 "tag:yaml.org,2002:float"
 (regexp
  (string-append
   "^(?:[-+]?(?:[0-9][0-9_]*)\\.[0-9_]*(?:[eE][-+][0-9]+)?"
   "|\\.[0-9_]+(?:[eE][-+][0-9]+)?"
   "|[-+]?[0-9][0-9_]*(?::[0-5]?[0-9])+\\.[0-9_]*"
   "|[-+]?\\.(?:inf|Inf|INF)"
   "|\\.(?:nan|NaN|NAN))$"))
 (string->list "-+0123456789."))


(add-implicit-resolver!
 "tag:yaml.org,2002:int"
 (regexp
  (string-append
   "^(?:[-+]?0b[0-1_]+"
   "|[-+]?0[0-7_]+"
   "|[-+]?(?:0|[1-9][0-9_]*)"
   "|[-+]?0x[0-9a-fA-F_]+"
   "|[-+]?[1-9][0-9_]*(?::[0-5]?[0-9])+)$"))
 (string->list "-+0123456789"))

(add-implicit-resolver!
 "tag:yaml.org,2002:merge"
 (regexp "^(?:<<)$")
 (string->list "<"))

(add-implicit-resolver!
 "tag:yaml.org,2002:null"
 (regexp "^(?:~|null|Null|NULL| )$")
 (append (string->list "~nN") (list "")))

(add-implicit-resolver!
 "tag:yaml.org,2002:timestamp"
 (regexp
  (string-append
   "^(?:[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
   "|[0-9][0-9][0-9][0-9] -[0-9][0-9]? -[0-9][0-9]?"
   "(?:[Tt]|[ \\t]+)[0-9][0-9]?"
   ":[0-9][0-9] :[0-9][0-9] (?:\\.[0-9]*)?"
   "(?:[ \\t]*(?:Z|[-+][0-9][0-9]?(?::[0-9][0-9])?))?)$"))
 (string->list "0123456789"))

(add-implicit-resolver!
 "tag:yaml.org,2002:value"
 (regexp "^(?:=)$")
 (string->list "="))

;; The following resolver is only for documentation purposes. It cannot
;; work because plain scalars cannot start with '!', '&', or '*'.
(add-implicit-resolver!
 "tag:yaml.org,2002:yaml"
 (regexp "^(?:!|&|\\*)$")
 (string->list "!&*"))


