;;;;;; parser.rkt - YAML parser.    -*- Mode: Racket -*-

#lang racket

(require
 racket/generator
 "errors.rkt"
 "events.rkt"
 "tokens.rkt"
 "scanner.rkt"
 "utils.rkt")

(provide
 parser+c%
 (contract-out
  [parse-file (path-string? . -> . (listof event?))]
  [parse-string (string? . -> . (listof event?))]
  [parse (() (input-port?) . ->* . (listof event?))]
  [parser% parser+c%]))

(define parser+c%
  (class/c
   (init-field
    [in input-port?])
   [check-event?
    (() #:rest (listof (any/c . -> . boolean?)) . ->*m . boolean?)]
   [peek-event (->m (or/c event? #f))]
   [get-event (->m (or/c event? #f))]))

(define (parse-file filename)
  (with-input-from-file filename parse))

(define (parse-string string)
  (with-input-from-string string parse))

(define (parse [in (current-input-port)])
  (define parser (new parser% [in in]))
  (let loop ([events '()])
    (if (event? (send parser peek-event))
        (loop (cons (send parser get-event) events))
        (reverse events))))

(define parser-error (make-error 'parser))

(define DEFAULT-TAGS #hash(("!" . "!") ("!!" . "tag:yaml.org,2002:")))

(define parser%
  (class object%
    (init-field
     [in (current-input-port)])
    
    (super-new)
    
    (define scanner (new scanner% [in in]))
    
    (define current-event #f)
    (define yaml-version #f)
    (define tag-handles (make-hash))
    (define states '())
    (define marks '())
    (define (state) (parse-stream-start))
    
    (define/public (check-event? . choices)
      ;; Check the type of the next event.
      (unless (event? current-event)
        (when (procedure? state)
          (set! current-event (state))))
      (and (event? current-event)
           (or (null? choices)
               (and (list? choices)
                    (ormap (λ (c?) (c? current-event))
                           choices)))))
    
    (define/public (peek-event)
      ;; Get the next event.
      (unless (event? current-event)
        (when (procedure? state)
          (set! current-event (state))))
      current-event)
    
    (define/public (get-event)
      ;; Get the next event and proceed further.
      (unless (event? current-event)
        (when (procedure? state)
          (set! current-event (state))))
      (begin0 current-event
        (set! current-event #f)))
    
    ;; stream ::= STREAM-START implicit_document? explicit_document* STREAM-END
    
    (define (parse-stream-start)
      (let ([token (send scanner get-token)])
        (begin0 (stream-start-event (token-start token) (token-end token))
          (set! state parse-implicit-document-start))))
    
    ;; implicit_document ::= block_node DOCUMENT-END*
    ;; explicit_document ::= DIRECTIVE* DOCUMENT-START block_node? DOCUMENT-END*
    
    (define (parse-implicit-document-start)
      (cond
        [(send scanner check-token?
               directive-token? document-start-token? stream-end-token?)
         (parse-document-start)]
        [else
         (set! tag-handles DEFAULT-TAGS)
         (let ([mark (token-start (send scanner peek-token))])
           (begin0 (document-start-event mark mark #f #f #f)
             (append! states (list parse-document-end))
             (set! state parse-block-node)))]))
    
    (define (parse-document-start)
      (while (send scanner check-token? document-end-token?)
        (send scanner get-token))
      (cond
        [(send scanner check-token? stream-end-token?)
         (let ([token (send scanner get-token)])
           (begin0 (stream-end-event (token-start token) (token-end token))
             (unless (and (null? states) (null? marks))
               (error 'parser "assertion error (non-null ~a)"
                      (if (null? states) 'states 'marks)))
             (set! state #f)))]
        [else
         (let* ([token (send scanner peek-token)]
                [start (token-start token)])
           (match-let ([(cons version tags) (process-directives)])
             (unless (send scanner check-token? document-start-token?)
               (parser-error
                #f
                (format "expected '<document start>', but found ~a"
                        (token->string (send scanner peek-token)))
                (token-start (send scanner peek-token))))
             (let ([end (token-end (send scanner get-token))])
               (begin0 (document-start-event start end #t version tags)
                 (append! states (list parse-document-end))
                 (set! state parse-document-content)))))]))
    
    (define (parse-document-end)
      (let ([start (token-start (send scanner peek-token))]
            [end (token-start (send scanner peek-token))]
            [explicit #f])
        (when (send scanner check-token? document-end-token?)
          (set! end (token-end (send scanner get-token)))
          (set! explicit #t))
        (begin0 (document-end-event start end explicit)
          (set! state parse-document-start))))
    
    (define (parse-document-content)
      (if (send scanner check-token?
                directive-token?
                document-start-token?
                document-end-token?
                stream-end-token?)
          (begin0 (process-empty-scalar (token-start (send scanner peek-token)))
            (set! state (pop! states)))
          (parse-block-node)))
    
    (define (process-directives)
      (set! yaml-version #f)
      (set! tag-handles (make-hash))
      (let ([value #f])
        (while (send scanner check-token? directive-token?)
          (let ([token (send scanner get-token)])
            (cond
              [(string=? "YAML" (directive-token-name token))
               (let ([start (token-start token)])
                 (when yaml-version
                   (parser-error #f "found duplicate YAML directive" start))
                 (match-let ([(cons major minor)
                              (directive-token-value token)])
                   (unless (= 1 major)
                     (parser-error
                      #f "found incompatible YAML document" start))
                   (set! yaml-version (directive-token-value token))))]
              [(string=? "TAG" (directive-token-name token))
               (match-let ([(cons handle prefix)
                            (directive-token-value token)])
                 (when (hash-has-key? tag-handles handle)
                   (let ([msg (format "found duplicate tag handle ~a" handle)])
                     (parser-error #f msg (token-start token))))
                 (hash-set! tag-handles handle prefix))]
              [else
               (let ([name (directive-token-name token)])
                 (log-warning "Ignoring unknown directive: ~a" name))])))
        (if (null? (hash-keys tag-handles))
            (set! value (cons yaml-version #f))
            (set! value (cons yaml-version (hash-copy tag-handles))))
        (for ([(key tag) DEFAULT-TAGS])
          (unless (hash-has-key? tag-handles key)
            (hash-set! tag-handles key tag)))
        value))
    
    ;; block_node_or_indentless_sequence ::= ALIAS
    ;;   | properties (block_content | indentless_block_sequence)?
    ;;   | block_content | indentless_block_sequence
    ;; block_node ::= ALIAS | properties block_content? | block_content
    ;; flow_node ::= ALIAS | properties flow_content? | flow_content
    ;; properties ::= TAG ANCHOR? | ANCHOR TAG?
    ;; block_content ::= block_collection | flow_collection | SCALAR
    ;; flow_content ::= flow_collection | SCALAR
    ;; block_collection ::= block_sequence | block_mapping
    ;; flow_collection ::= flow_sequence | flow_mapping
    
    (define (parse-block-node) (parse-node #t #f))
    
    (define (parse-flow-node) (parse-node #f #f))
    
    (define (parse-block-node-or-indentless-sequence) (parse-node #t #t))
    
    (define (parse-node block indentless-sequence)
      (cond
        [(send scanner check-token? alias-token?)
         (let ([token (send scanner get-token)])
           (begin0 (alias-event
                    (token-start token)
                    (token-end token)
                    (alias-token-value token))
             (set! state (pop! states))))]
        [else
         (let ([anchor #f] [tag #f] [start #f] [end #f] [tag-mark #f])
           (cond
             [(send scanner check-token? anchor-token?)
              (let ([token (send scanner get-token)])
                (set! start (token-start token))
                (set! end (token-end token))
                (set! anchor (anchor-token-value token))
                (when (send scanner check-token? tag-token?)
                  (let ([token (send scanner get-token)])
                    (set! tag-mark (token-start token))
                    (set! end (token-end token))
                    (set! tag (tag-token-value token)))))]
             [(send scanner check-token? tag-token?)
              (let ([token (send scanner get-token)])
                (set! start (token-start token))
                (set! tag-mark (token-start token))
                (set! tag (tag-token-value token))
                (when (send scanner check-token? anchor-token?)
                  (let ([token (send scanner get-token)])
                    (set! end (token-end token))
                    (set! anchor (anchor-token-value token)))))])
           (match tag
             [(cons handle suffix)
              (if handle
                  (if (hash-has-key? tag-handles handle)
                      (let ([h (hash-ref tag-handles handle)])
                        (set! tag (format "~a~a" h suffix)))
                      (parser-error
                       "while parsing a node"
                       (format "found undefined tag handle ~a"
                               (pretty-format handle))
                       tag-mark))
                  (set! tag suffix))]
             [else #f])
           (unless start
             (set! start (token-start (send scanner peek-token)))
             (set! end (token-start (send scanner peek-token))))
           (let ([implicit (or (not tag) (equal? #\! tag))])
             (if (and indentless-sequence
                      (send scanner check-token? block-entry-token?))
                 (begin0 (sequence-start-event start end anchor tag implicit #f)
                   (set! state parse-indentless-sequence-entry))
                 (cond
                   [(send scanner check-token? scalar-token?)
                    (let ([token (send scanner get-token)])
                      (begin0 (scalar-event
                               start
                               (token-end token)
                               anchor
                               tag
                               (cond [(or (and (scalar-token-plain token)
                                               (not tag))
                                          (equal? #\! tag))
                                      (cons #t #f)]
                                     [(not tag) (cons #f #t)]
                                     [else (cons #f #f)])
                               (scalar-token-value token)
                               (scalar-token-style token))
                        (set! state (pop! states))))]
                   [(send scanner check-token? flow-sequence-start-token?)
                    (begin0
                        (let ([end (token-end (send scanner peek-token))])
                          (sequence-start-event
                           start end anchor tag implicit #t))
                      (set! state parse-flow-sequence-first-entry))]
                   [(send scanner check-token? flow-mapping-start-token?)
                    (begin0
                        (let ([end (token-end (send scanner peek-token))])
                          (mapping-start-event
                           start end anchor tag implicit #t))
                      (set! state parse-flow-mapping-first-key))]
                   [(and block (send scanner check-token?
                                     block-sequence-start-token?))
                    (begin0
                        (let ([end (token-end (send scanner peek-token))])
                          (sequence-start-event
                           start end anchor tag implicit #f))
                      (set! state parse-block-sequence-first-entry))]
                   [(and block (send scanner check-token?
                                     block-mapping-start-token?))
                    (begin0
                        (let ([end (token-end (send scanner peek-token))])
                          (mapping-start-event
                           start end anchor tag implicit #f))
                      (set! state parse-block-mapping-first-key))]
                   [(or anchor tag)
                    (begin0
                        (scalar-event
                         start end anchor tag (cons implicit #f) "" #f)
                      (set! state (pop! states)))]
                   [else
                    (let ([token (send scanner peek-token)])
                      (parser-error
                       (format "while parsing a ~a node"
                               (if block "block" "flow"))
                       (format "expected the node content, but found ~a"
                               (token->string (send scanner peek-token)))
                       (token-start (send scanner peek-token))))]))))]))
    
    ;; block_sequence ::=
    ;;   BLOCK-SEQUENCE-START (BLOCK-ENTRY block_node?)* BLOCK-END
    
    (define (parse-block-sequence-first-entry)
      (append! marks (list (token-start (send scanner get-token))))
      (parse-block-sequence-entry))
    
    (define (parse-block-sequence-entry)
      (cond
        [(send scanner check-token? block-entry-token?)
         (let ([token (send scanner get-token)])
           (cond
             [(send scanner check-token? block-entry-token? block-end-token?)
              (set! state parse-block-sequence-entry)
              (process-empty-scalar (token-end token))]
             [else
              (append! states (list parse-block-sequence-entry))
              (parse-block-node)]))]
        [else
         (unless (send scanner check-token? block-end-token?)
           (parser-error
            "while parsing a block collection"
            (format "expected <block end>, but found ~a"
                    (token->string (send scanner peek-token)))
            (token-start (send scanner peek-token))))
         (let ([token (send scanner get-token)])
           (begin0 (sequence-end-event (token-start token) (token-end token))
             (set! state (pop! states))
             (pop! marks)))]))
    
    ;; indentless_sequence ::= (BLOCK-ENTRY block_node?)+
    
    (define (parse-indentless-sequence-entry)
      (cond
        [(send scanner check-token? block-entry-token?)
         (let ([token (send scanner get-token)])
           (cond
             [(send scanner check-token?
                    block-entry-token?
                    key-token?
                    value-token?
                    block-end-token?)
              (set! state parse-indentless-sequence-entry)
              (process-empty-scalar (token-end token))]
             [else
              (append! states (list parse-indentless-sequence-entry))
              (parse-block-node)]))]
        [else
         (let ([token (send scanner peek-token)])
           (begin0 (sequence-end-event (token-start token) (token-end token))
             (set! state (pop! states))))]))
    
    ;; block_mapping ::=
    ;;   BLOCK-MAPPING_START ((KEY block_node_or_indentless_sequence?)?
    ;;     (VALUE block_node_or_indentless_sequence?)?)* BLOCK-END
    
    (define (parse-block-mapping-first-key)
      (append! marks (list (token-start (send scanner get-token))))
      (parse-block-mapping-key))
    
    (define (parse-block-mapping-key)
      (cond
        [(send scanner check-token? key-token?)
         (let ([token (send scanner get-token)])
           (cond
             [(send scanner check-token?
                    key-token?
                    value-token?
                    block-end-token?)
              (set! state parse-block-mapping-value)
              (process-empty-scalar (token-end token))]
             [else
              (append! states (list parse-block-mapping-value))
              (parse-block-node-or-indentless-sequence)]))]
        [else
         (unless (send scanner check-token? block-end-token?)
           (parser-error
            "while parsing a block mapping"
            (format "expected <block end>, but found ~a"
                    (token->string (send scanner peek-token)))
            (token-start (send scanner peek-token))))
         (let ([token (send scanner get-token)])
           (begin0 (mapping-end-event (token-start token) (token-end token))
             (set! state (pop! states))
             (pop! marks)))]))
    
    (define (parse-block-mapping-value)
      (cond
        [(send scanner check-token? value-token?)
         (let ([token (send scanner get-token)])
           (cond
             [(send scanner check-token?
                    key-token?
                    value-token?
                    block-end-token?)
              (set! state parse-block-mapping-key)
              (process-empty-scalar (token-end token))]
             [else
              (append! states (list parse-block-mapping-key))
              (parse-block-node-or-indentless-sequence)]))]
        [else
         (set! state parse-block-mapping-key)
         (process-empty-scalar (token-start (send scanner peek-token)))]))
    
    ;; flow_sequence ::=
    ;;   FLOW-SEQUENCE-START (flow_sequence_entry FLOW-ENTRY)*
    ;;   flow_sequence_entry? FLOW-SEQUENCE-END
    ;; flow_sequence_entry ::= flow_node | KEY flow_node? (VALUE flow_node?)?
    
    (define (parse-flow-sequence-first-entry)
      (append! marks (list (token-start (send scanner get-token))))
      (parse-flow-sequence-entry #t))
    
    (define (parse-flow-sequence-entry [first #f])
      (let ([flow-seq-end? (send scanner check-token?
                                 flow-sequence-end-token?)])
        (when (and (not flow-seq-end?) (not first))
          (if (send scanner check-token? flow-entry-token?)
              (send scanner get-token)
              (parser-error
               "while parsing a flow sequence"
               (format "expected ',' or ']', but got ~a"
                       (token->string (send scanner peek-token)))
               (token-start (send scanner peek-token)))))
        (cond
          [(and (not flow-seq-end?) (send scanner check-token? key-token?))
           (let ([start (token-start (send scanner peek-token))]
                 [end (token-end (send scanner peek-token))])
             (begin0 (mapping-start-event start end #f #f #t #t)
               (set! state parse-flow-sequence-entry-mapping-key)))]
          [(and (not flow-seq-end?)
                (not (send scanner check-token? flow-sequence-end-token?)))
           (append! states (list parse-flow-sequence-entry))
           (parse-flow-node)]
          [else
           (let ([token (send scanner get-token)])
             (begin0 (sequence-end-event (token-start token) (token-end token))
               (set! state (pop! states))
               (pop! marks)))])))
    
    (define (parse-flow-sequence-entry-mapping-key)
      (let ([token (send scanner get-token)])
        (cond
          [(send scanner check-token?
                 value-token?
                 flow-entry-token?
                 flow-sequence-end-token?)
           (set! state parse-flow-sequence-entry-mapping-value)
           (process-empty-scalar (token-end token))]
          [else
           (append! states (list parse-flow-sequence-entry-mapping-value))
           (parse-flow-node)])))
    
    (define (parse-flow-sequence-entry-mapping-value)
      (cond
        [(send scanner check-token? value-token?)
         (let ([token (send scanner get-token)])
           (cond
             [(send scanner check-token?
                    flow-entry-token?
                    flow-sequence-end-token?)
              (set! state parse-flow-sequence-entry-mapping-end)
              (process-empty-scalar (token-end token))]
             [else
              (append! states (list parse-flow-sequence-entry-mapping-end))
              (parse-flow-node)]))]
        [else
         (set! state parse-flow-sequence-entry-mapping-end)
         (process-empty-scalar (token-start (send scanner peek-token)))]))
    
    (define (parse-flow-sequence-entry-mapping-end)
      (let ([token (send scanner peek-token)])
        (set! state parse-flow-sequence-entry)
        (mapping-end-event (token-start token) (token-end token))))
    
    ;; flow_mapping ::= 
    ;;   FLOW-MAPPING-START(flow_mapping_entry FLOW-ENTRY)*
    ;;   flow_mapping_entry? FLOW-MAPPING-END
    ;; flow_mapping_entry ::= flow_node | KEY flow_node? (VALUE flow_node?)?
    
    (define (parse-flow-mapping-first-key)
      (append! marks (list (token-start (send scanner get-token))))
      (parse-flow-mapping-key #t))
    
    (define (parse-flow-mapping-key [first #f])
      (let ([flow-map-end? (send scanner check-token? flow-mapping-end-token?)])
        (when (and (not flow-map-end?) (not first))
          (if (send scanner check-token? flow-entry-token?)
              (send scanner get-token)
              (parser-error
               "while parsing a flow mapping"
               (format "expected ',' or '}', but got ~a"
                       (token->string (send scanner peek-token)))
               (token-start (send scanner peek-token)))))
        (cond
          [(and (not flow-map-end?) (send scanner check-token? key-token?))
           (let ([token (send scanner get-token)])
             (cond
               [(send scanner check-token?
                      value-token?
                      flow-entry-token?
                      flow-mapping-end-token?)
                (set! state parse-flow-mapping-value)
                (process-empty-scalar (token-end token))]
               [else
                (append! states (list parse-flow-mapping-value))
                (parse-flow-node)]))]
          [(and (not flow-map-end?) (not (send scanner check-token?
                                               key-token?
                                               flow-mapping-end-token?)))
           (append! states (list parse-flow-mapping-empty-value))
           (parse-flow-node)]
          [else
           (let ([token (send scanner get-token)])
             (begin0 (mapping-end-event (token-start token) (token-end token))
               (set! state (pop! states))
               (pop! marks)))])))
    
    (define (parse-flow-mapping-value)
      (cond
        [(send scanner check-token? value-token?)
         (let ([token (send scanner get-token)])
           (cond
             [(send scanner check-token?
                    flow-entry-token?
                    flow-mapping-end-token?)
              (set! state parse-flow-mapping-key)
              (process-empty-scalar (token-end token))]
             [else
              (append! states (list parse-flow-mapping-key))
              (parse-flow-node)]))]
        [else
         (set! state parse-flow-mapping-key)
         (process-empty-scalar (token-start (send scanner peek-token)))]))
    
    (define (parse-flow-mapping-empty-value)
      (set! state parse-flow-mapping-key)
      (process-empty-scalar (token-start (send scanner peek-token))))
    
    (define (process-empty-scalar mark)
      (scalar-event mark mark #f #f (cons #t #f) "" #f))))

(module+ test
  (require rackunit)
  
  (for ([(test-file check-file) (test-files #".parse")])
    (test-case (path->string check-file)
      (for ([event (parse-file test-file)]
            [line (file->lines check-file)])
        (check-equal? (event->string event) line))))

  (test-case "parse-flow-mapping-key"
    (check-exn
     #rx"expected ',' or '}'"
     (λ () (parse-string "{key: value"))))

  (test-case "parse-flow-sequence-entry"
    (check-exn
     #rx"expected ',' or ']'"
     (λ () (parse-string "[value"))))

  (test-case "parse-node"
    (check-exn
     #rx"found undefined tag handle"
     (λ () (parse-string "!foo!bar baz"))))

  (test-case "process-directives"
    (check-exn
     #rx"found incompatible YAML document"
     (λ () (parse-string "%YAML 2.0\n---\nfoo")))
    (check-exn
     #rx"found duplicate YAML directive"
     (λ () (parse-string "%YAML 1.1\n%YAML 1.2\n---\nfoo")))
    (check-exn
     #rx"found duplicate tag handle"
     (λ () (parse-string "%TAG ! !\n%TAG ! !\n---\nfoo")))))
