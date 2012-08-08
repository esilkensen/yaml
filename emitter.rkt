;;;;;; emitter.rkt - YAML emitter.    -*- Mode: Racket -*-

#lang racket

(require (planet dyoo/while-loop) "events.rkt" "utils.rkt")

(define (emitter-error message)
  (error 'emitter message))

(define (make-emitter [out (current-output-port)]
                      #:canonical [canonical #f]
                      #:indent [indent #f]
                      #:width [width #f]
                      #:allow-unicode [allow-unicode #f]
                      #:line-break [line-break #f])
  (define DEFAULT-TAG-PREFIXES
    #hash(("!" . "!") ("tag:yaml.org,2002:" . "!!")))

  (define encoding #f)
  (define states '())
  (define (state) (expect-stream-start))
  (define events '())
  (define event #f)
  (define indents '())
  (define indent #f)
  (define flow-level 0)
  (define root-context #f)
  (define sequence-context #f)
  (define mapping-context #f)
  (define simple-key-context #f)
  (define line 0)
  (define column 0)
  (define whitespace #t)
  (define indentation #t)
  (define open-ended #f)
  (define best-indent
    (if (and indent (< 1 indent 10)) indent 2))
  (define best-width
    (if (and width (> width (* 2 best-indent))) width 80))
  (define best-line-break
    (if (member line-break '("\r" "\n" "\r\n")) line-break "\n"))
  (define tag-prefixes #f)
  (define prepared-anchor #f)
  (define prepared-tag #f)
  (define analysis #f)
  (define style #f)

  (define (dispose)
    ;; Reset the state attributes (to clear self-references)
    (set! states '())
    (set! state #f))

  (define (emit event)
    (append! events (list event))
    (while (not (need-more-events))
      (set! event (car events))
      (set! events (cdr events))
      (state)
      (set! event #f)))

  ;; In some cases, we wait for a few next events before emitting.

  (define (need-more-events)
    (or (null? events)
        (case (event-type (car events))
          [(document-start) (need-events 1)]
          [(sequence-start) (need-events 2)]
          [(mapping-start) (need-events 3)]
          [else #f])))

  (define (need-events count)
    (let loop ([level 0] [es (cdr events)])
      (if (null? es)
          (< (length events) (+ count 1))
          (case (event-type (car es))
            [(document-start collection-start sequence-start mapping-start)
             (loop (+ level 1) (cdr es))]
            [(document-end collection-end sequence-end mapping-end)
             (loop (- level 1) (cdr es))]
            [(stream-end)
             (loop (- level 1) (cdr es))]
            [else #f]))))

  (define (increase-indent [flow #f] [indentless #f])
    (append! indents (list indent))
    (if (eq? #f indent)
        (set! indent (if flow best-indent 0))
        (unless indentless
          (set! indent (+ indent best-indent)))))

  ;; States.

  ;; Stream handlers.

  (define (expect-stream-start)
    (cond
     [(eq? 'stream-start (event-type event))
      (write-stream-start)
      (set! state expect-first-document-start)]
     [else
      (emitter-error
       (format "expected stream-start, but got ~a"
               (event-type event)))]))

  (define (expect-nothing)
    (emitter-error
     (format "expected nothing, but got ~a"
             (event-type event))))

  ;; Document handlers.

  (define (expect-first-document)
    (expect-document-start #t))

  (define (expect-document-start [first #f])
    (case (event-type event)
      [(document-start)
       (when (and (or (document-start-event-version event)
                      (document-start-event-tags event))
                  open-ended)
         (write-indicator "..." #t)
         (write-indent))
       (when (document-start-event-version event)
         (write-version-directive
          (prepare-version (document-start-event-version event))))
       (set! tag-prefixes (hash-copy DEFAULT-TAG-PREFIXES))
       (when (document-start-event-tags event)
         (let ([tags (document-start-event-tags event)])
           (for ([handle (sort (hash-keys tags) string<?)])
             (let ([prefix (hash-ref tags handle)])
               (hash-set! tag-prefixes prefix handle)
               (let* ([handle-text (prepare-tag-handle handle)]
                      [prefix-text (prepare-tag-prefix prefix)])
                 (write-tag-directive handle-text prefix-text))))))
       (unless (and first (not (document-start-event-explicit event))
                    (not canonical) (not (document-start-event-version event))
                    (or (not (hash? (document-start-event-tags event)))
                        (null? (hash-keys (document-start-event-tags event))))
                    (not (check-empty-document)))
         (write-indent)
         (write-indicator "---" #t)
         (when canonical
           (write-indent)))
       (set! state expect-document-root)]
      [(stream-end)
       (when open-ended
         (write-indicator "..." #t)
         (write-indent))
       (write-stream-end)
       (set! state expect-nothing)]
      [else
       (emitter-error
        (format "expected document-start, but got ~a"
                (event-type event)))]))

  (define (expect-document-end)
    (cond
     [(eq? 'document-end (event-type event))
      (write-indent)
      (when (document-end-explicit event)
        (write-indicator "..." #t)
        (write-indent))
      (flush-stream)
      (set! state expect-document-start)]
     [else
      (emitter-error
       (format "expected document-end, but got ~a"
               (event-type event)))]))

  (define (expect-document-root)
    (append! states (list expect-document-end))
    (expect-node #t #f #f #f))

  ;; Node handlers.

  (define (expect-node [root #f] [sequence #f] [mapping #f] [simple-key #f])
    (set! root-context root)
    (set! sequence-context sequence)
    (set! mapping-context mapping)
    (set! simple-key-context simple-key)
    (case (event-type event)
      [(alias)
       (expect-alias)]
      [(scalar collection-start sequence-start mapping-start)
       (process-anchor "&")
       (process-tag)
       (case (event-type event)
         [(scalar)
          (expect-scalar)]
         [(sequence-start)
          (if (or (> flow-level 0)
                  canonical
                  (sequence-start-event-flow-style event)
                  (check-empty-sequence))
              (expect-flow-sequence)
              (expect-block-sequence))]
         [(mapping-start)
          (if (or (> flow-level 0)
                  canonical
                  (mapping-start-event-flow-style event)
                  (check-empty-mapping))
              (expect-flow-mapping)
              (expect-block-mapping))])]
      [else
       (emitter-error
        (format "expected node, but got ~a"
                (event-type event)))]))

  (define (expect-alias)
    (unless (alias-event-anchor event)
      (emitter-error "anchor is not specified for alias"))
    (process-anchor "*")
    (set! state (pop! states)))

  (define (expect-scalar)
    (increase-indent #t)
    (process-scalar)
    (set! indent (pop! indents))
    (set! state (pop! states)))

  ;; Flow sequence handlers.

  (define (expect-flow-sequence)
    (write-indicator "[" #t #t)
    (set! flow-level (add1 flow-level))
    (increase-indent #t)
    (set! state expect-first-flow-sequence-item))

  (define (expect-first-flow-sequence-item)
    (cond
     [(eq? 'sequence-end (event-type event))
      (set! indent (pop! indents))
      (set! flow-level (sub1 flow-level))
      (write-indicator "]" #f)
      (set! state (pop! states))]
     [else
      (when (or canonical (> column best-width))
        (write-indent))
      (append! states (list expect-flow-sequence-item))
      (expect-node #f #t #f #f)]))

  (define (expect-flow-sequence-item)
    (cond
     [(eq? 'sequence-end (event-type event))
      (set! indent (pop! indents))
      (set! flow-level (sub1 flow-level))
      (when canonical
        (write-indicator "," #f)
        (write-indent))
      (write-indicator "]" #f)
      (set! state (pop! states))]
     [else
      (write-indicator "," #f)
      (when (or canonical (> column best-width))
        (write-indent))
      (append! states (list expect-flow-sequence-item))
      (expect-node #f #t #f #f)]))

  ;; Flow mapping handlers.

  (define (expect-flow-mapping)
    (write-indicator "{" #t #f)
    (set! flow-level (add1 flow-level))
    (increase-indent #t)
    (set! state expect-first-flow-mapping-key))

  (define (expect-first-flow-mapping-key)
    (cond
     [(eq? 'mapping-end (event-type event))
      (set! indent (pop! indents))
      (set! flow-level (sub1 flow-level))
      (write-indicator "}" #f)
      (set! state (pop! states))]
     [else
       (when (or canonical (> column best-width))
         (write-indent))
       (cond
        [(and (not canonical) (check-simple-key))
         (append! states (list expect-flow-mapping-simple-value))
         (expect-node #f #f #t #t)]
        [else
         (write-indicator "?" #t)
         (append! states (list expect-flow-mapping-value))
         (expect-node #f #f #t #f)])]))

  (define (expect-flow-mapping-key)
    (cond
     [(eq? 'mapping-end (event-type event))
      (set! indent (pop! indents))
      (set! flow-level (sub1 flow-level))
      (when canonical
        (write-indicator "," #f)
        (write-indent))
      (write-indicator "}" #f)
      (set! state (pop! states))]
     [else
      (write-indicator "," #f)
      (when (or canonical (> column best-width))
        (write-indent))
      (cond
       [(and (not canonical) (check-simple-key))
        (append! states (list expect-flow-mapping-simple-value))
        (expect-node #f #f #t #t)]
       [else
        (write-indicator "?" #t)
        (append! states (list expect-flow-mapping-value))
        (expect-node #f #f #t #f)])]))

  (define (expect-flow-mapping-simple-value)
    (write-indicator ":" #f)
    (append! states (list expect-flow-mapping-key))
    (expect-node #f #f #t #f))

  (define (expect-flow-mapping-value)
    (when (or canonical (> column best-width))
      (write-indent))
    (write-indicator ":" #t)
    (append! states (list expect-flow-mapping-key))
    (expect-node #f #f #t #f))

  ;; Block sequence handlers.

  (define (expect-block-sequence)
    (increase-indent #f (and mapping-context (not indentation)))
    (set! state expect-first-block-sequence-item))

  (define (expect-first-block-sequence-item)
    (expect-block-sequence-item #t))

  (define (expect-block-sequence-item [first #f])
    (cond
     [(and (not first) (eq? 'sequence-end (event-type event)))
      (set! indent (pop! indents))
      (set! state (pop! states))]
     [else
      (write-indent)
      (write-indicator "-" #t #f #t)
      (append! states (list expect-block-sequence-item))
      (expect-node #f #t #f #f)]))

  ;; Block mapping handlers.

  
  
  ;; Checkers.

  

  ;; Anchor, Tag, and Scalar processors.

  

  ;; Analyzers.

  

  ;; Writers.

  ;;(define (write-indicator indicator need-whitespace
  ;;                         [whitespace #f] [indention #f])

  ;; Scalar streams.
