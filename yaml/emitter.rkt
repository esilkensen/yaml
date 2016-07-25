;;;;;; emitter.rkt - YAML emitter.    -*- Mode: Racket -*-

#lang racket

(require
 srfi/13
 "errors.rkt"
 "events.rkt"
 "utils.rkt")

(provide
 (contract-out
  [make-emitter
   (()
    (output-port?
     #:canonical boolean?
     #:indent (or/c exact-positive-integer? #f)
     #:width (or/c exact-positive-integer? #f)
     #:allow-unicode boolean?
     #:line-break (or/c "\n" "\r" "\r\n" #f))
    . ->* .
    ;; emit
    (event? . -> . void?))]))

(define (emitter-error message)
  (error 'emitter message))

(struct scalar-analysis
  (scalar empty multiline allow-flow-plain allow-block-plain
          allow-single-quoted allow-double-quoted allow-block))

(define (make-emitter [out (current-output-port)]
                      #:canonical [canonical #f]
                      #:indent [default-indent #f]
                      #:width [default-width #f]
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
  (define indention #t)
  (define open-ended #f)
  (define best-indent
    (if (and default-indent
             (< 1 default-indent 10))
        default-indent
        2))
  (define best-width
    (if (and default-width
             (> default-width (* 2 best-indent)))
        default-width
        80))
  (define best-line-break
    (if (member line-break '("\r" "\n" "\r\n"))
        line-break
        (string #\newline)))
  (define tag-prefixes #f)
  (define prepared-anchor #f)
  (define prepared-tag #f)
  (define analysis #f)
  (define style #f)
  
  (define (dispose)
    ;; Reset the state attributes (to clear self-references)
    (set! states '())
    (set! state #f))
  
  (define (emit evt)
    (append! events (list evt))
    (while (not (need-more-events?))
      (set! event (car events))
      (set! events (cdr events))
      (state)
      (set! event #f)))
  
  ;; In some cases, we wait for a few next events before emitting.
  
  (define (need-more-events?)
    (cond
      [(null? events) #t]
      [(document-start-event? (car events))
       (need-events? 1)]
      [(sequence-start-event? (car events))
       (need-events? 2)]
      [(mapping-start-event? (car events))
       (need-events? 3)]
      [else #f]))
  
  (define (need-events? count)
    (let loop ([level 0] [es (cdr events)])
      (if (null? es)
          (< (length events) (add1 count))
          (let ([e (car es)])
            (cond
              [(or (document-start-event? e)
                   (collection-start-event? e))
               (set! level (add1 level))]
              [(or (document-end-event? e)
                   (collection-end-event? e))
               (set! level (sub1 level))]
              [(stream-end-event? e)
               (set! level -1)])
            (and (>= level 0)
                 (loop level (cdr es)))))))
  
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
      [(stream-start-event? event)
       (write-stream-start)
       (set! state expect-first-document-start)]
      [else
       (emitter-error
        (format "expected stream-start, but got ~a"
                (event->string event)))]))
  
  (define (expect-nothing)
    (emitter-error
     (format "expected nothing, but got ~a"
             (event->string event))))
  
  ;; Document handlers.
  
  (define (expect-first-document-start)
    (expect-document-start #t))
  
  (define (expect-document-start [first #f])
    (cond
      [(document-start-event? event)
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
                    (not (check-empty-document?)))
         (write-indent)
         (write-indicator "---" #t)
         (when canonical
           (write-indent)))
       (set! state expect-document-root)]
      [(stream-end-event? event)
       (when open-ended
         (write-indicator "..." #t)
         (write-indent))
       (write-stream-end)
       (set! state expect-nothing)]
      [else
       (emitter-error
        (format "expected document-start, but got ~a"
                (event->string event)))]))
  
  (define (expect-document-end)
    (cond
      [(document-end-event? event)
       (write-indent)
       (when (document-end-event-explicit event)
         (write-indicator "..." #t)
         (write-indent))
       (flush-stream)
       (set! state expect-document-start)]
      [else
       (emitter-error
        (format "expected document-end, but got ~a"
                (event->string event)))]))
  
  (define (expect-document-root)
    (append! states (list expect-document-end))
    (expect-node #t #f #f #f))
  
  ;; Node handlers.
  
  (define (expect-node [root #f] [sequence #f] [mapping #f] [simple-key #f])
    (set! root-context root)
    (set! sequence-context sequence)
    (set! mapping-context mapping)
    (set! simple-key-context simple-key)
    (cond
      [(alias-event? event)
       (expect-alias)]
      [(or (scalar-event? event)
           (collection-start-event? event))
       (process-anchor "&")
       (process-tag)
       (cond
         [(scalar-event? event)
          (expect-scalar)]
         [(sequence-start-event? event)
          (if (or (> flow-level 0)
                  canonical
                  (sequence-start-event-flow-style event)
                  (check-empty-sequence?))
              (expect-flow-sequence)
              (expect-block-sequence))]
         [(mapping-start-event? event)
          (if (or (> flow-level 0)
                  canonical
                  (mapping-start-event-flow-style event)
                  (check-empty-mapping?))
              (expect-flow-mapping)
              (expect-block-mapping))])]
      [else
       (emitter-error
        (format "expected node, but got ~a"
                (event->string event)))]))
  
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
      [(sequence-end-event? event)
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
      [(sequence-end-event? event)
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
    (write-indicator "{" #t #t)
    (set! flow-level (add1 flow-level))
    (increase-indent #t)
    (set! state expect-first-flow-mapping-key))
  
  (define (expect-first-flow-mapping-key)
    (cond
      [(mapping-end-event? event)
       (set! indent (pop! indents))
       (set! flow-level (sub1 flow-level))
       (write-indicator "}" #f)
       (set! state (pop! states))]
      [else
       (when (or canonical (> column best-width))
         (write-indent))
       (cond
         [(and (not canonical) (check-simple-key?))
          (append! states (list expect-flow-mapping-simple-value))
          (expect-node #f #f #t #t)]
         [else
          (write-indicator "?" #t)
          (append! states (list expect-flow-mapping-value))
          (expect-node #f #f #t #f)])]))
  
  (define (expect-flow-mapping-key)
    (cond
      [(mapping-end-event? event)
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
         [(and (not canonical) (check-simple-key?))
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
    (increase-indent #f (and mapping-context (not indention)))
    (set! state expect-first-block-sequence-item))
  
  (define (expect-first-block-sequence-item)
    (expect-block-sequence-item #t))
  
  (define (expect-block-sequence-item [first #f])
    (cond
      [(and (not first) (sequence-end-event? event))
       (set! indent (pop! indents))
       (set! state (pop! states))]
      [else
       (write-indent)
       (write-indicator "-" #t #f #t)
       (append! states (list expect-block-sequence-item))
       (expect-node #f #t #f #f)]))
  
  ;; Block mapping handlers.
  
  (define (expect-block-mapping)
    (increase-indent #f)
    (set! state expect-first-block-mapping-key))
  
  (define (expect-first-block-mapping-key)
    (expect-block-mapping-key #t))
  
  (define (expect-block-mapping-key [first #f])
    (cond
      [(and (not first) (mapping-end-event? event))
       (set! indent (pop! indents))
       (set! state (pop! states))]
      [else
       (write-indent)
       (cond
         [(check-simple-key?)
          (append! states (list expect-block-mapping-simple-value))
          (expect-node #f #f #t #t)]
         [else
          (write-indicator "?" #t #f #t)
          (append! states (list expect-block-mapping-value))
          (expect-node #f #f #t #f)])]))
  
  (define (expect-block-mapping-simple-value)
    (write-indicator ":" #f)
    (append! states (list expect-block-mapping-key))
    (expect-node #f #f #t #f))
  
  (define (expect-block-mapping-value)
    (write-indent)
    (write-indicator ":" #t #f #t)
    (append! states (list expect-block-mapping-key))
    (expect-node #f #f #t #f))
  
  ;; Checkers.
  ;;  (expect-node [root #f] [sequence #f] [mapping #f] [simple-key #f])
  
  (define (check-empty-sequence?)
    (and (sequence-start-event? event)
         (not (null? events))
         (sequence-end-event? (car events))))
  
  (define (check-empty-mapping?)
    (and (mapping-start-event? event)
         (not (null? events))
         (mapping-end-event? (car events))))
  
  (define (check-empty-document?)
    (and (document-start-event? event)
         (not (null? events))
         (let ([e (car events)])
           (and (scalar-event? e)
                (not (scalar-event-anchor e))
                (not (scalar-event-tag e))
                (scalar-event-implicit e)
                (equal? "" (scalar-event-value e))))))
  
  (define (check-simple-key?)
    (let ([len 0])
      (when (and (node-event? event)
                 (node-event-anchor event))
        (unless prepared-anchor
          (set! prepared-anchor
                (prepare-anchor (node-event-anchor event))))
        (set! len (+ len (string-length prepared-anchor))))
      (when (and (or (scalar-event? event)
                     (collection-start-event? event))
                 (any-event-tag event))
        (unless prepared-tag
          (set! prepared-tag
                (prepare-tag (any-event-tag event))))
        (set! len (+ len (string-length prepared-tag))))
      (when (scalar-event? event)
        (unless analysis
          (set! analysis (analyze-scalar (scalar-event-value event))))
        (set! len (+ len (string-length (scalar-analysis-scalar analysis)))))
      (or (and (< len 128)
               (alias-event? event))
          (and (scalar-event? event)
               (not (scalar-analysis-empty analysis))
               (not (scalar-analysis-multiline analysis)))
          (check-empty-sequence?)
          (check-empty-mapping?))))
  
  ;; Anchor, Tag, and Scalar processors.
  
  (define (process-anchor indicator)
    (cond
      [(not (any-event-anchor event))
       (set! prepared-anchor #f)]
      [else
       (unless prepared-anchor
         (set! prepared-anchor
               (prepare-anchor (any-event-anchor event))))
       (when prepared-anchor
         (write-indicator (format "~a~a" indicator prepared-anchor) #t))
       (set! prepared-anchor #f)]))
  
  (define (process-tag)
    (let ([tag (any-event-tag event)])
      (when (and (scalar-event? event)
                 (not style))
        (set! style (choose-scalar-style)))
      (cond
        [(and (scalar-event? event)
              (or (not canonical) (not tag))
              (or (and (equal? "" style)
                       (car (scalar-event-implicit event)))
                  (and (not (equal? "" style))
                       (cdr (scalar-event-implicit event)))))
         (set! prepared-tag #f)]
        [(and (not (scalar-event? event))
              (or (not canonical)
                  (not tag))
              (collection-start-event-implicit event))
         (set! prepared-tag #f)]
        [else
         (when (and (scalar-event? event)
                    (car (scalar-event-implicit event))
                    (not tag))
           (set! tag "!")
           (set! prepared-tag #f))
         (unless tag
           (emitter-error "tag is not specified"))
         (unless prepared-tag
           (set! prepared-tag (prepare-tag tag)))
         (when prepared-tag
           (write-indicator prepared-tag #t))
         (set! prepared-tag #f)])))
  
  (define (choose-scalar-style)
    (unless analysis
      (set! analysis (analyze-scalar (scalar-event-value event))))
    (cond
      [(or (equal? #\" (scalar-event-style event)) canonical)
       #\"]
      [(and (not (scalar-event-style event))
            (car (scalar-event-implicit event))
            (not (and simple-key-context
                      (or (scalar-analysis-empty analysis)
                          (scalar-analysis-multiline analysis))))
            (or (and (> flow-level 0)
                     (scalar-analysis-allow-flow-plain analysis))
                (and (= flow-level 0)
                     (scalar-analysis-allow-block-plain analysis))))
       ""]
      [(and (scalar-event-style event)
            (member (scalar-event-style event) '(#\| #\>))
            (zero? flow-level)
            (not simple-key-context)
            (scalar-analysis-allow-block analysis))
       (scalar-event-style event)]
      [(and (or (not (scalar-event-style event))
                (char=? #\' (scalar-event-style event)))
            (scalar-analysis-allow-single-quoted analysis)
            (not (and simple-key-context
                      (scalar-analysis-multiline analysis))))
       #\']
      [else
       #\"]))
  
  (define (process-scalar)
    (unless analysis
      (set! analysis (analyze-scalar (scalar-event-value event))))
    (unless style
      (set! style (not simple-key-context)))
    (let ([split (not simple-key-context)])
      (cond
        [(equal? #\" style)
         (write-double-quoted (scalar-analysis-scalar analysis) split)]
        [(equal? #\' style)
         (write-single-quoted (scalar-analysis-scalar analysis) split)]
        [(equal? #\> style)
         (write-folded (scalar-analysis-scalar analysis))]
        [(equal? #\| style)
         (write-literal (scalar-analysis-scalar analysis))]
        [else
         (write-plain (scalar-analysis-scalar analysis) split)])
      (set! analysis #f)
      (set! style #f)))
  
  ;; Analyzers.
  
  (define (prepare-version version)
    (match-let ([(cons major minor) version])
      (unless (= 1 major)
        (emitter-error
         (format "unsupported YAML version: ~a.~a" major minor)))
      (format "~a.~a" major minor)))
  
  (define (prepare-tag-handle handle)
    (unless (> (string-length handle) 0)
      (emitter-error "tag handle must not be empty"))
    (let ([cs (string->list handle)])
      (unless (and (char=? #\! (car cs))
                   (char=? #\! (last cs)))
        (emitter-error
         (format "tag handle must start and end with '!': ~a" handle)))
      (when (> (string-length handle) 1)
        (for ([ch (substring handle 1 (sub1 (string-length handle)))])
          (unless (or (char<=? #\0 ch #\9)
                      (char<=? #\A ch #\Z)
                      (char<=? #\a ch #\z)
                      (char=? #\- ch)
                      (char=? #\_ ch))
            (emitter-error
             (format
              "invalid character ~a in the tag handle: ~a" ch handle)))))
      handle))
  
  (define (prepare-tag-prefix prefix)
    (unless (> (string-length prefix) 0)
      (emitter-error "tag prefix must not be empty"))
    (let ([chunks '()]
          [start 0]
          [end 0])
      (when (char=? #\! (string-ref prefix 0))
        (set! end 1))
      (while (< end (string-length prefix))
        (let ([ch (string-ref prefix end)])
          (cond
            [(or (char<=? #\0 ch #\9)
                 (char<=? #\A ch #\Z)
                 (char<=? #\a ch #\z)
                 (string-index "-;/?!:@&=+$,_.~*'()[]" ch))
             (set! end (add1 end))]
            [else
             (when (< start end)
               (append! chunks (list (substring prefix start end))))
             (set! start (add1 end))
             (set! end (add1 end))
             (append! chunks (list (format "~a" ch)))])))
      (when (< start end)
        (append! chunks (list (substring prefix start end))))
      (apply string-append chunks)))
  
  (define (prepare-tag tag)
    (unless (and (string? tag) (> (string-length tag) 0))
      (emitter-error "tag must not be empty"))
    (if (string=? "!" tag)
        tag
        (let ([handle #f]
              [suffix tag])
          (for ([prefix (sort (hash-keys tag-prefixes) string<?)])
            (when (and (string-prefix? prefix tag)
                       (or (string=? "!" prefix)
                           (< (string-length prefix)
                              (string-length tag))))
              (set! handle (hash-ref tag-prefixes prefix))
              (set! suffix (substring tag (string-length prefix)))))
          (let ([chunks '()]
                [start 0]
                [end 0])
            (while (< end (string-length suffix))
              (let ([ch (string-ref suffix end)])
                (cond
                  [(or (char<=? #\0 ch #\9)
                       (char<=? #\A ch #\Z)
                       (char<=? #\a ch #\z)
                       (string-index "-;/?!:@&=+$,_.~*'()[]" ch))
                   (set! end (add1 end))]
                  [else
                   (when (< start end)
                     (append! chunks (list (substring suffix start end))))
                   (set! start (add1 end))
                   (set! end (add1 end))
                   (append! chunks (list (format "~a" ch)))])))
            (when (< start end)
              (append! chunks (list (substring suffix start end))))
            (if (and (string? handle) (> (string-length handle) 0))
                (format "~a~a" handle (apply string-append chunks))
                (format "!<~a>" (apply string-append chunks)))))))
  
  (define (prepare-anchor anchor)
    (unless (> (string-length anchor) 0)
      (emitter-error "anchor must not be empty"))
    (for ([ch anchor])
      (unless (or (char<=? #\0 ch #\9)
                  (char<=? #\A ch #\Z)
                  (char<=? #\a ch #\z)
                  (char=? #\- ch)
                  (char=? #\_ ch))
        (emitter-error
         (format "invalid character ~a in the anchor: ~a" ch anchor))))
    anchor)
  
  (define (analyze-scalar scalar)
    (cond
      [(string=? "" scalar)
       (scalar-analysis scalar #t #f #f #t #t #t #f)]
      [else
       (let ([block-indicators #f]
             [flow-indicators #f]
             [line-breaks #f]
             [special-characters #f]
             [leading-space #f]
             [leading-break #f]
             [trailing-space #f]
             [trailing-break #f]
             [break-space #f]
             [space-break #f]
             [preceeded-by-whitespace #t]
             [followed-by-whitespace
              (or (= 1 (string-length scalar))
                  (string-index "\0 \t\r\n\x85\u2028\u2029"
                                (string-ref scalar 1)))]
             [previous-space #f]
             [previous-break #f]
             [index 0]
             [allow-flow-plain #t]
             [allow-block-plain #t]
             [allow-single-quoted #t]
             [allow-double-quoted #t]
             [allow-block #t])
         (when (or (string-prefix? "---" scalar)
                   (string-prefix? "..." scalar))
           (set! block-indicators #t)
           (set! flow-indicators #t))
         (while (< index (string-length scalar))
           (let ([ch (string-ref scalar index)])
             (cond
               [(zero? index)
                (when (string-index "#,[]{}&*!|>'\"%@`" ch)
                  (set! flow-indicators #t)
                  (set! block-indicators #t))
                (when (string-index "?:" ch)
                  (set! flow-indicators #t)
                  (when followed-by-whitespace
                    (set! block-indicators #t)))
                (when (and (char=? #\- ch) followed-by-whitespace)
                  (set! flow-indicators #t)
                  (set! block-indicators #t))]
               [else
                (when (string-index ",?[]{}" ch)
                  (set! flow-indicators #t))
                (when (char=? #\: ch)
                  (set! flow-indicators #t)
                  (when followed-by-whitespace
                    (set! block-indicators #t)))
                (when (and (char=? #\# ch) preceeded-by-whitespace)
                  (set! flow-indicators #t)
                  (set! block-indicators #t))])
             (when (string-index "\n\x85\u2028\u2029" ch)
               (set! line-breaks #t))
             (unless (or (char=? #\newline ch)
                         (char<=? #\space ch #\~))
               (cond
                 [(and (or (char=? #\u0085 ch)
                           (char<=? #\u00A0 ch #\uD7FF)
                           (char<=? #\uE000 ch #\uFFFD))
                       (not (char=? #\uFEFF ch)))
                  (unless allow-unicode
                    (set! special-characters #t))]
                 [else (set! special-characters #t)]))
             (cond
               [(char=? #\space ch)
                (when (zero? index)
                  (set! leading-space #t))
                (when (= index (- (string-length scalar) 1))
                  (set! trailing-space #t))
                (when previous-break
                  (set! break-space #t))
                (set! previous-space #t)
                (set! previous-break #t)]
               [(string-index "\n\x85\u2028\u2029" ch)
                (when (zero? index)
                  (set! leading-break #t))
                (when (= index (- (string-length scalar) 1))
                  (set! trailing-break #t))
                (when previous-space
                  (set! space-break #t))
                (set! previous-space #f)
                (set! previous-break #t)]
               [else
                (set! previous-space #f)
                (set! previous-break #f)])
             (set! index (add1 index))
             (set! preceeded-by-whitespace
                   (string-index "\0 \t\r\n\x85\u2028\u2029" ch))
             (set! followed-by-whitespace
                   (or (>= (+ 1 index) (string-length scalar))
                       (string-index "\0 \t\r\n\x85\u2028\u2029"
                                     (string-ref scalar (+ index 1)))))))
         (when (or leading-space leading-break
                   trailing-space trailing-break)
           (set! allow-flow-plain #f)
           (set! allow-block-plain #f))
         (when trailing-space
           (set! allow-block #f))
         (when break-space
           (set! allow-flow-plain #f)
           (set! allow-block-plain #f)
           (set! allow-single-quoted #f))
         (when (or space-break special-characters)
           (set! allow-flow-plain #f)
           (set! allow-block-plain #f)
           (set! allow-single-quoted #f)
           (set! allow-block #f))
         (when line-breaks
           (set! allow-flow-plain #f)
           (set! allow-block-plain #f))
         (when flow-indicators
           (set! allow-flow-plain #f))
         (when block-indicators
           (set! allow-block-plain #f))
         (scalar-analysis
          scalar #f line-breaks allow-flow-plain allow-block-plain
          allow-single-quoted allow-double-quoted allow-block))]))
  
  ;; Writers.
  
  (define (flush-stream)
    (flush-output out))
  
  (define (write-stream-start)
    ;; no encoding here
    #f)
  
  (define (write-stream-end)
    (flush-stream))
  
  (define (write-indicator indicator need-whitespace
                           [write-whitespace #f] [write-indention #f])
    (let ([data (if (or whitespace (not need-whitespace))
                    indicator
                    (format " ~a" indicator))])
      (set! whitespace write-whitespace)
      (set! indention (and indention write-indention))
      (set! column (+ column (string-length data)))
      (set! open-ended #f)
      (fprintf out data)))
  
  (define (write-indent)
    (let ([indent (or indent 0)])
      (when (or (not indention) (> column indent)
                (and (= column indent) (not whitespace)))
        (write-line-break))
      (when (< column indent)
        (let ([n (- indent column)])
          (set! whitespace #t)
          (set! column indent)
          (fprintf out (list->string (build-list n (λ _ #\space))))))))
  
  (define (write-line-break [data #f])
    (when (char? data)
      (set! data (string data)))
    (unless data
      (set! data best-line-break))
    (set! whitespace #t)
    (set! indention #t)
    (set! line (add1 line))
    (set! column 0)
    (fprintf out data))
  
  (define (write-version-directive version-text)
    (fprintf out (format "%YAML ~a" version-text))
    (write-line-break))
  
  (define (write-tag-directive handle-text prefix-text)
    (fprintf out (format "%TAG ~a ~a" handle-text prefix-text))
    (write-line-break))
  
  ;; Scalar streams.
  
  (define (write-single-quoted text [split #t])
    (write-indicator "'" #t)
    (let ([spaces #f]
          [breaks #f]
          [start 0]
          [end 0])
      (while (<= end (string-length text))
        (let ([ch #f])
          (when (< end (string-length text))
            (set! ch (string-ref text end)))
          (cond
            [spaces
             (unless (char=? #\space ch)
               (if (and (= (+ 1 start) end)
                        (> column best-width)
                        split
                        (not (zero? start))
                        (not (= end (string-length text))))
                   (write-indent)
                   (let ([data (substring text start end)])
                     (set! column (+ column (string-length data)))
                     (fprintf out data)))
               (set! start end))]
            [breaks
             (unless (and (char? ch) (string-index "\n\x85\u2028\u2029" ch))
               (when (char=? (string-ref text start) #\newline)
                 (write-line-break))
               (for ([br (substring text start end)])
                 (if (char=? br #\newline)
                     (write-line-break)
                     (write-line-break br)))
               (write-indent)
               (set! start end))]
            [else
             (when (or (not (char? ch))
                       (string-index " \n\x85\u2028\u2029" ch)
                       (char=? #\' ch))
               (when (< start end)
                 (let ([data (substring text start end)])
                   (set! column (+ column (string-length data)))
                   (fprintf out data)
                   (set! start end))))])
          (when (equal? #\' ch)
            (set! column (+ column 2))
            (fprintf out "''")
            (set! start (add1 end)))
          (when (char? ch)
            (set! spaces (char=? #\space ch))
            (set! breaks (string-index "\n\x85\u2028\u2029" ch)))
          (set! end (add1 end))))
      (write-indicator "'" #f)))
  
  (define (write-double-quoted text [split #t])
    (define ESCAPE-REPLACEMENTS
      #hash((#\nul . #\0)
            (#\u0007 . #\a)
            (#\backspace . #\b)
            (#\tab . #\t)
            (#\newline . #\n)
            (#\vtab . #\v)
            (#\page . #\f)
            (#\return . #\r)
            (#\u001B . #\e)
            (#\" . #\")
            (#\\ . #\\)
            (#\u0085 . #\N)
            (#\u00A0 . #\_)
            (#\u2028 . #\L)
            (#\u2029 . #\P)))
    (let ([start 0] [end 0])
      (write-indicator "\"" #t)
      (while (<= end (string-length text))
        (let ([ch #f])
          (when (< end (string-length text))
            (set! ch (string-ref text end)))
          (when (or (not (char? ch))
                    (string-index "\"\\\x85\u2028\u2029\uFEFF" ch)
                    (not (char<=? #\space ch #\~))
                    (and allow-unicode
                         (or (char<=? #\u00A0 ch #\uD7FF)
                             (char<=? #\uE000 ch #\uFFFD))))
            (when (< start end)
              (let ([data (substring text start end)])
                (set! column (+ column (string-length data)))
                (fprintf out data)
                (set! start end)))
            (when (char? ch)
              (let ([data ""])
                (cond
                  [(hash-has-key? ESCAPE-REPLACEMENTS ch)
                   (let ([esc (hash-ref ESCAPE-REPLACEMENTS ch)])
                     (set! data (format "\\~a" esc)))]
                  [(char<=? ch #\u00FF)
                   (let ([hex (number->string (char->integer ch) 16)])
                     (set! data (format "\\x~a" (string-upcase hex))))]
                  [(char<=? ch #\uFFFF)
                   (let ([hex (number->string (char->integer ch) 16)])
                     (if (= 2 (string-length hex))
                         (set! data (format "\\u00~a" (string-upcase hex)))
                         (set! data (format "\\u~a" (string-upcase hex)))))]
                  [else
                   (let ([hex (number->string (char->integer ch) 16)])
                     (if (= 2 (string-length hex))
                         (set! data (format "\\U000000~a" (string-upcase hex)))
                         (set! data (format "\\U0000~a" (string-upcase hex)))))])
                (set! column (+ column (string-length data)))
                (fprintf out data)
                (set! start (add1 end)))))
          (when (and (< 0 end (sub1 (string-length text)))
                     (or (equal? #\space ch) (>= start end))
                     (> (+ column (- end start)) best-width)
                     split)
            (let ([data (string-append (substring text start end) "\\")])
              (when (< start end)
                (set! start end))
              (set! column (+ column (string-length data)))
              (fprintf out data)
              (write-indent)
              (set! whitespace #f)
              (set! indention #f)
              (when (char=? #\space (string-ref text start))
                (let ([data "\\"])
                  (set! column (+ column (string-length data)))
                  (fprintf out data)))))
          (set! end (add1 end))))
      (write-indicator "\"" #f)))
  
  (define (determine-block-hints text)
    (let ([hints ""])
      (when (and (string? text) (> (string-length text) 0))
        (when (string-index " \n\x85\u2028\u2029" (string-ref text 0))
          (set! hints (format "~a~a" hints best-indent)))
        (cond
          [(not (string-index "\n\x85\u2028\u2029"
                              (string-ref text (- (string-length text) 1))))
           (set! hints (string-append hints "-"))]
          [(or (= 1 (string-length text))
               (string-index "\n\x85\u2028\u2029"
                             (string-ref text (- (string-length text) 2))))
           (set! hints (string-append hints "+"))]))
      hints))
  
  (define (write-folded text)
    (let ([hints (determine-block-hints text)]
          [leading-space #t]
          [spaces #f]
          [breaks #t]
          [start 0]
          [end 0])
      (write-indicator (string-append ">" hints) #t)
      (when (and (> (string-length hints) 0)
                 (char=? #\+ (string-ref hints
                                         (sub1 (string-length hints)))))
        (set! open-ended #t))
      (write-line-break)
      (while (<= end (string-length text))
        (let ([ch #f])
          (when (< end (string-length text))
            (set! ch (string-ref text end)))
          (cond
            [breaks
             (unless (and (char? ch)
                          (string-index "\n\x85\u2028\u2029" ch))
               (when (and (not leading-space)
                          (char? ch)
                          (not (char=? #\space ch))
                          (char=? #\newline (string-ref text start)))
                 (write-line-break))
               (set! leading-space (equal? #\space ch))
               (for ([br (substring text start end)])
                 (if (char=? #\newline br)
                     (write-line-break)
                     (write-line-break br)))
               (when (char? ch)
                 (write-indent))
               (set! start end))]
            [spaces
             (unless (and (char? ch)
                          (char=? #\space ch))
               (if (and (= (add1 start) end) (> column best-width))
                   (write-indent)
                   (let ([data (substring text start end)])
                     (set! column (+ column (string-length data)))
                     (fprintf out data)))
               (set! start end))]
            [else
             (when (or (not (char? ch))
                       (string-index "\n\x85\u2028\u2029" ch))
               (let ([data (substring text start end)])
                 (set! column (+ column (string-length data)))
                 (fprintf out data)
                 (unless (char? ch)
                   (write-line-break))
                 (set! start end)))])
          (when (char? ch)
            (set! breaks (string-index "\n\x85\u2028\u2029" ch))
            (set! spaces (char=? #\space ch)))
          (set! end (add1 end))))))
  
  (define (write-literal text)
    (let ([hints (determine-block-hints text)]
          [breaks #t]
          [start 0]
          [end 0])
      (write-indicator (string-append "|" hints) #t)
      (when (and (> (string-length hints) 0)
                 (char=? #\+ (string-ref hints
                                         (sub1 (string-length hints)))))
        (set! open-ended #t))
      (write-line-break)
      (while (<= end (string-length text))
        (let ([ch #f])
          (when (< end (string-length text))
            (set! ch (string-ref text end)))
          (when breaks
            (when (or (not (char? ch))
                      (not (string-index "\n\x85\u2028\u2029" ch)))
              (for ([br (substring text start end)])
                (if (char=? #\newline br)
                    (write-line-break)
                    (write-line-break br)))
              (when (char? ch)
                (write-indent))
              (set! start end)))
          (unless breaks
            (when (or (not (char? ch))
                      (string-index "\n\x85\u2028\u2029" ch))
              (fprintf out (substring text start end))
              (unless (char? ch)
                (write-line-break))
              (set! start end)))
          (when (char? ch)
            (set! breaks (string-index "\n\x85\u2028\u2029" ch)))
          (set! end (add1 end))))))
  
  (define (write-plain text [split #t])
    (when root-context
      (set! open-ended #t))
    (when (> (string-length text) 0)
      (let ([spaces #f]
            [breaks #f]
            [start 0]
            [end 0])
        (unless whitespace
          (let ([data " "])
            (set! column (+ column (string-length data)))
            (fprintf out data)))
        (set! whitespace #f)
        (set! indention #f)
        (while (<= end (string-length text))
          (let ([ch #f])
            (when (< end (string-length text))
              (set! ch (string-ref text end)))
            (cond
              [spaces
               (unless (equal? #\space ch)
                 (cond
                   [(and (= (add1 start) end)
                         (> column best-width)
                         split)
                    (write-indent)
                    (set! whitespace #f)
                    (set! indention #f)]
                   [else
                    (let ([data (substring text start end)])
                      (set! column (+ column (string-length data)))
                      (fprintf out data))])
                 (set! start end))]
              [breaks
               (unless (and (char? ch)
                            (string-index "\n\x85\u2028\u2029" ch))
                 (when (char=? #\newline (string-ref text start))
                   (write-line-break))
                 (for ([br (substring text start end)])
                   (if (char=? #\newline br)
                       (write-line-break)
                       (write-line-break br)))
                 (write-indent)
                 (set! whitespace #f)
                 (set! indention #f)
                 (set! start end))]
              [else
               (unless (and (char? ch)
                            (string-index "\n\x85\u2028\u2029" ch))
                 (let ([data (substring text start end)])
                   (set! column (+ column (string-length data)))
                   (fprintf out data)
                   (set! start end)))])
            (when (char? ch)
              (set! spaces (char=? #\space ch))
              (set! breaks (string-index "\n\x85\u2028\u2029" ch)))
            (set! end (add1 end)))))))
  
  (values emit))

(module+ test
  (require rackunit racket/generator "parser.rkt")
  (for ([(test-file check-file) (test-files #"emit")])
    (test-case check-file
      (let* ([out (open-output-string)]
             [in (open-input-file check-file)]
             [emit (make-emitter out)])
        (for ([event (parse-file test-file)])
          (emit event))
        (check-equal? (get-output-string out) (port->string in))
        (close-output-port out)
        (close-input-port in)))))
