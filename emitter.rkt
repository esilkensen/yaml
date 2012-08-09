;;;;;; emitter.rkt - YAML emitter.    -*- Mode: Racket -*-

#lang racket

(require (planet dyoo/while-loop) srfi/13 "events.rkt" "utils.rkt")

(define (emitter-error message)
  (error 'emitter message))

(struct scalar-analysis
  (scalar empty multiline allow-flow-plain allow-block-plain
          allow-single-quoted allow-double-quoted allow-block))

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
     [(stream-start-event? event)
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

  (define (expect-first-document-start)
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
                    (not (check-empty-document?)))
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
                  (check-empty-sequence?))
              (expect-flow-sequence)
              (expect-block-sequence))]
         [(mapping-start)
          (if (or (> flow-level 0)
                  canonical
                  (mapping-start-event-flow-style event)
                  (check-empty-mapping?))
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
    (write-indicator "{" #t #f)
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
    (increase-indent #f (and mapping-context (not indentation)))
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
           (scalar-event? e)
           (not (scalar-event-anchor e))
           (not (scalar-event-tag e))
           (scalar-event-implicit e)
           (equal? "" (scalar-event-value e)))))

  (define (check-simple-key?)
    (let ([len 0])
      (when (and (any-node-event? event)
                 (any-event-attr 'anchor event))
        (unless prepared-anchor
          (set! prepared-anchor
                (prepare-anchor (any-event-attr 'anchor event))))
        (set! len (+ len (string-length prepared-anchor))))
      (when (and (or (scalar-event? event)
                     (any-collection-start-event? event))
                 (any-event-attr 'tag event))
        (unless prepared-tag
          (set! prepared-tag (prepare-tag (any-event-attr 'tag event))))
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
     [(not (any-event-attr 'anchor event))
      (set! prepared-anchor #f)]
     [else
      (unless prepared-anchor
        (set! prepared-anchor
              (prepare-anchor (any-event-attr 'anchor event))))
      (when prepared-anchor
        (write-indicator (format "~a~a" indicator prepared-anchor) #t))
      (set! prepared-anchor #f)]))

  (define (process-tag)
    (let ([tag (any-event-attr 'tag event)])
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
             (any-event-attr 'implicit event))
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
     [(or (equal? "\"" style) canonical)
      "\""]
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
           (member (scalar-event-style event) '("|" ">"))
           (scalar-analysis-allow-single-quoted analysis)
           (not (and simple-key-context
                     (scalar-analysis-multiline analysis))))
      "\'"]
     [else
      "\""]))

  (define (process-scalar)
    (unless analysis
      (set! analysis (analyze-scalar (scalar-event-value event))))
    (unless style
      (set! style (not simple-key-context)))
    (let ([split (not simple-key-context)])
      (cond
       [(equal? "\"" style)
        (write-double-quoted (scalar-analysis-scalar analysis) split)]
       [(equal? "\'" style)
        (write-single-quoted (scalar-analysis-scalar analysis) split)]
       [(equal? ">" style)
        (write-folded (scalar-analysis-scalar analysis))]
       [(equal? "|" style)
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
      (for ([ch (substring handle 1 (- (string-length handle) 1))])
        (unless (or (char<=? #\0 ch #\9)
                    (char<=? #\A ch #\Z)
                    (char<=? #\a ch #\z)
                    (char=? #\- ch)
                    (char=? #\_ ch))
          (emitter-error
           (format "invalid character ~a in the tag handle: ~a" ch handle))))
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
        (append! chunks (substring prefix start end)))
      (apply string-append chunks)))

  (define (prepare-tag tag)
    (unless (> (string-length tag) 0)
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
              (set! suffix (substring prefix (string-length prefix)))))
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
              (append! chunks (substring suffix start end)))
            (if (> (string-length handle) 0)
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

  (define (analyze-scalar scalar) #f) ;; TODO
              
  ;; Writers.

  (define (flush-stream) #f) ;; TODO

  (define (write-stream-start) #f) ;; TODO

  (define (write-stream-end) #f) ;; TODO

  (define (write-indicator indicator need-whitespace
                           [whitespace #f] [indention #f]) #f) ;; TODO

  (define (write-indent) #f) ;; TODO

  (define (write-line-break [data #f]) #f) ;; TODO

  (define (write-version-directive version-text) #f) ;; TODO

  (define (write-tag-directive handle-text prefix-text) #f) ;; TODO

  ;; Scalar streams.

  (define (write-single-quoted text [split #t]) #f) ;; TODO

  (define (write-double-quoted text [split #t]) #f) ;; TODO

  (define (determine-block-hints text) #f) ;; TODO

  (define (write-folded text) #f) ;; TODO

  (define (write-literal text) #f) ;; TODO

  (define (write-plain text [split #t]) #f) ;; TODO

  ;; TODO
  (values #f))
