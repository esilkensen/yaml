;;;;;; scanner.rkt - YAML scanner.    -*- Mode: Racket -*-

#lang racket

(require srfi/13) ; String Libraries
(require (planet dyoo/while-loop))
(require "tokens.rkt")

(provide (all-defined-out))

(define (scanner-error context problem problem-mark)
  (error 'scanner "~a\n~a\n~a" context problem problem-mark))

(struct simple-key (token-number required? index line column mark))

(define (make-scanner)
  ;; TODO
  (define column 0)
  (define line 0)
  (define index 0)
  (define (peek [k 0]) #f)
  (define (forward [k 0]) #f)
  (define (prefix [k 0]) #f)
  (define (get-mark) #f)
  (define (add-token! token) #f)
  
  ;; Had we reached the end of the stream?
  (define done? #f)
  
  ;; The number of unclosed '{' and '['. `flow_level == 0' means block
  ;; context.
  (define flow-level 0)
  
  ;; List of processed tokens that are not yet emitted.
  (define tokens '())
  
  ;; Number of tokens that were emitted through the `get_token' method.
  (define tokens-taken 0)
  
  ;; The current indentation level.
  (define indent -1)

  ;; Past indentation levels.
  (define indents '())

  ;;; Variables related to simple keys treatment.
  
  ;; A simple key is a key that is not denoted by the '?' indicator.
  ;; Example of simple keys:
  ;;   ---
  ;;   block simple key: value
  ;;   ? not a simple key:
  ;;   { flow simple key: value }
  ;; We emit the KEY token before all keys, so when we find a potential
  ;; simple key, we try to locate the corresponding ':' indicator.
  ;; Simple keys should be limited to a single line and 1024 characters.
  
  ;; Can a simple key start at the current position? A simple key may
  ;; start:
  ;; - at the beginning of the line, not counting indentation spaces
  ;;       (in block context),
  ;; - after '{', '[', ',' (in the flow context),
  ;; - after '?', ':', '-' (in the block context).
  ;; In the block context, this flag also signifies if a block
  ;; collection may start at the current position.
  (define allow-simple-key #t)
  
  ;; Keep track of possible simple keys. This is a dictionary. The key
  ;; is `flow_level'; there can be no more than one possible simple key
  ;; for each level. The value is a SIMPLE-KEY record:
  ;;   (token-number, required, index, line, column, mark)
  ;; A simple key may start with ALIAS, ANCHOR, TAG, SCALAR(flow),
  ;; '[' or '{' tokens.
  (define possible-simple-keys (make-hash))
  
  ;;; Public methods.

  (define (check-token? choices)
    ;; Check if the next token is one of the given types.
    (while (need-more-tokens?)
      (fetch-more-tokens))
    (and (list? tokens)
         (let ([t (token-type (car tokens))])
           (or (not choices)
               (and (list? choices)
                    (ormap (λ (c) (eq? t c)) choices))))))
  
  (define (peek-token)
    ;; Return the next token, but do not delete if from the queue.
    (while (need-more-tokens?)
      (fetch-more-tokens))
    (and (list? tokens) (car tokens)))
  
  (define (get-token)
    ;; Return the next token.
    (while (need-more-tokens?)
      (fetch-more-tokens))
    (and (list? tokens)
         (begin0 (car tokens)
           (set! tokens (cdr tokens))
           (set! tokens-taken (add1 tokens-taken)))))

  ;;; Private methods.

  (define (need-more-tokens?)
    (and (not done?)
         (or (list? tokens)
             ;; The current token may be a potential simple key, so we
             ;; need to look further.
             (begin
               (stale-possible-simple-keys!)
               (= (next-possible-simple-key) tokens-taken)))))
  
  (define (fetch-more-tokens)
    (define ctable
      (make-hash
       `((#\% . (,check-directive? . ,fetch-directive))
         (#\- . ((,check-document-start? . ,fetch-document-start)
                 (,check-block-entry? . ,fetch-block-entry)))
         (#\. . (,check-document-end? . ,fetch-document-end))
         (#\[ . ,fetch-flow-sequence-start)
         (#\{ . ,fetch-flow-mapping-start)
         (#\] . ,fetch-flow-sequence-end)
         (#\} . ,fetch-flow-mapping-end)
         (#\, . ,fetch-flow-entry)
         (#\* . ,fetch-alias)
         (#\& . ,fetch-anchor)
         (#\! . ,fetch-tag)
         (#\' . ,fetch-single)
         (#\" . ,fetch-double)
         (#\? . (,check-key? . ,fetch-key))
         (#\| . (,(λ () (zero? flow-level)) . ,fetch-literal)))))
    (define (check-ch? ch)
      (match (hash-ref ctable ch #f)
        [(? procedure? fetch) fetch]
        [(cons (? procedure? check?) (? procedure? fetch))
         (and (check?) fetch)]
        [(list (cons (? procedure? checks?) (? procedure? fetches)) ...)
         (for/or ([check? checks?] [fetch fetches])
           (and (check?) fetch))]
        [else #f]))
    (scan-to-next-token)
    (stale-possible-simple-keys!)
    (unwind-indent! column)
    (let ([ch (peek)])
      (cond
       [(or (eof-object? ch) (char=? #\nul ch))
        (fetch-stream-end)]
       [(check-ch? ch) ((check-ch? ch))]
       [(check-plain?) (fetch-plain)]
       [else
        (scanner-error
         "while scanning for the next token"
         (format "found character ~a that cannot start any token" ch)
         (get-mark))])))
  
  ;;; Simple keys treatment.

  (define (next-possible-simple-key)
    ;; Return the number of the nearest possible simple key.
    (and (hash? possible-simple-keys)
         (simple-key-token-number
          (hash-ref possible-simple-keys
                    (apply min (hash-keys possible-simple-keys))))))

  (define (stale-possible-simple-keys!)
    ;; Remove entries that are no longer possible simple keys. According
    ;; to the YAML specification, simple keys
    ;; - should be limited to a single line.
    ;; - should be no longer than 1024 characters.
    ;; Disabling this procedure will allow simple keys of any length and
    ;; height (may cause problems if indentation is broken though).
    (for ([(level key) possible-simple-keys])
      (when (or (not (= (simple-key-line key) line))
                (> (simple-key-index (- index key)) 1024))
        (when (simple-key-required? key)
          (scanner-error
           "while scanning a simple key"
           "could not find expected ':'"
           (get-mark)))
        (hash-remove! possible-simple-keys level))))

  (define (save-possible-simple-key!)
    ;; The next token may start a simple key. We check if it's possible
    ;; and save its position. This function is called for
    ;;   ALIAS, ANCHOR, TAG, SCALAR(flow), '[', and '{'.
    (define required? (and (zero? flow-level) (= indent column)))
    (when (or allow-simple-key (not required?))
      (error 'scanner "required simple key not allowed"))
    (when allow-simple-key
      (remove-possible-simple-key!)
      (let ([token-number (+ tokens-taken (length tokens))])
        (hash-set!
         possible-simple-keys
         flow-level
         (simple-key token-number required? index line column (get-mark))))))

  (define (remove-possible-simple-key!)
    ;; Remove the saved possible key position at the current flow level.
    (when (hash-has-key? possible-simple-keys flow-level)
      (let ([key (hash-ref possible-simple-keys flow-level)])
        (when (simple-key-required? key)
          (scanner-error
           "while scanning a simple key"
           "could not find expected ':'"
           (get-mark)))
        (hash-remove! possible-simple-keys flow-level))))

  ;;; Indentation functions.

  (define (unwind-indent! column)
    ;; In the flow context, indentation is ignored. We make the scanner
    ;; less restrictive than specification requires.
    (unless (zero? flow-level)
      ;; In block context, we may need to issue the BLOCK-END tokens.
      (while (> indent column)
        (let ([mark (get-mark)])
          (set! indent (car indents))
          (set! indents (cdr indents))
          (add-token! (block-end-token mark mark))))))

  (define (add-indent! column)
    ;; Check if we need to increase indentation.
    (and (< indent column)
         (begin0 #t
           (set! indent column)
           (set! indents (cons indent indents)))))

  ;;; Fetchers.

  (define (fetch-stream-start)
    (let ([mark (get-mark)])
      (add-token! (stream-start-token mark mark))))

  (define (fetch-stream-end)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (set! possible-simple-keys (make-hash))
    (let ([mark (get-mark)])
      (add-token! (stream-end-token mark mark)))
    (set! done? #t))

  (define (fetch-directive)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-directive)))

  (define (fetch-document-start)
    (fetch-document-indicator document-start-token))

  (define (fetch-document-end)
    (fetch-document-indicator document-end-token))

  (define (fetch-document-indicator token)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (let ([start-mark (get-mark)])
      (forward 3)
      (let ([end-mark (get-mark)])
        (add-token! (token start-mark end-mark)))))

  (define (fetch-flow-sequence-start)
    (fetch-flow-collection-start flow-sequence-start-token))

  (define (fetch-flow-mapping-start)
    (fetch-flow-collection-start flow-mapping-start-token))

  (define (fetch-flow-collection-start token)
    (save-possible-simple-key!)
    (set! flow-level (add1 flow-level))
    (set! allow-simple-key #t)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (token start-mark end-mark)))))

  (define (fetch-flow-sequence-end)
    (fetch-flow-collection-end flow-sequence-end-token))

  (define (fetch-flow-mapping-end)
    (fetch-flow-collection-end flow-mapping-end-token))

  (define (fetch-flow-collection-end token)
    (remove-possible-simple-key!)
    (set! flow-level (sub1 flow-level))
    (set! allow-simple-key #f)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (token start-mark end-mark)))))

  (define (fetch-flow-entry)
    (set! allow-simple-key #t)
    (remove-possible-simple-key!)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (flow-entry-token start-mark end-mark)))))

  (define (fetch-block-entry)
    (when (zero? flow-level)
      (unless allow-simple-key
        (let ([problem "sequence entries are not allowed here"]
              [problem-mark (get-mark)])
          (error 'scanner "~a\n~a" problem problem-mark)))
      (when (add-indent! column)
        (let ([mark (get-mark)])
          (add-token! (block-sequence-start-token mark mark)))))
    (set! allow-simple-key #t)
    (remove-possible-simple-key!)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (block-entry-token start-mark end-mark)))))

  (define (fetch-key)
    (when (zero? flow-level)
      (unless allow-simple-key
        (let ([problem "mapping keys are not allowed here"]
              [problem-mark (get-mark)])
          (error 'scanner "~a\n~a" problem problem-mark)))
      (when (add-indent! column)
        (let ([mark (get-mark)])
          (add-token! (block-mapping-start-token mark mark)))))
    (set! allow-simple-key (not flow-level))
    (remove-possible-simple-key!)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (key-token start-mark end-mark)))))

  (define (fetch-value)
    (cond
     [(hash-has-key? possible-simple-keys flow-level)
      (let* ([key (hash-ref possible-simple-keys flow-level)]
             [i (- (simple-key-token-number key) tokens-taken)]
             [mark (simple-key-mark key)])
        (hash-remove! possible-simple-keys flow-level)
        (add-token! (key-token mark mark) i)
        (when (zero? flow-level)
          (when (add-indent! (simple-key-column key))
            (add-token! (block-mapping-start-token mark mark) i)))
        (set! allow-simple-key #f))]
     [else
      (when (zero? flow-level)
        (unless allow-simple-key
          (let ([problem "mapping values are not allowed here"]
                [problem-mark (get-mark)])
            (error 'scanner "~a\n~a" problem problem-mark)))
        (when (add-indent! column)
          (let ([mark (get-mark)])
            (add-token! (block-mapping-start-token mark mark)))))
      (set! allow-simple-key (zero? flow-level))
      (remove-possible-simple-key!)])
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (value-token start-mark end-mark)))))

  (define (fetch-alias)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-anchor alias-token)))

  (define (fetch-anchor)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-anchor anchor-token)))

  (define (fetch-tag)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-tag)))

  (define (fetch-literal)
    (fetch-block-scalar #\|))

  (define (fetch-folded)
    (fetch-block-scalar #\>))

  (define (fetch-block-scalar style)
    (set! allow-simple-key #t)
    (remove-possible-simple-key!)
    (add-token! (scan-block-scalar style)))

  (define (fetch-single)
    (fetch-flow-scalar #\'))

  (define (fetch-double)
    (fetch-flow-scalar #\"))

  (define (fetch-flow-scalar style)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-flow-scalar style)))

  (define (fetch-plain)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-plain)))

  ;;; Checkers.

  (define (check-directive?)
    ;; DIRECTIVE: ^ '%' ...
    ;; The '%' indicator is already checked.
    (zero? column))

  (define (check-document-start?)
    ;; DOCUMENT-START: ^ '---' (' '|'\n')
    (and (zero? column)
         (string=? "---" (prefix 3))
         (and (char? (peek 3))
              (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 3)))))

  (define (check-document-end?)
    ;; DOCUMENT-END: ^ '...' (' '|'\n')
    (and (zero? column)
         (string=? "..." (prefix 3))
         (and (char? (peek 3))
              (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 3)))))

  (define (check-block-entry?)
    ;; BLOCK-ENTRY: '-' (' '|'\n')
    (and (char? (peek 1))
         (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1))))

  (define (check-key?)
    ;; KEY(flow context): '?'
    ;; KEY(block context): '?' (' '|'\n')
    (or (not (zero? flow-level))
        (and (char? (peek 1))
             (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1)))))

  (define (check-value?)
    ;; VALUE(flow context): ':'
    ;; VALUE(block context): ':' (' '|'\n')
    (or (not (zero? flow-level))
        (and (char? (peek 1))
             (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1)))))

  (define (check-plain?)
    ;; A plain scalar may start with any non-space character except:
    ;;   '-', '?', ':', ',', '[', ']', '{', '}',
    ;;   '#', '&', '*', '!', '|', '>', '\', '\"',
    ;;   '%', '@', '`'.
    ;;
    ;; It may also start with
    ;;   '-', '?', ':'
    ;; if it is followed by a non-space character.
    ;;
    ;; Note that we limit the last rule to the block context (except the
    ;; '-' character) because we want the flow context to be space
    ;; independent.
    (or (and (not (eof-object? (peek)))
             (not (string-index
                   "\0 \t\r\n\x85\u2028\u2029-?:,[]{}#&*!|>'\"%@"
                   (peek))))
        (and (not (eof-object? (peek)))
             (not (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1)))
             (or (equal? #\- (peek))
                 (and (zero? flow-level)
                      (string-index "?:" (peek)))))))

  ;; Scanners.
  
  (define (scan-to-next-token)
    ;; We ignore spaces, line breaks and comments.
    ;; If we find a line break in the block section, we set the flag
    ;; `allow_simple_key' on.
    (when (and (zero? index) (equal? #\uFEFF (peek)))
      (forward))
    (let ([found #f])
      (while (not found)
        (while (equal? #\space (peek))
          (forward))
        (when (equal? #\# (peek))
          (while (and (not (eof-object? (peek)))
                      (not (string-index "\0\r\n\x85\u2028\u2029" (peek))))
            (forward)))
        (if (> 0 (string-length (scan-line-break)))
            (when (zero? flow-level)
              (set! allow-simple-key #t))
            (set! found #t)))))

  (define (scan-directive)
    ;; See the specification for details.
    (let ([start-mark (get-mark)])
      (forward)
      (let ([name (scan-directive-name start-mark)]
            [value #f] [end-mark #f])
        (cond
         [(string=? "YAML" name)
          (set! value (scan-yaml-directive-value start-mark))
          (set! end-mark (get-mark))]
         [(string=? "TAG" name)
          (set! value (scan-tag-directive-value start-mark))
          (set! end-mark (get-mark))]
         [else
          (set! end-mark (get-mark))
          (while (and (not (eof-object? (peek)))
                      (not (string-index "\0\r\n\x85\u2028\u2029" (peek))))
            (forward))])
        (scan-directive-ignored-line start-mark)
        (directive-token start-mark end-mark name value))))

  (define (scan-directive-name start-mark)
    ;; See the specification fro details.
    (let ([len 0])
      (while (regexp-match? #rx"[0-9A-Za-z_-]" (string (peek len)))
        (set! len (add1 len)))
      (when (zero? len)
        (scanner-error
         "while scanning a directive"
         (format "expected alphanumeric character, but found ~a" (peek))
         (get-mark)))
      (let ([value (prefix len)])
        (forward len)
        (unless (or (eof-object? (peek))
                    (string-index "\0 \r\n\x85\u2028\u2029" (peek)))
          (scanner-error
           "while scanning a directive"
           (format "expected alphanumeric character, but found ~a" (peek))
           (get-mark)))
        value)))

  (define (scan-yaml-directive-value start-mark)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (let ([major (scan-yaml-directive-number start-mark)])
      (unless (equal? #\. (peek))
        (scanner-error
         "while scanning a directive"
         (format "expected a digit or '.', but found ~a" (peek))
         (get-mark)))
      (forward)
      (let ([minor (scan-yaml-directive-number start-mark)])
        (unless (or (eof-object? (peek))
                    (string-index "\0 \r\n\x85\u2028\u2029" (peek)))
          (scanner-error
           "while scanning a directive"
           (format "expected a diit or ' ', but found ~a" (peek))
           (get-mark)))
        (cons major minor))))

  (define (scan-yaml-directive-number start-mark)
    ;; See the specification for details.
    (unless (char<=? #\0 (peek) #\9)
      (scanner-error
       "while scanning a directive"
       (format "expected a digit, but found ~a" (peek))
       (get-mark)))
    (let ([len 0])
      (while (char<=? #\0 (peek len) #\9)
        (set! len (add1 len)))
      (let ([value (string->number (prefix len))])
        (forward len)
        value)))

  (define (scan-tag-directive-value start-mark)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (let ([handle (scan-tag-directive-handle start-mark)])
      (while (equal? #\space (peek))
        (forward))
      (cons handle (scan-tag-directive-prefix start-mark))))

  (define (scan-tag-directive-handle start-mark)
    ;; See the specification for details.
    (let ([value (scan-tag-handle "directive" start-mark)])
      (unless (equal? #\space (peek))
        (scanner-error
         "while scanning a directive"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      value))

  (define (scan-tag-directive-prefix start-mark)
    ;; See the specification for details.
    (let ([value (scan-tag-uri "directive" start-mark)])
      (unless (or (eof-object? (peek))
                  (string-index "\0 \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a directive"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      value))

  (define (scan-directive-ignored-line start-mark)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (when (equal? #\# (peek))
      (while (and (not (eof-object? (peek)))
                  (not (string-index "\0 \r\n\x85\u2028\u2029" (peek))))
        (forward)))
    (unless (or (eof-object? (peek))
                (string-index "\0\r\n\x85\u2028\u2029" (peek)))
      (scanner-error
       "while scanning a directive"
       (format "expected a comment or a line break, but found ~a" (peek))
       (get-mark)))
    (scan-line-break))

  (define (scan-anchor token)
    ;; The specification does not restrict characters for anchors and
    ;; aliases. This may lead to problems, for instance, the document:
    ;;   [ *alias, value ]
    ;; can be interpreted in two ways, as
    ;;   [ "value" ]
    ;; and
    ;;   [ *alias, "value" ]
    ;; Therefore we restrict aliases to numbers and ASCII letters.
    (let ([start-mark (get-mark)]
          [name (if (equal? #\* (peek)) "alias" "anchor")])
      (forward)
      (let ([len 0])
        (while (regexp-match? #rx"[0-9A-Za-z_-]" (string (peek len)))
          (set! len (add1 len)))
        (when (zero? len)
          (scanner-error
           (format "while scanning an ~a" name)
           (format "expected alphanumeric character, but found ~a" (peek))
           (get-mark)))
        (let ([value (prefix len)])
          (forward len)
          (unless (or (eof-object? (peek))
                      (string-index
                       "\0 \t\r\n\x85\u2028\u2029?:,]}%@`"
                       (peek)))
            (scanner-error
             (format "while scanning an ~a" name)
             (format "expected alphanumeric character, but found ~a" (peek))
             (get-mark)))
          (let ([end-mark (get-mark)])
            (token start-mark end-mark value))))))

  (define (scan-tag)
    ;; See the specification for details.
    (let ([start-mark (get-mark)]
          [handle #f] [suffix #f])
      (cond
       [(equal? #\< (peek 1))
        (forward 2)
        (set! suffix (scan-tag-uri "tag" start-mark))
        (unless (equal? #\> (peek))
          (scanner-error
           "while parsing a tag"
           (format "expected '>', but found ~a" (peek))
           (get-mark)))
        (forward)]
       [(or (eof-object? (peek 1))
            (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1)))
        (set! suffix #\!)
        (forward)]
       [else
        (let ([len 1] [use-handle #f])
          (while (and (not (eof-object? (peek len)))
                      (not (string-index
                            "\0 \t\r\n\x85\u2028\u2029"
                            (peek len))))
            (when (equal? #\! (peek len))
              (set! use-handle #t)
              (break))
            (set! len (+ len 1)))
          (set! handle #\!)
          (if use-handle
              (set! handle (scan-tag-handle "tag" start-mark))
              (forward))
          (set! suffix (scan-tag-uri "tag" start-mark)))])
      (unless (or (eof-object? (peek))
                  (string-index "\0 \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a tag"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      (tag-token start-mark (get-mark) (cons handle suffix))))

  (define (scan-block-scalar style)
    ;; See the specification for details.
    (let ([folded (equal? style #\>)]
          [chunks '()] [breaks #f] [line-break ""]
          [start-mark (get-mark)] [end-mark #f])
      (forward)
      (match-let ([(cons chomping increment)
                   (scan-block-scalar-indicators start-mark)])
        (scan-block-scalar-ignored-line start-mark)
        (let ([min-indent (+ indent 1)])
          (when (< min-indent 1)
            (set! min-indent 1))
          (if increment
              (begin
                (set! indent (+ min-indent increment -1))
                (let ([be (scan-block-scalar-breaks indent)])
                  (set! breaks (car be))
                  (set! end-mark (cdr be))))
              (match-let ([(list b i e) (scan-block-scalar-indentation)])
                (set! breaks b)
                (set! end-mark e)
                (set! indent (max min-indent i)))))
        (while (and (= column indent)
                    (char? (peek))
                    (not (char=? #\nul (peek))))
          (set! chunks (append chunks breaks))
          (let ([leading-non-space (not (string-index " \t" (peek)))]
                [len 0])
            (while (and (not (eof-object? (peek len)))
                        (not (string-index
                              "\0\r\n\x85\u2028\u2029"
                              (peek len))))
              (set! len (+ len 1)))
            (set! chunks (append chunks (string->list (prefix len))))
            (forward len)
            (set! line-break (scan-line-break))
            (let ([be (scan-block-scalar-breaks indent)])
              (set! breaks (car be))
              (set! end-mark (cdr be))
              (if (and (= column indent)
                       (char? (peek))
                       (not (char=? #\nul (peek))))
                  (begin ;; Unfortunately, folding rules are ambiguous.
                    (if (and folded leading-non-space
                             (equal? "\n" line-break)
                             (not (string-index " \t" (peek))))
                        (unless breaks
                          (set! chunks (append chunks '(#\space))))
                        (set! chunks
                              (append chunks (string->list line-break)))))
                  (break)))))
        (unless (eq? #f chomping)
          (set! chunks (append chunks (string->list line-break))))
        (when (eq? #t chomping)
          (set! chunks (append chunks breaks)))
        (scalar-token start-mark end-mark (apply string chunks) #f style))))

  (define (scan-block-scalar-indicators start-mark)
    ;; See the specification for details.
    (let ([chomping 'None] [increment 'None])
      (cond
       [(or (equal? #\+ (peek)) (equal? #\- (peek)))
        (set! chomping (equal? #\+ (peek)))
        (forward)
        (when (string-index "0123456789" (peek))
          (set! increment (string->number (string (peek))))
          (when (zero? increment)
            (scanner-error
             "while scanning a block scalar"
             "expected indentation indicator (1-9), but found 0"
             (get-mark)))
          (forward))]
       [(string-index "0123456789" (peek))
        (set! increment (string->number (string (peek))))
        (when (zero? increment)
          (scanner-error
           "while scanning a block scalar"
           "expected indentation indicator (1-9), but found 0"
           (get-mark)))
        (forward)
        (when (or (equal? #\+ (peek)) (equal? #\- (peek)))
          (set! chomping (equal? #\+ (peek)))
          (forward))])
      (unless (or (eof-object? (peek))
                  (string-index "\0 \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a block scalar"
         (format "expected chomping or indentation indicators, but found ~a"
                 (peek))
         (get-mark)))
      (cons chomping increment)))

  (define (scan-block-scalar-ignored-line start-mark)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (when (equal? #\# (peek))
      (while (and (not (eof-object? (peek)))
                  (not (string-index "\0\r\n\x85\u2028\u2029" (peek))))
        (forward)))
    (unless (or (eof-object? (peek))
                (string-index "\0\r\n\x85\u2028\u2029" (peek)))
      (scanner-error
       "while scanning a block scalar"
       (format "expected a comment or a line break, but found ~a" (peek))
       (get-mark)))
    (scan-line-break))

  (define (scan-block-scalar-indentation)
    ;; See the specification for details.
    (let ([chunks '()]
          [max-indent 0]
          [end-mark (get-mark)])
      (while (and (not (eof-object? (peek)))
                  (string-index " \r\n\x85\u2028\u2029" (peek)))
        (cond
         [(not (equal? #\space (peek)))
          (set! chunks (append chunks (string->list (scan-line-break))))
          (set! end-mark (get-mark))]
         [else
          (forward)
          (when (> column max-indent)
            (set! max-indent column))]))
      (list chunks max-indent end-mark)))

  (define (scan-block-scalar-breaks)
    ;; See the specification for details.
    (let ([chunks '()]
          [max-indent 0]
          [end-mark #f])
      (while (and (< column indent)
                  (equal? #\space (peek)))
        (forward))
      (while (and (not (eof-object? (peek)))
                  (string-index "\r\n\x85\u2028\u2029" (peek)))
        (set! chunks (append chunks (string->list (scan-line-break))))
        (set! end-mark (get-mark))
        (while (and (< column indent)
                    (equal? #\space (peek)))
          (forward)))
      (cons chunks end-mark)))

  (define (scan-flow-scalar style)
    ;; See the specification for details.
    ;; Note that we lose indentation rules for quoted scalars. Quoted
    ;; scalars don't need to adhere to indentation because " and ' clearly
    ;; mark the beginning and the end of them. Therefore we are less
    ;; restrictive than the specification requires. We only need to check
    ;; that document separators are not included in scalars.
    (let ([double (equal? #\" style)]
          [start-mark (get-mark)]
          [quote (peek)])
      (forward)
      (let ([chunks (scan-flow-scalar-non-spaces double start-mark)])
        (while (not (equal? quote (peek)))
          (set! chunks
                (append chunks
                        (scan-flow-scalar-spaces double start-mark)))
          (set! chunks
                (append chunks
                        (scan-flow-scalar-non-spaces double start-mark))))
        (forward)
        (let ([end-mark (get-mark)])
          (scalar-token start-mark end-mark (apply string chunks) #f style)))))

  (define (scan-flow-scalar-non-spaces double start-mark)
    ;; See the specification for details.
    (define esc-repls
      #hash((#\0 . #\nul)
            (#\a . #\u07)
            (#\b . #\u08)
            (#\t . #\u09)
            (#\tab . #\u09)
            (#\n . #\u0A)
            (#\v . #\u0B)
            (#\f . #\u0C)
            (#\r . #\u0D)
            (#\e . #\u1B)
            (#\space . #\u20)
            (#\" . #\")
            (#\\ . #\\)
            (#\N . #\u85)
            (#\_ . #\uA0)
            (#\L . #\u2028)
            (#\P . #\u2029)))
    (define esc-codes #hash((#\x . 2) (#\u . 4) (#\U . 8)))
    (let ([chunks '()])
      (while #t
        (let ([len 0])
          (while (and (char? (peek len))
                      (not (string-index "'\"\\\0 \t\r\n\x85\u2028\u2029"
                                         (peek len))))
            (set! len (+ len 1)))
          (unless (zero? len)
            (set! chunks (append chunks (string->list (prefix len))))
            (forward len))
          (cond
           [(and (not double)
                 (equal? #\' (peek))
                 (equal? #\' (peek 1)))
            (set! chunks (append chunks (list #\')))
            (forward 2)]
           [(or (and double (equal? #\' (peek)))
                (and (not double) (or (equal? #\" (peek))
                                      (equal? #\\ (peek)))))
            (set! chunks (append chunks (list (peek))))
            (forward)]
           [(and double (equal? #\\ (peek)))
            (forward)
            (cond
             [(hash-has-key? esc-repls (peek))
              (set! chunks (append chunks (list (hash-ref esc-repls (peek)))))
              (forward)]
             [(hash-has-key? esc-codes (peek))
              (let ([len (hash-ref esc-codes (peek))])
                (forward)
                (for ([k (in-range len)])
                  (let ([ch (peek k)])
                    (unless (and (char? ch)
                                 (string-index "01223456789ABCDEFabcdef" ch))
                      (scanner-error
                       "while scanning a double-quoted scalar"
                       (format "expected escape sequence, but found ~a" ch)))))
                (let ([code (string->number (prefix len) 16)])
                  (set! chunks (append chunks (list (integer->char code))))
                  (forward len)))]
             [(and (char? (peek))
                   (string-index "\r\n\x85\u2028\u2029" (peek)))
              (scan-line-break)
              (set! chunks
                    (append chunks
                            (scan-flow-scalar-breaks double start-mark)))]
             [else
              (scanner-error
               "while scanning a double-quoted scalar"
               (format "found unknown escape character ~a" (peek))
               (get-mark))])]
           [else (break)])))
      chunks))

  (define (scan-flow-scalar-spaces double start-mark)
    ;; See the specification for details.
    (let ([chunks '()] [len 0])
      (while (or (equal? #\space (peek len))
                 (equal? #\tab (peek len)))
        (set! len (add1 len)))
      (let ([whitespaces (prefix len)])
        (forward len)
        (cond 
         [(or (eof-object? (peek)) (char=? #\nul (peek)))
          (scanner-error
           "while scanning a quoted scalar"
           "found unexpected end of stream"
           (get-mark))]
         [(string-index "\r\n\x85\u2028\u2029" (peek))
          (let* ([line-break (scan-line-break)]
                 [breaks (scan-flow-scalar-breaks double start-mark)])
            (cond
             [(not (equal? "\n" line-break))
              (set! chunks (append chunks (string->list line-break)))]
             [(null? breaks)
              (set! chunks (append chunks (list #\space)))])
            (set! chunks (append chunks breaks)))]
         [else
          (set! chunks (string->list whitespaces))])
        chunks)))

  (define (scan-flow-scalar-breaks double start-mark)
    ;; See the specification for details.
    (let ([chunks '()])
      (while #t
        ;; Instead of checking indentation, we check for document
        ;; separators.
        (let ([pre (prefix 3)])
          (when (and (or (equal? "---" pre)
                         (equal? "..." pre))
                     (char? (peek 3))
                     (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 3)))
            (scanner-error
             "while scanning a quoted scalar"
             "found unexpected document separator"
             (get-mark)))
          (while (and (char? (peek)) (string-index " \t" (peek)))
            (forward))
          (if (and (char? (peek))
                   (string-index "\r\n\x85\u2028\u2029" (peek)))
              (set! chunks (append chunks (string->list (scan-line-break))))
              (break))))
      chunks))

  (define (scan-plain)
    ;; See the specification for details.
    ;; We add an additional restriction for the flow context:
    ;;   plain scalars in the flow context cannot contain ',' ':' '?'.
    ;; We also keep track of the `allow-simple-key' flag here.
    (let ([chunks '()]
          [start-mark (get-mark)]
          [end-mark (get-mark)])
      (set! indent (add1 indent))
      (while #t
        (let ([len 0])
          (when (equal? #\# (peek))
            (break))
          (while #t
            (when (and (char? (peek))
                       (or (string-index "\0 \t\r\n\x85\u2028\u2029" (peek))
                           (and (zero? flow-level) (equal? #\: (peek))
                                (string-index "\0 \t\r\n\x85\u2028\u2029"
                                              (peek (add1 len))))
                           (and (> 0 flow-level)
                                (string-index ",:?[]{}" (peek)))))
              (break))
            (set! len (add1 len)))
          (when (and (> 0 flow-level) (equal? #\: (peek))
                     (or (eof-object? (peek (add1 len)))
                         (not (string-index "\0 \t\r\n\x85\u2028\u2029,[]{}"
                                            (peek (add1 len))))))
            (forward len)
            (scanner-error
             "while scanning a plain scalar"
             "found unexpected ':'"
             (get-mark)))
          (when (zero? len)
            (break))
          (set! allow-simple-key #f)
          (set! chunks (append chunks (string->list (prefix len))))
          (forward len)
          (set! end-mark (get-mark))
          (let ([spaces (scan-plain-spaces indent start-mark)])
            (when (or (null? spaces) (equal? #\# (peek))
                      (and (zero? flow-level) (< column indent)))
              (break)))))
      (scalar-token start-mark end-mark (apply string chunks) #t)))

  (define (scan-plain-spaces indent start-mark)
    ;; See the specification for details.
    ;; The specification is really confusing about tabs in plain scalars.
    ;; We just forbid them completely. Do not use tabs in YAML!
    (let ([chunks '()] [len 0])
      (while (equal? #\space (peek len))
        (set! len (add1 len)))
      (let ([whitespaces (prefix len)])
        (forward len)
        (cond
         [(and (char? (peek))
               (string-index "\r\n\x85\u2028\u2029" (peek)))
          (let ([line-break (scan-line-break)])
            (set! allow-simple-key #t)
            (let ([pre (prefix 3)])
              (if (and (or (equal? "---" pre)
                           (equal? "..." pre))
                       (and (char? (peek 3))
                            (string-index
                             "\0 \t\r\n\x85\u2028\u2029"
                             (peek 3))))
                  '()
                  (let ([breaks '()] [ret #f])
                    (while (and (char? (peek))
                                (string-index
                                 "\r\n\x85\u2028\u2029"
                                 (peek)))
                      (cond
                       [(equal? #\space (peek))
                        (forward)]
                       [else
                        (set! breaks
                              (append breaks
                                      (string->list (scan-line-break))))
                        (let ([pre (prefix 3)])
                          (when (and (or (equal? "---" pre)
                                         (equal? "..." pre))
                                     (char? (peek 3))
                                     (string-index
                                      "\0 \t\r\n\x85\u2028\u2029"
                                      (peek 3)))
                            (set! ret '())
                            (break)))]))
                    (cond
                     [(null? ret) ret]
                     [(not (equal? "\n" line-break))
                      (set! chunks
                            (append chunks
                                    (string->list line-break)))]
                     [(null? breaks)
                      (set! chunks
                            (append chunks (list #\space)))])
                    (set! chunks (append chunks breaks))))))]
         [else
          (set! chunks (append chunks (string->list whitespaces)))])
        chunks)))

  (define (scan-tag-handle name start-mark)
    ;; See the specification for details.
    ;; For some strange reason, the specification does not allow '_' in
    ;; tag handles. I have allowed it anyway.
    (unless (equal? #\! (peek))
      (scanner-error
       (format "while scanning a ~a" name)
       (forward "expected '!', but found ~a" (peek))
       (get-mark)))
    (let ([len 1])
      (when (and (char? (peek len))
                 (not (equal? #\space (peek len))))
        (while (and (char? (peek len))
                    (regexp-match? #rx"[0-9A-Za-z_-]" (string (peek len))))
          (set! len (add1 len)))
        (unless (equal? #\! (peek len))
          (forward len)
          (scanner-error
           (format "while scanning a ~a" name)
           (format "expected '!', but found !a" (peek))
           (get-mark)))
        (set! len (add1 len)))
      (let ([value (prefix len)])
        (forward len)
        value)))

  (define (scan-tag-uri name start-mark)
    ;; See the specification for details.
    ;; Note: we do not check if URI is well-formed.
    (let ([chunks '()] [len 0])
      (while (and (char? (peek len))
                  (regexp-match? #rx"[0-9A-Za-z/;?:@&=+$,_.!~*'()[]%-]"
                                 (peek len)))
        (cond
         [(equal? #\% (peek len))
          (set! chunks (append chunks (string->list (prefix len))))
          (forward len)
          (set! len 0)
          (set! chunks
                (append chunks
                        (string->list (scan-uri-escapes name start-mark))))]
         [else (set! len (add1 len))]))
      (when (> 0 len)
        (set! chunks (append chunks (string->list (prefix len))))
        (forward len)
        (set! len 0))
      (when (null? chunks)
        (scanner-error
         (format "while parsing a ~a" name)
         (format "expected URI, but found ~a" (peek))
         (get-mark)))
      (apply string chunks)))

  (define (scan-uri-escapes name start-mark)
    ;; See the specification for details.
    (let ([bytes '()]
          [mark (get-mark)])
      (while (equal? #\% (peek))
        (forward)
        (for ([k (in-range 2)])
          (unless (and (char? (peek k))
                       (regexp-match? #rx"[0-9A-Za-z]" (string (peek k))))
            (scanner-error
             (format "while scanning a ~a" name)
             (format "expected URI escape, but found ~a" (peek k))
             (get-mark))))
        (let ([c (string->number (prefix 2) 16)])
          (set! bytes (append bytes (list (integer->char c))))
          (forward 2)))
      (apply string bytes)))

  (define (scan-line-break)
    ;; Transforms:
    ;;   '\r\n'   => '\n'
    ;;   '\r'     => '\n'
    ;;   '\n'     => '\n'
    ;;   '\x85'   => '\n'
    ;;   '\u2028' => '\u2028'
    ;;   '\u2029' => '\u2029'
    ;;   default  => ''
    (let ([ch (peek)])
      (cond
       [(and (char? ch)
             (string-index "\r\n\x85" ch))
        (if (equal? "\r\n" (prefix 2))
            (forward 2)
            (forward))
        "\n"]
       [(and (char? ch)
             (string-index "\u2028\u2029" ch))
        (forward)
        (string ch)]
       [else ""])))

  ;; Add the STREAM-START token.
  (fetch-stream-start))
