;;;;;; scanner.rkt - YAML scanner.    -*- Mode: Racket -*-

#lang racket

(require
 srfi/13
 "errors.rkt"
 "tokens.rkt"
 "utils.rkt")

(provide
 (contract-out
  [scan-file (path-string? . -> . (listof token?))]
  [scan-string (string? . -> . (listof token?))]
  [scan (() (input-port?) . ->* . (listof token?))]
  [make-scanner
   (()
    (input-port?)
    . ->* .
    (values
     ;; check-token?
     (() #:rest (listof (any/c . -> . boolean?)) . ->* . boolean?)
     ;; peek-token
     (-> (or/c token? #f))
     ;; get-token
     (-> (or/c token? #f))))]))

(define (scan-file filename)
  (let ([in (open-input-file filename)])
    (begin0 (scan in)
      (close-input-port in))))

(define (scan-string string)
  (let ([in (open-input-string string)])
    (begin0 (scan in)
      (close-input-port in))))

(define (scan [in (current-input-port)])
  (define-values (check-token? peek-token get-token)
    (make-scanner in))
  (let loop ([tokens '()])
    (if (token? (peek-token))
        (loop (cons (get-token) tokens))
        (reverse tokens))))

(define scanner-error (make-error 'scanner))

(struct simple-key (token-number required? index line column mark))

(define (make-scanner [in (current-input-port)])
  (define line 0)
  (define column 0)
  (define index 0)
  (define buffer-length 0)
  (define buffer (make-vector 1024 #\nul))
  
  (define (peek [i 0])
    ;; peek the next i-th character
    (when (>= (+ i index) (vector-length buffer))
      (let ([new-buffer (make-vector (* (vector-length buffer) 2) #\nul)])
        (vector-copy! new-buffer 0 buffer)
        (set! buffer new-buffer)))
    (when (>= (+ index i) buffer-length)
      (for ([j (in-range buffer-length (+ index i 1))])
        (vector-set! buffer j (read-char in))
        (set! buffer-length (add1 buffer-length))))
    (vector-ref buffer (+ i index)))
  
  (define (prefix [l 1])
    ;; peek the next l characters
    (let loop ([i 0] [cs '()])
      (if (= i l)
          (list->string (reverse cs))
          (let ([c (peek i)])
            (if (char? c)
                (loop (+ i 1) (cons c cs))
                (loop l cs))))))
  
  (define (forward [l 1])
    ;; read the next l characters and move the index
    (let ([tmp-index index])
      (for ([i (in-range l)])
        (when (char? (peek i))
          (set! tmp-index (add1 tmp-index))
          (cond
            [(or (and (char? (peek i))
                      (string-index "\n\x85\u2028\u2029" (peek i)))
                 (and (equal? #\return (peek i))
                      (not (equal? #\newline (peek (add1 i))))))
             (set! line (add1 line))
             (set! column 0)]
            [(not (equal? #\uFEFF (peek i)))
             (set! column (add1 column))])))
      (set! index tmp-index)))
  
  (define (get-mark)
    (define name
      (if (path? (object-name in))
          (let-values ([(path-base path-name path-dir?)
                        (split-path (object-name in))])
            path-name)
          (object-name in)))
    (mark name index line column buffer))
  
  (define (add-token! token)
    (set! tokens (append tokens (list token))))
  
  (define (add-token-at! token i)
    (let-values ([(left right) (split-at tokens i)])
      (set! tokens (append left (cons token right)))))
  
  (define done? #f)
  (define flow-level 0)
  (define tokens '())
  (define tokens-taken 0)
  (define indent -1)
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
  
  (define (check-token? . choices)
    ;; Check if the next token is one of the given types.
    (while (need-more-tokens?)
      (fetch-more-tokens))
    (and (not (null? tokens))
         (or (null? choices)
             (and (list? choices)
                  (ormap (λ (c?) (c? (car tokens)))
                         choices)))))
  
  (define (peek-token)
    ;; Return the next token, but do not delete if from the queue.
    (while (need-more-tokens?)
      (fetch-more-tokens))
    (and (not (null? tokens))
         (car tokens)))
  
  (define (get-token)
    ;; Return the next token.
    (let ([token (peek-token)])
      (and (token? token)
           (set! tokens (cdr tokens))
           (set! tokens-taken (add1 tokens-taken))
           token)))
  
  ;;; Private methods.
  
  (define (need-more-tokens?)
    (and (not done?)
         (or (null? tokens)
             ;; The current token may be a potential simple key, so we
             ;; need to look further.
             (begin
               (stale-possible-simple-keys!)
               (equal? (next-possible-simple-key) tokens-taken)))))
  
  (define (fetch-more-tokens)
    (define ctable
      (make-hash
       `((#\% . ((,check-directive? . ,fetch-directive)))
         (#\- . ((,check-document-start? . ,fetch-document-start)
                 (,check-block-entry? . ,fetch-block-entry)))
         (#\. . ((,check-document-end? . ,fetch-document-end)))
         (#\[ . ,fetch-flow-sequence-start)
         (#\{ . ,fetch-flow-mapping-start)
         (#\] . ,fetch-flow-sequence-end)
         (#\} . ,fetch-flow-mapping-end)
         (#\, . ,fetch-flow-entry)
         (#\? . ((,check-key? . ,fetch-key)))
         (#\: . ((,check-value? . ,fetch-value)))
         (#\* . ,fetch-alias)
         (#\& . ,fetch-anchor)
         (#\! . ,fetch-tag)
         (#\| . ((,(λ () (zero? flow-level)) . ,fetch-literal)))
         (#\> . ((,(λ () (zero? flow-level)) . ,fetch-folded)))
         (#\' . ,fetch-single)
         (#\" . ,fetch-double))))
    (define (check-ch? ch)
      (and (hash-has-key? ctable ch)
           (let ([ct (hash-ref ctable ch)])
             (if (list? ct)
                 (let loop ([ct ct])
                   (and (not (null? ct))
                        (if ((caar ct))
                            (cdar ct)
                            (loop (cdr ct)))))
                 ct))))
    (scan-to-next-token)
    (stale-possible-simple-keys!)
    (unwind-indent! column)
    (let ([ch (peek)])
      (if (or (eof-object? ch) (char=? #\nul ch))
          (fetch-stream-end)
          (let ([ct (check-ch? ch)])
            (if ct
                (ct)
                (if (check-plain?)
                    (fetch-plain)
                    (scanner-error
                     "while scanning for the next token"
                     (format
                      "found character ~a that cannot start any token" ch)
                     (get-mark))))))))
  
  ;;; Simple keys treatment.
  
  (define (next-possible-simple-key)
    ;; Return the number of the nearest possible simple key.
    (and (hash? possible-simple-keys)
         (not (null? (hash-keys possible-simple-keys)))
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
    (hash-for-each
     possible-simple-keys
     (λ (level key)
       (when (or (not (= (simple-key-line key) line))
                 (> (- index (simple-key-index key)) 1024))
         (when (simple-key-required? key)
           (scanner-error
            "while scanning a simple key"
            "could not find expected ':'"
            (get-mark)))
         (hash-remove! possible-simple-keys level)))))
  
  (define (save-possible-simple-key!)
    ;; The next token may start a simple key. We check if it's possible
    ;; and save its position. This function is called for
    ;;   ALIAS, ANCHOR, TAG, SCALAR(flow), '[', and '{'.
    (define required? (and (zero? flow-level) (= indent column)))
    (unless (or allow-simple-key (not required?))
      (scanner-error #f "required simple key not allowed" (get-mark)))
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
    (when (zero? flow-level)
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
           (set! indents (cons indent indents))
           (set! indent column))))
  
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
        (scanner-error #f "sequence entries are not allowed here" (get-mark)))
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
        (scanner-error #f "mapping keys are not allowed here" (get-mark)))
      (when (add-indent! column)
        (let ([mark (get-mark)])
          (add-token! (block-mapping-start-token mark mark)))))
    (set! allow-simple-key (zero? flow-level))
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
         (add-token-at! (key-token mark mark) i)
         (when (zero? flow-level)
           (when (add-indent! (simple-key-column key))
             (add-token-at! (block-mapping-start-token mark mark) i)))
         (set! allow-simple-key #f))]
      [else
       (when (zero? flow-level)
         (unless allow-simple-key
           (scanner-error #f "mapping values are not allowed here" (get-mark)))
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
         (or (eof-object? (peek 3))
             (string-index " \t\r\n\x85\u2028\u2029" (peek 3)))))
  
  (define (check-document-end?)
    ;; DOCUMENT-END: ^ '...' (' '|'\n')
    (and (zero? column)
         (string=? "..." (prefix 3))
         (or (eof-object? (peek 3))
             (string-index " \t\r\n\x85\u2028\u2029" (peek 3)))))
  
  (define (check-block-entry?)
    ;; BLOCK-ENTRY: '-' (' '|'\n')
    (or (eof-object? (peek 1))
        (string-index " \t\r\n\x85\u2028\u2029" (peek 1))))
  
  (define (check-key?)
    ;; KEY(flow context): '?'
    ;; KEY(block context): '?' (' '|'\n')
    (or (not (zero? flow-level))
        (or (eof-object? (peek 1))
            (string-index " \t\r\n\x85\u2028\u2029" (peek 1)))))
  
  (define (check-value?)
    ;; VALUE(flow context): ':'
    ;; VALUE(block context): ':' (' '|'\n')
    (or (not (zero? flow-level))
        (or (eof-object? (peek 1))
            (string-index " \t\r\n\x85\u2028\u2029" (peek 1)))))
  
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
                   " \t\r\n\x85\u2028\u2029-?:,[]{}#&*!|>'\"%@"
                   (peek))))
        (and (not (eof-object? (peek)))
             (and (not (string-index " \t\r\n\x85\u2028\u2029" (peek)))
                  (or (equal? #\- (peek))
                      (and (zero? flow-level)
                           (string-index "?:" (peek))))))))
  
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
                      (not (string-index "\r\n\x85\u2028\u2029" (peek))))
            (forward)))
        (if (> (string-length (scan-line-break)) 0)
            (when (zero? flow-level)
              (set! allow-simple-key #t))
            (set! found #t)))))
  
  ;; (: scan-directive (-> directive-token))
  (define (scan-directive)
    ;; See the specification for details.
    (let ([start-mark (get-mark)])
      (forward)
      (let* ([name (scan-directive-name)]
             [value #f]
             [end-mark
              (cond
                [(string=? "YAML" name)
                 (set! value (scan-yaml-directive-value))
                 (get-mark)]
                [(string=? "TAG" name)
                 (set! value (scan-tag-directive-value))
                 (get-mark)]
                [else
                 (begin0 (get-mark)
                   (while (and (not (eof-object? (peek)))
                               (not (string-index
                                     "\r\n\x85\u2028\u2029" (peek))))
                     (forward)))])])
        (scan-directive-ignored-line)
        (directive-token start-mark end-mark name value))))
  
  ;; (: scan-directive-name (-> String))
  (define (scan-directive-name)
    ;; See the specification for details.
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
                    (string-index " \r\n\x85\u2028\u2029" (peek)))
          (scanner-error
           "while scanning a directive"
           (format "expected alphanumeric character, but found ~a" (peek))
           (get-mark)))
        value)))
  
  ;; (: scan-yaml-directive-value (-> (Pairof Integer Integer)))
  (define (scan-yaml-directive-value)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (let ([major (scan-yaml-directive-number)])
      (unless (equal? #\. (peek))
        (scanner-error
         "while scanning a directive"
         (format "expected a digit or '.', but found ~a" (peek))
         (get-mark)))
      (forward)
      (let ([minor (scan-yaml-directive-number)])
        (unless (or (eof-object? (peek))
                    (string-index " \r\n\x85\u2028\u2029" (peek)))
          (scanner-error
           "while scanning a directive"
           (format "expected a digit or ' ', but found ~a" (peek))
           (get-mark)))
        (cons major minor))))
  
  ;; (: scan-yaml-directive-number (-> Integer))
  (define (scan-yaml-directive-number)
    ;; See the specification for details.
    (let ([c (peek)])
      (unless (and (char? c) (char<=? #\0 c #\9))
        (scanner-error
         "while scanning a directive"
         (format "expected a digit, but found ~a" c)
         (get-mark))))
    (let ([len 0])
      (while (let ([c (peek len)])
               (and (char? c)
                    (char<=? #\0 c #\9)))
        (set! len (add1 len)))
      (begin0 (string->number (prefix len))
        (forward len))))
  
  ;; (: scan-tag-directive-value (-> (Pairof String String)))
  (define (scan-tag-directive-value)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (let ([handle (scan-tag-directive-handle)])
      (while (equal? #\space (peek))
        (forward))
      (cons handle (scan-tag-directive-prefix))))
  
  ;; (: scan-tag-directive-handle (-> String))
  (define (scan-tag-directive-handle)
    ;; See the specification for details.
    (let ([value (scan-tag-handle "directive")])
      (unless (equal? #\space (peek))
        (scanner-error
         "while scanning a directive"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      value))
  
  ;; (: scan-tag-directive-prefix (-> String))
  (define (scan-tag-directive-prefix)
    ;; See the specification for details.
    (let ([value (scan-tag-uri "directive")])
      (unless (or (eof-object? (peek))
                  (string-index " \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a directive"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      value))
  
  ;; (: scan-directive-ignored-line (-> String))
  (define (scan-directive-ignored-line)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (when (equal? #\# (peek))
      (while (and (not (eof-object? (peek)))
                  (not (string-index "\r\n\x85\u2028\u2029" (peek))))
        (forward)))
    (unless (or (eof-object? (peek))
                (string-index "\r\n\x85\u2028\u2029" (peek)))
      (scanner-error
       "while scanning a directive"
       (format "expected a comment or a line break, but found ~a" (peek))
       (get-mark)))
    (scan-line-break))
  
  ;; (: scan-anchor
  ;;    ((U (mark mark String -> alias-token)
  ;;        (mark mark String -> anchor-token)) -> (U alias-token anchor-token)))
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
        (while (and (char? (peek len))
                    (regexp-match? #rx"[0-9A-Za-z_-]" (string (peek len))))
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
                       " \t\r\n\x85\u2028\u2029?:,]}%@`"
                       (peek)))
            (scanner-error
             (format "while scanning an ~a" name)
             (format "expected alphanumeric character, but found ~a" (peek))
             (get-mark)))
          (let ([end-mark (get-mark)])
            (token start-mark end-mark value))))))
  
  ;; (: scan-tag (-> tag-token))
  (define (scan-tag)
    ;; See the specification for details.
    (let ([start-mark (get-mark)]
          [handle #f] [suffix #f])
      (cond
        [(equal? #\< (peek 1))
         (forward 2)
         (set! suffix (scan-tag-uri "tag"))
         (unless (equal? #\> (peek))
           (scanner-error
            "while parsing a tag"
            (format "expected '>', but found ~a" (peek))
            (get-mark)))
         (forward)]
        [(or (eof-object? (peek 1))
             (string-index " \t\r\n\x85\u2028\u2029" (peek 1)))
         (set! suffix "!")
         (forward)]
        [else
         (let ([len 1] [use-handle #f])
           (call/cc
            (λ (break)
              (while (and (not (eof-object? (peek len)))
                          (not (string-index
                                " \t\r\n\x85\u2028\u2029"
                                (peek len))))
                (when (equal? #\! (peek len))
                  (break (set! use-handle #t)))
                (set! len (+ len 1)))))
           (set! handle "!")
           (if use-handle
               (set! handle (scan-tag-handle "tag"))
               (forward))
           (set! suffix (scan-tag-uri "tag")))])
      (unless (or (eof-object? (peek))
                  (string-index " \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a tag"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      (tag-token start-mark (get-mark) (cons handle suffix))))
  
  ;; (: scan-block-scalar ((Option Char) -> scalar-token))
  (define (scan-block-scalar style)
    ;; See the specification for details.
    (let ([folded (equal? style #\>)]
          [chunks '()]
          [breaks '()]
          [line-break ""]
          [start-mark (get-mark)]
          [end-mark #f])
      (forward)
      (match-let ([(cons chomping increment)
                   (scan-block-scalar-indicators)]
                  [tmp-indent -1])
        (scan-block-scalar-ignored-line)
        (let ([min-indent (+ indent 1)])
          (when (< min-indent 1)
            (set! min-indent 1))
          (if (integer? increment)
              (begin
                (set! tmp-indent (+ min-indent increment -1))
                (let ([be (scan-block-scalar-breaks tmp-indent)])
                  (set! breaks (car be))
                  (set! end-mark (cdr be))))
              (match-let ([(list b i e) (scan-block-scalar-indentation)])
                (set! breaks b)
                (set! end-mark e)
                (set! tmp-indent (max min-indent i)))))
        (call/cc
         (λ (break)
           (while (and (= column tmp-indent)
                       (let ([c (peek)])
                         (and (char? c)
                              (not (char=? #\nul c)))))
             (set! chunks (append chunks breaks))
             (let ([leading-non-space (not (string-index " \t" (peek)))]
                   [len 0])
               (while (and (not (eof-object? (peek len)))
                           (not (string-index
                                 "\r\n\x85\u2028\u2029"
                                 (peek len))))
                 (set! len (+ len 1)))
               (set! chunks (append chunks (string->list (prefix len))))
               (forward len)
               (set! line-break (scan-line-break))
               (let ([be (scan-block-scalar-breaks tmp-indent)])
                 (set! breaks (car be))
                 (set! end-mark (cdr be))
                 (if (and (= column tmp-indent)
                          (let ([c (peek)])
                            (and (char? c)
                                 (not (char=? #\nul c)))))
                     (begin ;; Unfortunately, folding rules are ambiguous.
                       (if (and folded leading-non-space
                                (equal? "\n" line-break)
                                (not (string-index " \t" (peek))))
                           (when (null? breaks)
                             (set! chunks (append chunks '(#\space))))
                           (set! chunks
                                 (append chunks (string->list line-break)))))
                     (break (void))))))))
        (when (not (eq? #f chomping))
          (set! chunks (append chunks (string->list line-break))))
        (when (eq? #t chomping)
          (set! chunks (append chunks breaks)))
        (let ([end (if (mark? end-mark) end-mark start-mark)])
          (scalar-token start-mark end (list->string chunks) #f style)))))
  
  ;; (: scan-block-scalar-indicators
  ;;    (-> (Pairof (U Boolean 'None) (Option Integer))))
  (define (scan-block-scalar-indicators)
    ;; See the specification for details.
    (let ([chomping 'None] [increment #f])
      (cond
        [(or (equal? #\+ (peek)) (equal? #\- (peek)))
         (set! chomping (equal? #\+ (peek)))
         (forward)
         (let ([c (peek)])
           (when (and (char? c)
                      (string-index "0123456789" (peek)))
             (set! increment (string->number (string c)))
             (when (equal? 0 increment)
               (scanner-error
                "while scanning a block scalar"
                "expected indentation indicator (1-9), but found 0"
                (get-mark)))
             (forward)))]
        [(and (char? (peek)) (string-index "0123456789" (peek)))
         (let ([c (peek)])
           (when (char? c)
             (set! increment (string->number (string c)))))
         (when (equal? 0 increment)
           (scanner-error
            "while scanning a block scalar"
            "expected indentation indicator (1-9), but found 0"
            (get-mark)))
         (forward)
         (when (or (equal? #\+ (peek)) (equal? #\- (peek)))
           (set! chomping (equal? #\+ (peek)))
           (forward))])
      (unless (or (eof-object? (peek))
                  (string-index " \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a block scalar"
         (format "expected chomping or indentation indicators, but found ~a"
                 (peek))
         (get-mark)))
      (cons chomping increment)))
  
  ;; (: scan-block-scalar-ignored-line (-> String))
  (define (scan-block-scalar-ignored-line)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (when (equal? #\# (peek))
      (while (and (not (eof-object? (peek)))
                  (not (string-index "\r\n\x85\u2028\u2029" (peek))))
        (forward)))
    (unless (or (eof-object? (peek))
                (string-index "\r\n\x85\u2028\u2029" (peek)))
      (scanner-error
       "while scanning a block scalar"
       (format "expected a comment or a line break, but found ~a" (peek))
       (get-mark)))
    (scan-line-break))
  
  ;; (: scan-block-scalar-indentation
  ;;    (-> (List (Listof Char) Integer mark)))
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
  
  ;; (: scan-block-scalar-breaks
  ;;    (Integer -> (Pairof (Listof Char) (Option mark))))
  (define (scan-block-scalar-breaks indent)
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
  
  ;; (: scan-flow-scalar ((Option Char) -> scalar-token))
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
      (let ([chunks (scan-flow-scalar-non-spaces double)])
        (while (not (equal? quote (peek)))
          (set! chunks
                (append chunks
                        (scan-flow-scalar-spaces)))
          (set! chunks
                (append chunks
                        (scan-flow-scalar-non-spaces double))))
        (forward)
        (let ([end-mark (get-mark)])
          (scalar-token start-mark end-mark (list->string chunks) #f style)))))
  
  ;; (: scan-flow-scalar-non-spaces (Boolean -> (Listof Char)))
  (define (scan-flow-scalar-non-spaces double)
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
      (call/cc
       (λ (break)
         (while #t
           (let ([len 0])
             (while (and (char? (peek len))
                         (not (string-index "'\"\\\0 \t\r\n\x85\u2028\u2029"
                                            (peek len))))
               (set! len (+ len 1)))
             (unless (zero? len)
               (set! chunks (append chunks (string->list (prefix len))))
               (forward len))
             (let ([c (peek)] [d (peek 1)])
               (cond
                 [(and (not double)
                       (equal? #\' c)
                       (equal? #\' d))
                  (set! chunks (append chunks (list #\')))
                  (forward 2)]
                 [(and (char? c)
                       (or (and double (equal? #\' c))
                           (and (not double) (or (equal? #\" c)
                                                 (equal? #\\ c)))))
                  (set! chunks (append chunks (list (peek))))
                  (forward)]
                 [(and double (equal? #\\ c))
                  (forward)
                  (set! c (peek))
                  (cond
                    [(and (char? c) (hash-has-key? esc-repls (peek)))
                     (set! chunks
                           (append chunks
                                   (list (hash-ref esc-repls (peek)))))
                     (forward)]
                    [(and (char? c) (hash-has-key? esc-codes (peek)))
                     (let ([len (hash-ref esc-codes (peek))])
                       (forward)
                       (for ([k (in-range len)])
                         (let ([ch (peek k)])
                           (unless (and (char? ch)
                                        (string-index
                                         "01223456789ABCDEFabcdef" ch))
                             (scanner-error
                              "while scanning a double-quoted scalar"
                              (format
                               "expected escape sequence, but found ~a" ch)
                              (get-mark)))))
                       (let ([code (string->number (prefix len) 16)])
                         (set! chunks
                               (append chunks (list (integer->char code))))
                         (forward len)))]
                    [(and (char? (peek))
                          (string-index "\r\n\x85\u2028\u2029" (peek)))
                     (scan-line-break)
                     (set! chunks (append chunks (scan-flow-scalar-breaks)))]
                    [else
                     (scanner-error
                      "while scanning a double-quoted scalar"
                      (format "found unknown escape character ~a" (peek))
                      (get-mark))])]
                 [else (break (void))]))))))
      chunks))
  
  ;; (: scan-flow-scalar-spaces (-> (Listof Char)))
  (define (scan-flow-scalar-spaces)
    ;; See the specification for details.
    (let ([chunks '()] [len 0])
      (while (or (equal? #\space (peek len))
                 (equal? #\tab (peek len)))
        (set! len (add1 len)))
      (let ([whitespaces (prefix len)])
        (forward len)
        (cond 
          [(or (eof-object? (peek)) (equal? #\nul (peek)))
           (scanner-error
            "while scanning a quoted scalar"
            "found unexpected end of stream"
            (get-mark))]
          [(and (char? (peek)) (string-index "\r\n\x85\u2028\u2029" (peek)))
           (let* ([line-break (scan-line-break)]
                  [breaks (scan-flow-scalar-breaks)])
             (cond
               [(not (equal? "\n" line-break))
                (set! chunks (append chunks (string->list line-break)))]
               [(null? breaks)
                (set! chunks (append chunks (list #\space)))])
             (set! chunks (append chunks breaks)))]
          [else
           (set! chunks (string->list whitespaces))])
        chunks)))
  
  ;; (: scan-flow-scalar-breaks (-> (Listof Char)))
  (define (scan-flow-scalar-breaks)
    ;; See the specification for details.
    (let ([chunks '()])
      (call/cc
       (λ (break)
         (while #t
           ;; Instead of checking indentation, we check for document
           ;; separators.
           (let ([pre (prefix 3)])
             (when (and (or (equal? "---" pre)
                            (equal? "..." pre))
                        (or (eof-object? (peek 3))
                            (string-index "\t\r\n\x85\u2028\u2029"
                                          (peek 3))))
               (scanner-error
                "while scanning a quoted scalar"
                "found unexpected document separator"
                (get-mark)))
             (while (and (char? (peek)) (string-index " \t" (peek)))
               (forward))
             (if (and (char? (peek))
                      (string-index "\r\n\x85\u2028\u2029" (peek)))
                 (set! chunks (append chunks (string->list (scan-line-break))))
                 (break (void)))))))
      chunks))
  
  ;; (: scan-plain (-> scalar-token))
  (define (scan-plain)
    ;; See the specification for details.
    ;; We add an additional restriction for the flow context:
    ;;   plain scalars in the flow context cannot contain ',' ':' '?'.
    ;; We also keep track of the `allow-simple-key' flag here.
    (let ([chunks '()]
          [spaces '()]
          [start-mark (get-mark)]
          [end-mark (get-mark)]
          [tmp-indent (add1 indent)])
      (call/cc
       (λ (break)
         (while #t
           (let ([len 0] [ch (peek)])
             (when (equal? #\# ch)
               (break (void)))
             (call/cc
              (λ (break)
                (while #t
                  (set! ch (peek len))
                  (when (or (eof-object? ch)
                            (and (or (string-index
                                      " \t\r\n\x85\u2028\u2029" ch)
                                     (and (zero? flow-level)
                                          (equal? #\: ch)
                                          (or (eof-object?
                                               (peek (add1 len)))
                                              (string-index
                                               " \t\r\n\x85\u2028\u2029"
                                               (peek (add1 len)))))
                                     (and (> flow-level 0)
                                          (string-index ",:?[]{}" ch)))))
                    (break (void)))
                  (set! len (add1 len)))))
             (when (and (> flow-level 0) (equal? #\: ch)
                        (and (char? (peek (add1 len)))
                             (not (string-index
                                   " \t\r\n\x85\u2028\u2029,[]{}"
                                   (peek (add1 len))))))
               (forward len)
               (scanner-error
                "while scanning a plain scalar"
                "found unexpected ':'"
                (get-mark)))
             (when (zero? len)
               (break (void)))
             (set! allow-simple-key #f)
             (set! chunks (append chunks spaces))
             (set! chunks (append chunks (string->list (prefix len))))
             (forward len)
             (set! end-mark (get-mark))
             (set! spaces (scan-plain-spaces))
             (when (or (null? spaces) (equal? #\# (peek))
                       (and (zero? flow-level) (< column tmp-indent)))
               (break (void)))))))
      (scalar-token start-mark end-mark (list->string chunks) #t #f)))
  
  ;; (: scan-plain-spaces (-> (Listof Char)))
  (define (scan-plain-spaces)
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
                        (or (eof-object? (peek 3))
                            (string-index
                             " \t\r\n\x85\u2028\u2029"
                             (peek 3))))
                   '()
                   (let ([breaks '()]
                         [ret #f])
                     (call/cc
                      (λ (break)
                        (while (and (char? (peek))
                                    (string-index
                                     " \r\n\x85\u2028\u2029"
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
                                          (or (eof-object? (peek 3))
                                              (string-index
                                               "\t\r\n\x85\u2028\u2029"
                                               (peek 3))))
                                 (set! ret '())
                                 (break (void))))]))))
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
          [(> (string-length whitespaces) 0)
           (set! chunks (append chunks (string->list whitespaces)))])
        chunks)))
  
  ;; (: scan-tag-handle (String -> String))
  (define (scan-tag-handle name)
    ;; See the specification for details.
    ;; For some strange reason, the specification does not allow '_' in
    ;; tag handles. I have allowed it anyway.
    (unless (equal? #\! (peek))
      (scanner-error
       (format "while scanning a ~a" name)
       (format "expected '!', but found ~a" (peek))
       (get-mark)))
    (let ([len 1])
      (when (and (char? (peek len))
                 (not (equal? #\space (peek len))))
        (while (let ([c (peek len)])
                 (and (char? c)
                      (regexp-match? #rx"[0-9A-Za-z_-]" (string c))))
          (set! len (add1 len)))
        (unless (equal? #\! (peek len))
          (forward len)
          (scanner-error
           (format "while scanning a ~a" name)
           (format "expected '!', but found ~a" (peek))
           (get-mark)))
        (set! len (add1 len)))
      (let ([value (prefix len)])
        (forward len)
        value)))
  
  ;; (: scan-tag-uri (String -> String))
  (define (scan-tag-uri name)
    ;; See the specification for details.
    ;; Note: we do not check if URI is well-formed.
    (let ([chunks '()]
          [len 0]
          [ch (peek)])
      (while (and (char? ch)
                  (or (regexp-match? #"[0-9A-Za-z]" (format "~a" ch))
                      (string-index "-/;?:@&=+$,_.!~*'()[]%" ch)))
        (cond
          [(equal? #\% ch)
           (set! chunks (append chunks (string->list (prefix len))))
           (forward len)
           (set! len 0)
           (set! chunks
                 (append chunks
                         (string->list (scan-uri-escapes name))))]
          [else (set! len (add1 len))])
        (set! ch (peek len)))
      (when (> len 0)
        (set! chunks (append chunks (string->list (prefix len))))
        (forward len)
        (set! len 0))
      (when (null? chunks)
        (scanner-error
         (format "while parsing a ~a" name)
         (format "expected URI, but found ~a" (peek))
         (get-mark)))
      (list->string chunks)))
  
  ;; (: scan-uri-escapes (String -> String))
  (define (scan-uri-escapes name)
    ;; See the specification for details.
    (let ([bytes '()]
          [mark (get-mark)])
      (while (equal? #\% (peek))
        (forward)
        (for ([k (in-range 2)])
          (let ([c (peek k)])
            (unless (and (char? c)
                         (regexp-match? #rx"[0-9A-Za-z]" (string c)))
              (scanner-error
               (format "while scanning a ~a" name)
               (format "expected URI escape, but found ~a" c)
               (get-mark)))))
        (let ([c (string->number (prefix 2) 16)])
          (set! bytes (append bytes (list (integer->char c))))
          (forward 2)))
      (list->string bytes)))
  
  ;; (: scan-line-break (-> String))
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
  
  ;; Turn on line counting.
  (port-count-lines! in)
  
  ;; Add the STREAM-START token.
  (fetch-stream-start)
  (values check-token? peek-token get-token))

(module+ test
  (require rackunit)
  
  (for ([(test-file check-file) (test-files #".scan")])
    (test-case (path->string check-file)
      (for ([token (scan-file test-file)]
            [line (file->lines check-file)])
        (check-equal? (token->string token) line))))

  (test-case "scan-directive-name"
    (check-exn
     #rx"expected alphanumeric character"
     (λ () (scan-string "%! bar baz")))
    (check-exn
     #rx"expected alphanumeric character"
     (λ () (scan-string "%FOO! bar baz"))))

  (test-case "scan-yaml-directive"
    (check-exn
     #rx"expected a digit, but found x"
     (λ () (scan-string "%YAML 1.x")))
    (check-exn
     #rx"expected a digit or '.', but found -"
     (λ () (scan-string "%YAML 1-1")))
    (check-exn
     #rx"expected a digit or ' ', but found x"
     (λ () (scan-string "%YAML 1.1x"))))

  (test-case "scan-tag-directive-value"
    (check-exn
     #rx"expected ' ', but found #<eof>"
     (λ () (scan-string "%TAG !")))
    (check-exn
     #rx"expected ' ', but found >"
     (λ () (scan-string "%TAG ! !>"))))

  (test-case "scan-directive-ignored-line"
    (check-exn
     #rx"expected a comment or a line break"
     (λ () (scan-string "%TAG ! ! !"))))

  (test-case "scan-tag"
    (check-exn
     #rx"expected '>', but found "
     (λ () (scan-string "!<tag:yaml.org,2002:str foo : bar")))
    (check-exn
     #rx"expected ' ', but found f"
     (λ () (scan-string "!<tag:yaml.org,2002:str>foo : bar"))))

  (test-case "scan-tag-handle"
    (check-exn
     #rx"expected '!', but found f"
     (λ () (scan-string "%TAG foo bar")))
    (check-exn
     #rx"expected '!', but found "
     (λ () (scan-string "%TAG !foo bar"))))

  (test-case "scan-tag-uri"
    (check-exn
     #rx"expected URI, but found <"
     (λ () (scan-string "%TAG ! <foo>")))
    (check-exn
     #rx"expected URI escape, but found \\$"
     (λ () (scan-string "%TAG ! foo%$$bar"))))

  (test-case "scan-anchor"
    (check-exn
     #rx"expected alphanumeric character, but found "
     (λ () (scan-string "First occurrence: & Value")))
    (check-exn
     #rx"expected alphanumeric character, but found \\$"
     (λ () (scan-string "First occurrence: &anchor$Value")))))
