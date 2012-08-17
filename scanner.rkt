;;;;;; scanner.rkt - YAML scanner.    -*- Mode: Racket -*-

#lang typed/racket/no-check

(require/typed srfi/13
  [string-index (String Char -> (Option Integer))])
(require "tokens.rkt" "utils.rkt")

(provide scan-file scan-string scan make-scanner)

(define-syntax-rule (while test body ...)
  (let: loop : Void ()
    (if test
        (begin body ... (loop))
        (void))))

(: string-index? (String (U Char EOF) -> Boolean))
(define (string-index? str ch)
  (and (char? ch)
       (integer? (string-index str ch))))

(: scan-file (String -> (Listof token)))
(define (scan-file filename)
  (with-input-from-file filename
    (λ () (scan filename))))

(: scan-string (String -> (Listof token)))
(define (scan-string string)
  (with-input-from-string string
    (λ () (scan "<string>"))))

(: scan
   (case->
    (-> (Listof token))
    (String -> (Listof token))
    (String Input-Port -> (Listof token))))
(define (scan [name "<input>"] [in (current-input-port)])
  (define-values (check-token? peek-token get-token)
    (make-scanner name in))
  (let loop ([ts (ann '() (Listof token))])
    (let ([t (peek-token)])
      (if (token? t)
          (begin
            (get-token)
            (loop (cons t ts)))
          (reverse ts)))))

(: scanner-error ((Option String) String mark -> Void))
(define scanner-error (make-error 'scanner))

(struct: simple-key
  ([token-number : Integer]
   [required? : Boolean]
   [index : Integer]
   [line : Integer]
   [column : Integer]
   [mark : mark]))

(: make-scanner
   (case-> (-> (Values ((Any -> Boolean) * -> Boolean)
                       (-> (Option token))
                       (-> (Option token))))
           (String -> (Values ((Any -> Boolean) * -> Boolean)
                              (-> (Option token))
                              (-> (Option token))))
           (String Input-Port -> (Values ((Any -> Boolean) * -> Boolean)
                                         (-> (Option token))
                                         (-> (Option token))))))
(define (make-scanner [name "<input>"] [in (current-input-port)])
  (define line 0)
  (define column 0)
  (define index 0)
  (define buffer-length 0)
  (: buffer (Vectorof (U Char EOF)))
  (define buffer (make-vector 1024 #\nul))

  (: peek (case-> (-> (U Char EOF)) (Integer -> (U Char EOF))))
  (define (peek [i 0])
    ;; peek the next i-th character
    (when (>= (+ i index) (vector-length buffer))
      (let: ([new-buffer : (Vectorof (U Char EOF))
                         (make-vector (* (vector-length buffer) 2) #\nul)])
        (vector-copy! new-buffer 0 buffer)
        (set! buffer new-buffer)))
    (when (>= (+ index i) buffer-length)
      (for ([j (in-range buffer-length (+ index i 1))])
        (vector-set! buffer j (read-char in))
        (set! buffer-length (add1 buffer-length))))
    (vector-ref buffer (+ i index)))

  (: prefix (case-> (-> String) (Integer -> String)))
  (define (prefix [l 1])
    ;; peek the next l characters
    (let loop ([i 0] [cs (ann '() (Listof Char))])
      (if (= i l)
          (list->string (reverse cs))
          (let ([c (peek i)])
            (if (char? c)
                (loop (+ i 1) (cons c cs))
                (loop (+ i 1) cs))))))
  
  (: forward (case-> (-> Void) (Integer -> Void)))
  (define (forward [l 1])
    ;; read the next l characters and move the index
    (let ([tmp-index index])
      (for ([i (in-range l)])
        (when (char? (peek i))
          (set! tmp-index (add1 tmp-index))
          (cond
           [(or (string-index? "\n\x85\u2028\u2029" (peek i))
                (and (equal? #\return (peek i))
                     (not (equal? #\newline (peek (add1 i))))))
            (set! line (add1 line))
            (set! column 0)]
           [(not (equal? #\uFEFF (peek i)))
            (set! column (add1 column))])))
      (set! index tmp-index)))

  (: get-mark (-> mark))
  (define (get-mark)
    (mark name index line column buffer))

  (: add-token! (case-> (token -> Void) (token (Option Integer) -> Void)))
  (define (add-token! token [i #f])
    (if (number? i)
        (let-values ([(left right) (split-at tokens i)])
          (set! tokens (append left (cons token right))))
        (set! tokens (append tokens (list token)))))

  (: done? Boolean)
  (define done? #f)
  
  (: flow-level Integer)
  (define flow-level 0)
  
  (: tokens (Listof token))
  (define tokens '())

  (: tokens-taken Integer)
  (define tokens-taken 0)

  (: indent Integer)
  (define indent -1)

  (: indents (Listof Integer))
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
  (: allow-simple-key Boolean)
  (define allow-simple-key #t)
  
  ;; Keep track of possible simple keys. This is a dictionary. The key
  ;; is `flow_level'; there can be no more than one possible simple key
  ;; for each level. The value is a SIMPLE-KEY record:
  ;;   (token-number, required, index, line, column, mark)
  ;; A simple key may start with ALIAS, ANCHOR, TAG, SCALAR(flow),
  ;; '[' or '{' tokens.
  (: possible-simple-keys (HashTable Integer simple-key))
  (define possible-simple-keys (make-hash))
  
  ;;; Public methods.

  (: check-token? ((Any -> Boolean) * -> Boolean))
  (define (check-token? . choices)
    ;; Check if the next token is one of the given types.
    (while (need-more-tokens?)
      (fetch-more-tokens))
    (and (not (null? tokens))
         (or (null? choices)
             (and (list? choices)
                  (ormap (λ: ([c? : (Any -> Boolean)])
                           (c? (car tokens)))
                         choices)))))

  (: peek-token (-> (Option token)))
  (define (peek-token)
    ;; Return the next token, but do not delete if from the queue.
    (while (need-more-tokens?)
      (fetch-more-tokens))
    (and (not (null? tokens))
         (car tokens)))

  (: get-token (-> (Option token)))
  (define (get-token)
    ;; Return the next token.
    (while (need-more-tokens?)
      (fetch-more-tokens))
    (and (not (null? tokens))
         (begin0 (car tokens)
           (set! tokens (cdr tokens))
           (set! tokens-taken (add1 tokens-taken)))))

  ;;; Private methods.

  (: need-more-tokens? (-> Boolean))
  (define (need-more-tokens?)
    (and (not done?)
         (or (null? tokens)
             ;; The current token may be a potential simple key, so we
             ;; need to look further.
             (begin
               (stale-possible-simple-keys!)
               (equal? (next-possible-simple-key) tokens-taken)))))

  (: fetch-more-tokens (-> Void))
  (define (fetch-more-tokens)
    (: ctable
       (HashTable
        Char (U (-> Void) (Listof (Pairof (-> Boolean) (-> Void))))))
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
    (: check-ch? (Char -> (Option (-> Void))))
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

  (: next-possible-simple-key (-> (Option Integer)))
  (define (next-possible-simple-key)
    ;; Return the number of the nearest possible simple key.
    (and (hash? possible-simple-keys)
         (not (null? (hash-keys possible-simple-keys)))
         (simple-key-token-number
          (hash-ref possible-simple-keys
                    (apply min (hash-keys possible-simple-keys))))))

  (: stale-possible-simple-keys! (-> Void))
  (define (stale-possible-simple-keys!)
    ;; Remove entries that are no longer possible simple keys. According
    ;; to the YAML specification, simple keys
    ;; - should be limited to a single line.
    ;; - should be no longer than 1024 characters.
    ;; Disabling this procedure will allow simple keys of any length and
    ;; height (may cause problems if indentation is broken though).
    (hash-for-each
     possible-simple-keys
     (λ: ([level : Integer] [key : simple-key])
       (when (or (not (= (simple-key-line key) line))
                 (> (- index (simple-key-index key)) 1024))
         (when (simple-key-required? key)
           (scanner-error
            "while scanning a simple key"
            "could not find expected ':'"
            (get-mark)))
         (hash-remove! possible-simple-keys level)))))

  (: save-possible-simple-key! (-> Void))
  (define (save-possible-simple-key!)
    ;; The next token may start a simple key. We check if it's possible
    ;; and save its position. This function is called for
    ;;   ALIAS, ANCHOR, TAG, SCALAR(flow), '[', and '{'.
    (define required? (and (zero? flow-level) (= indent column)))
    (unless (or allow-simple-key (not required?))
      (error 'scanner "required simple key not allowed"))
    (when allow-simple-key
      (remove-possible-simple-key!)
      (let ([token-number (+ tokens-taken (length tokens))])
        (hash-set!
         possible-simple-keys
         flow-level
         (simple-key token-number required? index line column (get-mark))))))

  (: remove-possible-simple-key! (-> Void))
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

  (: unwind-indent! (Integer -> Void))
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

  (: add-indent! (Integer -> Boolean))
  (define (add-indent! column)
    ;; Check if we need to increase indentation.
    (and (< indent column)
         (begin0 #t
           (set! indents (cons indent indents))
           (set! indent column))))

  ;;; Fetchers.

  (: fetch-stream-start (-> Void))
  (define (fetch-stream-start)
    (let ([mark (get-mark)])
      (add-token! (stream-start-token mark mark))))

  (: fetch-stream-end (-> Void))
  (define (fetch-stream-end)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (set! possible-simple-keys
          (ann (make-hash) (HashTable Integer simple-key)))
    (let ([mark (get-mark)])
      (add-token! (stream-end-token mark mark)))
    (set! done? #t))

  (: fetch-directive (-> Void))
  (define (fetch-directive)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-directive)))

  (: fetch-document-start (-> Void))
  (define (fetch-document-start)
    (fetch-document-indicator document-start-token))

  (: fetch-document-end (-> Void))
  (define (fetch-document-end)
    (fetch-document-indicator document-end-token))

  (: fetch-document-indicator ((mark mark -> token) -> Void))
  (define (fetch-document-indicator token)
    (unwind-indent! -1)
    (remove-possible-simple-key!)
    (set! allow-simple-key #f)
    (let ([start-mark (get-mark)])
      (forward 3)
      (let ([end-mark (get-mark)])
        (add-token! (token start-mark end-mark)))))

  (: fetch-flow-sequence-start (-> Void))
  (define (fetch-flow-sequence-start)
    (fetch-flow-collection-start flow-sequence-start-token))

  (: fetch-flow-mapping-start (-> Void))
  (define (fetch-flow-mapping-start)
    (fetch-flow-collection-start flow-mapping-start-token))

  (: fetch-flow-collection-start ((mark mark -> token) -> Void))
  (define (fetch-flow-collection-start token)
    (save-possible-simple-key!)
    (set! flow-level (add1 flow-level))
    (set! allow-simple-key #t)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (token start-mark end-mark)))))

  (: fetch-flow-sequence-end (-> Void))
  (define (fetch-flow-sequence-end)
    (fetch-flow-collection-end flow-sequence-end-token))

  (: fetch-flow-mapping-end (-> Void))
  (define (fetch-flow-mapping-end)
    (fetch-flow-collection-end flow-mapping-end-token))

  (: fetch-flow-collection-end ((mark mark -> token) -> Void))
  (define (fetch-flow-collection-end token)
    (remove-possible-simple-key!)
    (set! flow-level (sub1 flow-level))
    (set! allow-simple-key #f)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (token start-mark end-mark)))))

  (: fetch-flow-entry (-> Void))
  (define (fetch-flow-entry)
    (set! allow-simple-key #t)
    (remove-possible-simple-key!)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (flow-entry-token start-mark end-mark)))))

  (: fetch-block-entry (-> Void))
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

  (: fetch-key (-> Void))
  (define (fetch-key)
    (when (zero? flow-level)
      (unless allow-simple-key
        (let ([problem "mapping keys are not allowed here"]
              [problem-mark (get-mark)])
          (error 'scanner "~a\n~a" problem problem-mark)))
      (when (add-indent! column)
        (let ([mark (get-mark)])
          (add-token! (block-mapping-start-token mark mark)))))
    (set! allow-simple-key (zero? flow-level))
    (remove-possible-simple-key!)
    (let ([start-mark (get-mark)])
      (forward)
      (let ([end-mark (get-mark)])
        (add-token! (key-token start-mark end-mark)))))

  (: fetch-value (-> Void))
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

  (: fetch-alias (-> Void))
  (define (fetch-alias)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-anchor alias-token)))

  (: fetch-anchor (-> Void))
  (define (fetch-anchor)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-anchor anchor-token)))

  (: fetch-tag (-> Void))
  (define (fetch-tag)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-tag)))

  (: fetch-literal (-> Void))
  (define (fetch-literal)
    (fetch-block-scalar #\|))

  (: fetch-folded (-> Void))
  (define (fetch-folded)
    (fetch-block-scalar #\>))

  (: fetch-block-scalar ((Option Char) -> Void))
  (define (fetch-block-scalar style)
    (set! allow-simple-key #t)
    (remove-possible-simple-key!)
    (add-token! (scan-block-scalar style)))

  (: fetch-single (-> Void))
  (define (fetch-single)
    (fetch-flow-scalar #\'))

  (: fetch-double (-> Void))
  (define (fetch-double)
    (fetch-flow-scalar #\"))

  (: fetch-flow-scalar ((Option Char) -> Void))
  (define (fetch-flow-scalar style)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-flow-scalar style)))

  (: fetch-plain (-> Void))
  (define (fetch-plain)
    (save-possible-simple-key!)
    (set! allow-simple-key #f)
    (add-token! (scan-plain)))

  ;;; Checkers.

  (: check-directive? (-> Boolean))
  (define (check-directive?)
    ;; DIRECTIVE: ^ '%' ...
    ;; The '%' indicator is already checked.
    (zero? column))

  (: check-document-start? (-> Boolean))
  (define (check-document-start?)
    ;; DOCUMENT-START: ^ '---' (' '|'\n')
    (and (zero? column)
         (string=? "---" (prefix 3))
         (string-index? "\0 \t\r\n\x85\u2028\u2029" (peek 3))))

  (: check-document-end? (-> Boolean))
  (define (check-document-end?)
    ;; DOCUMENT-END: ^ '...' (' '|'\n')
    (and (zero? column)
         (string=? "..." (prefix 3))
         (string-index? "\0 \t\r\n\x85\u2028\u2029" (peek 3))))

  (: check-block-entry? (-> Boolean))
  (define (check-block-entry?)
    ;; BLOCK-ENTRY: '-' (' '|'\n')
    (string-index? "\0 \t\r\n\x85\u2028\u2029" (peek 1)))

  (: check-key? (-> Boolean))
  (define (check-key?)
    ;; KEY(flow context): '?'
    ;; KEY(block context): '?' (' '|'\n')
    (or (not (zero? flow-level))
        (string-index? "\0 \t\r\n\x85\u2028\u2029" (peek 1))))

  (: check-value? (-> Boolean))
  (define (check-value?)
    ;; VALUE(flow context): ':'
    ;; VALUE(block context): ':' (' '|'\n')
    (or (not (zero? flow-level))
        (string-index? "\0 \t\r\n\x85\u2028\u2029" (peek 1))))

  (: check-plain? (-> Boolean))
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
             (not (string-index?
                   "\0 \t\r\n\x85\u2028\u2029-?:,[]{}#&*!|>'\"%@"
                   (peek))))
        (and (not (eof-object? (peek)))
             (not (string-index? "\0 \t\r\n\x85\u2028\u2029" (peek 1)))
             (or (equal? #\- (peek))
                 (and (zero? flow-level)
                      (string-index? "?:" (peek)))))))

  ;; Scanners.

  (: scan-to-next-token (-> Void))
  (define (scan-to-next-token)
    ;; We ignore spaces, line breaks and comments.
    ;; If we find a line break in the block section, we set the flag
    ;; `allow_simple_key' on.
    (when (and (zero? index) (equal? #\uFEFF (peek)))
      (forward))
    (let ([found (ann #f Boolean)])
      (while (not found)
        (while (equal? #\space (peek))
          (forward))
        (when (equal? #\# (peek))
          (while (and (not (eof-object? (peek)))
                      (not (string-index? "\0\r\n\x85\u2028\u2029" (peek))))
            (forward)))
        (if (> (string-length (scan-line-break)) 0)
            (when (zero? flow-level)
              (set! allow-simple-key #t))
            (set! found #t)))))

  (: scan-directive (-> directive-token))
  (define (scan-directive)
    ;; See the specification for details.
    (let ([start-mark (get-mark)])
      (forward)
      (let ([name (scan-directive-name)]
            [value #f] [end-mark #f])
        (cond
         [(string=? "YAML" name)
          (set! value (scan-yaml-directive-value))
          (set! end-mark (get-mark))]
         [(string=? "TAG" name)
          (set! value (scan-tag-directive-value))
          (set! end-mark (get-mark))]
         [else
          (set! end-mark (get-mark))
          (while (and (not (eof-object? (peek)))
                      (not (string-index? "\0\r\n\x85\u2028\u2029" (peek))))
            (forward))])
        (scan-directive-ignored-line)
        (directive-token start-mark end-mark name value))))

  (: scan-directive-name (-> String))
  (define (scan-directive-name)
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
                    (string-index? "\0 \r\n\x85\u2028\u2029" (peek)))
          (scanner-error
           "while scanning a directive"
           (format "expected alphanumeric character, but found ~a" (peek))
           (get-mark)))
        value)))

  (: scan-yaml-directive-value (-> (Pairof Integer Integer)))
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
                    (string-index? "\0 \r\n\x85\u2028\u2029" (peek)))
          (scanner-error
           "while scanning a directive"
           (format "expected a diit or ' ', but found ~a" (peek))
           (get-mark)))
        (cons major minor))))

  (: scan-yaml-directive-number (-> Integer))
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
      (let ([value (string->number (prefix len))])
        (forward len)
        value)))

  (: scan-tag-directive-value (-> (Pairof String String)))
  (define (scan-tag-directive-value)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (let ([handle (scan-tag-directive-handle)])
      (while (equal? #\space (peek))
        (forward))
      (cons handle (scan-tag-directive-prefix))))

  (: scan-tag-directive-handle (-> String))
  (define (scan-tag-directive-handle)
    ;; See the specification for details.
    (let ([value (scan-tag-handle "directive")])
      (unless (equal? #\space (peek))
        (scanner-error
         "while scanning a directive"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      value))

  (: scan-tag-directive-prefix (-> String))
  (define (scan-tag-directive-prefix)
    ;; See the specification for details.
    (let ([value (scan-tag-uri "directive")])
      (unless (or (eof-object? (peek))
                  (string-index? "\0 \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a directive"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      value))

  (: scan-directive-ignored-line (-> String))
  (define (scan-directive-ignored-line)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (when (equal? #\# (peek))
      (while (and (not (eof-object? (peek)))
                  (not (string-index? "\0 \r\n\x85\u2028\u2029" (peek))))
        (forward)))
    (unless (or (eof-object? (peek))
                (string-index? "\0\r\n\x85\u2028\u2029" (peek)))
      (scanner-error
       "while scanning a directive"
       (format "expected a comment or a line break, but found ~a" (peek))
       (get-mark)))
    (scan-line-break))

  (: scan-anchor
     ((mark mark String -> token) -> (U alias-token anchor-token)))
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
                      (string-index?
                       "\0 \t\r\n\x85\u2028\u2029?:,]}%@`"
                       (peek)))
            (scanner-error
             (format "while scanning an ~a" name)
             (format "expected alphanumeric character, but found ~a" (peek))
             (get-mark)))
          (let ([end-mark (get-mark)])
            (token start-mark end-mark value))))))

  (: scan-tag (-> tag-token))
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
            (string-index? "\0 \t\r\n\x85\u2028\u2029" (peek 1)))
        (set! suffix "!")
        (forward)]
       [else
        (let ([len 1] [use-handle #f])
          (call/cc
           (λ: ([break : (Any -> Any)])
             (while (and (not (eof-object? (peek len)))
                         (not (string-index?
                               "\0 \t\r\n\x85\u2028\u2029"
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
                  (string-index? "\0 \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a tag"
         (format "expected ' ', but found ~a" (peek))
         (get-mark)))
      (tag-token start-mark (get-mark) (cons handle suffix))))

  (: scan-block-scalar ((Option Char) -> scalar-token))
  (define (scan-block-scalar style)
    ;; See the specification for details.
    (let ([folded (equal? style #\>)]
          [chunks (ann '() (Listof Char))] [breaks #f] [line-break ""]
          [start-mark (get-mark)] [end-mark (ann #f (Option mark))])
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
         (λ: ([break : (Any -> Any)])
           (while (and (= column tmp-indent)
                       (char? (peek))
                       (not (char=? #\nul (peek))))
             (set! chunks (append chunks breaks))
             (let ([leading-non-space (not (string-index? " \t" (peek)))]
                   [len 0])
               (while (and (not (eof-object? (peek len)))
                           (not (string-index?
                                 "\0\r\n\x85\u2028\u2029"
                                 (peek len))))
                 (set! len (+ len 1)))
               (set! chunks (append chunks (string->list (prefix len))))
               (forward len)
               (set! line-break (scan-line-break))
               (let ([be (scan-block-scalar-breaks tmp-indent)])
                 (set! breaks (car be))
                 (set! end-mark (cdr be))
                 (if (and (= column tmp-indent)
                          (char? (peek))
                          (not (char=? #\nul (peek))))
                     (begin ;; Unfortunately, folding rules are ambiguous.
                       (if (and folded leading-non-space
                                (equal? "\n" line-break)
                                (not (string-index? " \t" (peek))))
                           (when (null? breaks)
                             (set! chunks (append chunks '(#\space))))
                           (set! chunks
                                 (append chunks (string->list line-break)))))
                     (break (void))))))))
        (when (not (eq? #f chomping))
          (set! chunks (append chunks (string->list line-break))))
        (when (eq? #t chomping)
          (set! chunks (append chunks breaks)))
        (unless (mark? end-mark)
          (set! end-mark start-mark))
        (scalar-token start-mark end-mark (list->string chunks) #f style))))

  (: scan-block-scalar-indicators
     (-> (Pairof (U Boolean 'None) (Option Integer))))
  (define (scan-block-scalar-indicators)
    ;; See the specification for details.
    (let ([chomping (ann 'None (U Boolean 'None))]
          [increment (ann #f (Option Integer))])
      (cond
       [(or (equal? #\+ (peek)) (equal? #\- (peek)))
        (set! chomping (equal? #\+ (peek)))
        (forward)
        (let ([c (peek)])
          (when (and (char? c)
                     (string-index? "0123456789" (peek)))
            (let ([inc (string->number (string c))])
              (when (and (integer? inc) (not (flonum? inc)))
                (set! increment inc)))
            (when (zero? increment)
              (scanner-error
               "while scanning a block scalar"
               "expected indentation indicator (1-9), but found 0"
               (get-mark)))
            (forward)))]
       [(string-index? "0123456789" (peek))
        (let ([c (peek)])
          (when (char? c)
            (let ([inc (string->number (string c))])
              (when (and (integer? inc) (not (flonum? inc)))
                (set! increment inc)))))
        (when (and (integer? increment) (zero? increment))
          (scanner-error
           "while scanning a block scalar"
           "expected indentation indicator (1-9), but found 0"
           (get-mark)))
        (forward)
        (when (or (equal? #\+ (peek)) (equal? #\- (peek)))
          (set! chomping (equal? #\+ (peek)))
          (forward))])
      (unless (or (eof-object? (peek))
                  (string-index? "\0 \r\n\x85\u2028\u2029" (peek)))
        (scanner-error
         "while scanning a block scalar"
         (format "expected chomping or indentation indicators, but found ~a"
                 (peek))
         (get-mark)))
      (cons chomping increment)))

  (: scan-block-scalar-ignored-line (-> String))
  (define (scan-block-scalar-ignored-line)
    ;; See the specification for details.
    (while (equal? #\space (peek))
      (forward))
    (when (equal? #\# (peek))
      (while (and (not (eof-object? (peek)))
                  (not (string-index? "\0\r\n\x85\u2028\u2029" (peek))))
        (forward)))
    (unless (or (eof-object? (peek))
                (string-index? "\0\r\n\x85\u2028\u2029" (peek)))
      (scanner-error
       "while scanning a block scalar"
       (format "expected a comment or a line break, but found ~a" (peek))
       (get-mark)))
    (scan-line-break))

  (: scan-block-scalar-indentation
     (-> (List (Listof Char) Integer mark)))
  (define (scan-block-scalar-indentation)
    ;; See the specification for details.
    (let ([chunks (ann '() (Listof Char))]
          [max-indent 0]
          [end-mark (get-mark)])
      (while (and (not (eof-object? (peek)))
                  (string-index? " \r\n\x85\u2028\u2029" (peek)))
        (cond
         [(not (equal? #\space (peek)))
          (set! chunks (append chunks (string->list (scan-line-break))))
          (set! end-mark (get-mark))]
         [else
          (forward)
          (when (> column max-indent)
            (set! max-indent column))]))
      (list chunks max-indent end-mark)))

  (: scan-block-scalar-breaks
     (Integer -> (Pairof (Listof Char) (Option mark))))
  (define (scan-block-scalar-breaks indent)
    ;; See the specification for details.
    (let ([chunks (ann '() (Listof Char))]
          [max-indent 0]
          [end-mark (ann #f (Option mark))])
      (while (and (< column indent)
                  (equal? #\space (peek)))
        (forward))
      (while (and (not (eof-object? (peek)))
                  (string-index? "\r\n\x85\u2028\u2029" (peek)))
        (set! chunks (append chunks (string->list (scan-line-break))))
        (set! end-mark (get-mark))
        (while (and (< column indent)
                    (equal? #\space (peek)))
          (forward)))
      (cons chunks end-mark)))

  (: scan-flow-scalar ((Option Char) -> scalar-token))
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

  (: scan-flow-scalar-non-spaces (Boolean -> (Listof Char)))
  (define (scan-flow-scalar-non-spaces double)
    ;; See the specification for details.
    (: esc-repls (HashTable Char Char))
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
    (: esc-codes (HashTable Char Integer))
    (define esc-codes #hash((#\x . 2) (#\u . 4) (#\U . 8)))
    (let ([chunks (ann '() (Listof Char))])
      (call/cc
       (λ: ([break : (Any -> Any)])
         (while #t
           (let ([len 0])
             (while (and (char? (peek len))
                         (not (string-index? "'\"\\\0 \t\r\n\x85\u2028\u2029"
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
                 (set! chunks (append chunks (list c)))
                 (forward)]
                [(and double (equal? #\\ c))
                 (forward)
                 (set! c (peek))
                 (cond
                  [(and (char? c) (hash-has-key? esc-repls c))
                   (set! chunks (append chunks
                                        (list (hash-ref esc-repls c))))
                   (forward)]
                  [(and (char? c) (hash-has-key? esc-codes c))
                   (let ([len (hash-ref esc-codes c)])
                     (forward)
                     (for ([k (in-range len)])
                       (let ([ch (peek k)])
                         (unless (and (char? ch)
                                      (string-index?
                                       "01223456789ABCDEFabcdef" ch))
                           (scanner-error
                            "while scanning a double-quoted scalar"
                            (format
                             "expected escape sequence, but found ~a" ch)
                            (get-mark)))))
                     (let ([code (string->number (prefix len) 16)])
                       (when (and (integer? code) (not (flonum? code)))
                         (set! chunks
                               (append chunks (list (integer->char code)))))
                       (forward len)))]
                  [(string-index? "\r\n\x85\u2028\u2029" (peek))
                   (scan-line-break)
                   (set! chunks (append chunks (scan-flow-scalar-breaks)))]
                  [else
                   (scanner-error
                    "while scanning a double-quoted scalar"
                    (format "found unknown escape character ~a" (peek))
                    (get-mark))])]
                [else (break (void))]))))))
      chunks))

  (: scan-flow-scalar-spaces (-> (Listof Char)))
  (define (scan-flow-scalar-spaces)
    ;; See the specification for details.
    (let ([chunks (ann '() (Listof Char))] [len 0])
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
         [(string-index? "\r\n\x85\u2028\u2029" (peek))
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

  (: scan-flow-scalar-breaks (-> (Listof Char)))
  (define (scan-flow-scalar-breaks)
    ;; See the specification for details.
    (let ([chunks (ann '() (Listof Char))])
      (call/cc
       (λ: ([break : (Any -> Any)])
         (while #t
           ;; Instead of checking indentation, we check for document
           ;; separators.
           (let ([pre (prefix 3)])
             (when (and (or (equal? "---" pre)
                            (equal? "..." pre))
                        (char? (peek 3))
                        (string-index? "\0 \t\r\n\x85\u2028\u2029" (peek 3)))
               (scanner-error
                "while scanning a quoted scalar"
                "found unexpected document separator"
                (get-mark)))
             (while (and (char? (peek)) (string-index? " \t" (peek)))
               (forward))
             (if (and (char? (peek))
                      (string-index? "\r\n\x85\u2028\u2029" (peek)))
                 (set! chunks (append chunks (string->list (scan-line-break))))
                 (break (void)))))))
      chunks))

  (: scan-plain (-> scalar-token))
  (define (scan-plain)
    ;; See the specification for details.
    ;; We add an additional restriction for the flow context:
    ;;   plain scalars in the flow context cannot contain ',' ':' '?'.
    ;; We also keep track of the `allow-simple-key' flag here.
    (let ([chunks (ann '() (Listof Char))]
          [spaces (ann '() (Listof Char))]
          [start-mark (get-mark)]
          [end-mark (get-mark)]
          [tmp-indent (add1 indent)])
      (call/cc
       (λ: ([break : (Any -> Any)])
         (while #t
           (let ([len 0] [ch (peek)])
             (when (equal? #\# ch)
               (break (void)))
             (call/cc
              (λ: ([break : (Any -> Any)])
                (while #t
                  (set! ch (peek len))
                  (when (or (eof-object? ch)
                            (and (or (string-index?
                                      "\0 \t\r\n\x85\u2028\u2029" ch)
                                     (and (zero? flow-level) (equal? #\: ch)
                                          (string-index?
                                           "\0 \t\r\n\x85\u2028\u2029"
                                           (peek (add1 len))))
                                     (and (> flow-level 0)
                                          (string-index? ",:?[]{}" ch)))))
                    (break (void)))
                  (set! len (add1 len)))))
             (when (and (> flow-level 0) (equal? #\: ch)
                        (or (eof-object? (peek (add1 len)))
                            (not (string-index?
                                  "\0 \t\r\n\x85\u2028\u2029,[]{}"
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

  (: scan-plain-spaces (-> (Listof Char)))
  (define (scan-plain-spaces)
    ;; See the specification for details.
    ;; The specification is really confusing about tabs in plain scalars.
    ;; We just forbid them completely. Do not use tabs in YAML!
    (let ([chunks (ann '() (Listof Char))] [len 0])
      (while (equal? #\space (peek len))
        (set! len (add1 len)))
      (let ([whitespaces (prefix len)])
        (forward len)
        (cond
         [(and (char? (peek))
               (string-index? "\r\n\x85\u2028\u2029" (peek)))
          (let ([line-break (scan-line-break)])
            (set! allow-simple-key #t)
            (let ([pre (prefix 3)])
              (if (and (or (equal? "---" pre)
                           (equal? "..." pre))
                       (and (char? (peek 3))
                            (string-index?
                             "\0 \t\r\n\x85\u2028\u2029"
                             (peek 3))))
                  '()
                  (let ([breaks (ann '() (Listof Char))]
                        [ret (ann #f (Option Null))])
                    (call/cc
                     (λ: ([break : (Any -> Any)])
                       (while (and (char? (peek))
                                   (string-index?
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
                                        (char? (peek 3))
                                        (string-index?
                                         "\0 \t\r\n\x85\u2028\u2029"
                                         (peek 3)))
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

  (: scan-tag-handle (String -> String))
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
           (format "expected '!', but found !a" (peek))
           (get-mark)))
        (set! len (add1 len)))
      (let ([value (prefix len)])
        (forward len)
        value)))

  (: scan-tag-uri (String -> String))
  (define (scan-tag-uri name)
    ;; See the specification for details.
    ;; Note: we do not check if URI is well-formed.
    (let ([chunks (ann '() (Listof Char))]
          [len 0]
          [ch (peek)])
      (while (and (char? ch)
                  (or (regexp-match? #"[0-9A-Za-z]" (string ch))
                      (string-index? "-/;?:@&=+$,_.!~*'()[]%" ch)))
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

  (: scan-uri-escapes (String -> String))
  (define (scan-uri-escapes name)
    ;; See the specification for details.
    (let ([bytes (ann '() (Listof Char))]
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
          (and (integer? c) (not (flonum? c))
               (set! bytes (append bytes (list (integer->char c)))))
          (forward 2)))
      (list->string bytes)))

  (: scan-line-break (-> String))
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
             (string-index? "\r\n\x85" ch))
        (if (equal? "\r\n" (prefix 2))
            (forward 2)
            (forward))
        "\n"]
       [(and (char? ch)
             (string-index? "\u2028\u2029" ch))
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
  (define-simple-check (check-scanner test-file check-file)
    (for ([token (scan-file test-file)]
          [line (read-file check-file)])
      (check-equal? (token->string token) line)))
  (test-begin
   (for ([(test-file check-file) (test-files #"scan")])
     (check-scanner test-file check-file))))
