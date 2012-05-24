;;;;;; scanner.rkt - YAML scanner.    -*- Mode: Racket -*-

#lang racket

(require srfi/13) ; String Libraries
(require "tokens.rkt")

(provide (all-defined-out))

(define-syntax-rule (while test body ...)
  (let loop () (when test body ... (loop))))

(define (scanner-error context problem problem-mark)
  (error 'scanner "~a\n~a\n~a" context problem problem-mark))

(struct simple-key (token-number required? index line column mark))

;; Had we reached the end of the stream?
(define done? #f)

;; The number of unclosed '{' and '['. `flow_level == 0' means block
;; context.
(define flow-level 0)

;; List of processed tokens that are not yet emitted.
(define tokens '())

;; Add the STREAM-START token.
(fetch-stream-start)

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
       (#\{ . ,fetch-mapping-start)
       (#\] . ,fetch-flow-sequence-end)
       (#\} . ,fetch-mapping-end)
       (#\, . ,fetch-flow-entry)
       (#\* . ,fetch-alias)
       (#\& . ,fetch-anchor)
       (#\! . ,fetch-tag)
       (#\' . ,fetch-single)
       (#\" . ,fetch-double)
       (#\? . (,check-key? . ,fetch-key))
       (#\| . (,(λ () (not flow-level)) . ,fetch-literal)))))
  (define (check-ch? ch)
    (match (hash-ref ctable ch #f)
      [(? procedure? fetch) fetch]
      [(cons (? procedure? check?) (? procedure? fetch))
       (and (check?) fetch)]
      [(list (cons (? procedure? checks?) (? procedure? fetches)) ...)
       (for/or ([check? checks?] [fetch fetches])
         (and (check?) fetch))]
      [else #f]))
  (scan-to-next-token!)
  (stale-possible-simple-keys!)
  (unwind-indent! column)
  (let ([ch (peek)])
    (cond
     [(eof-obejct? ch) (fetch-stream-end)]
     [(check-ch? ch) ((check-ch? ch))]
     [(check-plain?) (fetch-plain)]
     [else
      (let ([context "while scanning for the next token"]
            [problem "found character ~a that cannot start any token"]
            [problem-mark (get-mark)])
        (scanner-error context problem problem-mark))])))

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
              (> (simple-key-index index-key) 1024))
      (when (simple-key-required? key)
        (let ([context "while scanning a simple key"]
              [problem "could not find expected ':'"]
              [problem-mark (get-mark)])
          (scanner-error context problem problem-mark)))
      (hash-remove! possible-simple-keys level))))

(define (save-possible-simple-key!)
  ;; The next token may start a simple key. We check if it's possible
  ;; and save its position. This function is called for
  ;;   ALIAS, ANCHOR, TAG, SCALAR(flow), '[', and '{'.
  (define required? (and (not flow-level) (= indent column)))
  (when (or allow-simple-key (not required?))
    (error 'scanner "required simple key not allowed"))
  (when allow-simple-key
    (remove-possible-simple-keys!)
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
        (let ([context "while scanning a simple key"]
              [problem "could not find expected ':'"]
              [problem-mark (get-mark)])
          (scanner-error context problem problem-mark)))
      (hash-remove! possible-simple-keys flow-level))))

;;; Indentation functions.

(define (unwind-indent! column)
  ;; In the flow context, indentation is ignored. We make the scanner
  ;; less restrictive than specification requires.
  (unless flow-level
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
  (set! done #t))

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
  (set! allow-simple-key! #t)
  (remove-possible-simple-key!)
  (let ([start-mark (get-mark)])
    (forward)
    (let ([end-mark (get-mark)])
      (add-token! (flow-entry-token start-mark end-mark)))))

(define (fetch-block-entry)
  (unless flow-level
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
  (unless flow-level
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
      (unless flow-level
        (when (add-indent! (simple-key-column key))
          (add-token! (block-mapping-start-token mark mark) i)))
      (set! allow-simple-key #f))]
   [else
    (unless flow-level
      (unless allow-simple-key
        (let ([problem "mapping values are not allowed here"]
              [problem-mark (get-mark)])
          (error 'scanner "~a\n~a" problem problem-mark)))
      (when (add-indent! column)
        (let ([mark (get-mark)])
          (add-token! (block-mapping-start-token mark mark)))))
    (set! allow-simple-key (not flow-level))
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
       (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 3))))

(define (check-document-end?)
  ;; DOCUMENT-END: ^ '...' (' '|'\n')
  (and (zero? column)
       (string=? "..." (prefix 3))
       (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 3))))

(define (check-block-entry?)
  ;; BLOCK-ENTRY: '-' (' '|'\n')
  (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1)))

(define (check-key?)
  ;; KEY(flow context): '?'
  ;; KEY(block context): '?' (' '|'\n')
  (or flow-level
      (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1))))

(define (check-value?)
  ;; VALUE(flow context): ':'
  ;; VALUE(block context): ':' (' '|'\n')
  (or flow-level
      (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1))))

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
  (let ([ch (peek)])
    (or (not (string-index "\0 \t\r\n\x85\u2028\u2029-?:,[]{}#&*!|>'\"%@" ch))
        (and (not (string-index "\0 \t\r\n\x85\u2028\u2029" (peek 1)))
             (or (char=? #\- ch)
                 (and (not flow-level)
                      (string-index "?:" ch)))))))

;; Scanners.
      
