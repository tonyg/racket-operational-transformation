#lang racket/base

(provide (struct-out client-state)
         make-client
         incorporate-local-operation
         incorporate-operation-from-server
         flush-buffered-operation)

(require racket/match)
(require "operation.rkt")
(require "server.rkt")

(struct client-state (id ;; *Globally-unique* symbol (randomly generated)
                      local-revisions-applied ;; Natural
                      server-revision ;; Natural
                      operation-in-flight ;; Operation
                      operation-accumulator ;; Operation
                      document ;; Document
                      )
  #:prefab)

(define (make-client snapshot)
  (client-state (make-client-id)
                0
                (server-snapshot-revision snapshot)
                (identity-operation)
                (identity-operation)
                (server-snapshot-document snapshot)))

(define (make-client-id)
  (local-require (only-in racket/random crypto-random-bytes))
  (local-require (only-in file/sha1 bytes->hex-string))
  (string->symbol (string-append "client-" (bytes->hex-string (crypto-random-bytes 8)))))

(define (incorporate-local-operation c op)
  (struct-copy client-state c
               [operation-accumulator
                (compose-operation op (client-state-operation-accumulator c))]
               [document (apply-operation op (client-state-document c))]))

(define (incorporate-operation-from-server c new-op)
  (match-define (pending-operation expected-current-revision op-id op) new-op)
  (match-define (client-state xid xcount current-revision in-flight accumulator doc) c)
  (when (not (equal? current-revision expected-current-revision))
    (error 'incorporate-operation-from-server
           "Out-of-order operation received: rev ~v, op ~v"
           expected-current-revision
           op))
  (if (equal? op-id (list xid xcount))
      (struct-copy client-state c
                   [local-revisions-applied (+ xcount 1)]
                   [server-revision (+ current-revision 1)]
                   [operation-in-flight (identity-operation)])
      (let*-values (((op in-flight) (transform-operation op in-flight))
                    ((op accumulator) (transform-operation op accumulator)))
        (struct-copy client-state c
                     [server-revision (+ current-revision 1)]
                     [operation-in-flight in-flight]
                     [operation-accumulator accumulator]
                     [document (apply-operation op doc #:remote? #t)]))))

(define (flush-buffered-operation c)
  (match-define (client-state xid xcount server-revision in-flight acc _) c)
  (if (or (not (identity-operation? in-flight)) (identity-operation? acc))
      (values #f
              c)
      (let ((p (pending-operation server-revision (list xid xcount) acc)))
        (values p
                (struct-copy client-state c
                             [operation-in-flight acc]
                             [operation-accumulator (identity-operation)])))))

(module+ test
  (require rackunit)
  (require "text.rkt")
  (require "text/simple-document.rkt")
  (require (submod "text/simple-document.rkt" test-data))

  (define s (box #f))
  (define c1 (box #f))
  (define c2 (box #f))

  (define (reset-to-doc! d)
    (set-box! s (make-server d))
    (set-box! c1 (make-client (extract-snapshot (unbox s))))
    (set-box! c2 (make-client (extract-snapshot (unbox s)))))

  (define-syntax-rule (check-client cb expected)
    (check-equal? (simple-document-text (client-state-document (unbox cb))) expected))

  (define-syntax-rule (check-server expected)
    (check-equal? (simple-document-text (server-state-document (unbox s))) expected))

  (define (! b v)
    (printf "~a\n" v)
    (set-box! b v))
  (define (--)
    (printf "----------------------------------------\n")
    (void))

  (define (perform! cb op)
    (! cb (incorporate-local-operation (unbox cb) op)))

  (define (send! cb)
    (define-values (p c) (flush-buffered-operation (unbox cb)))
    (! cb c)
    (when p (! s (incorporate-operation-from-client (unbox s) p))))

  (define (receive! cb)
    (define p (extract-operation (unbox s) (client-state-server-revision (unbox cb))))
    (when p
      (! cb (incorporate-operation-from-server (unbox cb) p))
      (receive! cb)))

  (reset-to-doc! d0)
  (check-server "abcde")
  (check-client c1 "abcde")
  (check-client c2 "abcde")
  (--)

  (perform! c1 i1)
  (check-server "abcde")
  (check-client c1 "abxyzcde")
  (check-client c2 "abcde")
  (--)

  (perform! c1 (insertion 7 "uvw"))
  (send! c1)
  (check-server "abxyzcduvwe")
  (check-client c1 "abxyzcduvwe")
  (check-client c2 "abcde")
  (--)

  (perform! c2 d1)
  (send! c2)
  (check-server "aduvwe")
  (check-client c1 "abxyzcduvwe")
  (check-client c2 "ade")
  (--)

  (receive! c1)
  (receive! c2)
  (check-server "aduvwe")
  (check-client c1 "aduvwe")
  (check-client c2 "aduvwe")
  (--)

  ;;--------------------------------------------------
  (reset-to-doc! (simple-document ""))
  (check-server "")
  (check-client c1 "")
  (check-client c2 "")
  (--)
  (perform! c1 (insertion 0 "q"))
  (perform! c2 (insertion 0 "a"))
  (send! c1)
  (send! c2)
  (perform! c1 (insertion 1 "werty"))
  (perform! c2 (insertion 1 "sdfgh"))
  (check-client c1 "qwerty")
  (check-client c2 "asdfgh")
  (receive! c1)
  (receive! c2)
  (--)
  (send! c1)
  (send! c2)
  (receive! c1)
  (receive! c2)
  (--)
  (check-server "qawertysdfgh")
  (check-client c1 "qawertysdfgh")
  (check-client c2 "qawertysdfgh")
  (--)
  )
