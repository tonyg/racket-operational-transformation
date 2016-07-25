#lang racket/base

(provide (struct-out server-state)
         (struct-out pending-operation)
         (struct-out server-snapshot)
         make-server
         incorporate-operation-from-client
         extract-operation
         extract-snapshot)

(require racket/match)
(require (only-in racket/list split-at))

(require "operation.rkt")

(struct server-state (document revision original-ids operations) #:prefab)
(struct pending-operation (base-revision id op) #:prefab)
(struct server-snapshot (revision document) #:prefab)

(define (make-server initial-document)
  (server-state initial-document 0 '() '()))

(define (incorporate-operation-from-client s p)
  (match-define (pending-operation revision original-id op) p)
  (match-define (server-state doc revision-count original-ids all-operations) s)
  (define-values (ops-after-rev ops-before-rev)
    (split-at all-operations (- revision-count revision)))
  (define-values (ops-after-rev* op*) (transform-operation ops-after-rev op))
  (server-state (apply-operation op* doc)
                (+ revision-count 1)
                (cons original-id original-ids)
                (cons op* (append ops-after-rev* ops-before-rev))))

(define (extract-operation s [rev (- (server-state-revision s) 1)])
  (define current-rev (server-state-revision s))
  (define index (- current-rev rev 1))
  (and (not (negative? index))
       (pending-operation rev
                          (list-ref (server-state-original-ids s) index)
                          (list-ref (server-state-operations s) index))))

(define (extract-snapshot s)
  (server-snapshot (server-state-revision s)
                   (server-state-document s)))

(module+ test
  (require rackunit)
  (require "text/simple-document.rkt")
  (require (submod "text/simple-document.rkt" test-data))

  (define (check-document-equal? s expected)
    (check-equal? (simple-document-text (server-state-document s)) expected))

  (define (do-op s rev id op)
    (incorporate-operation-from-client s (pending-operation rev id op)))

  (let* ((s (make-server d0))
         (s (do-op s 0 'i1 i1))
         (_ (check-document-equal? s "abxyzcde"))
         (s (do-op s 0 'd1 d1))
         (_ (check-document-equal? s "ade"))
         (s (do-op s 2 'i2 i2))
         (_ (check-document-equal? s "adeuvw"))
         (s (do-op s 3 'd2 d2))
         (_ (check-document-equal? s "advw")))
    (void))

  (let* ((s (make-server d0))
         (s (do-op s 0 'd1 d1))
         (_ (check-document-equal? s "ade"))
         (s (do-op s 0 'i1 i1))
         (_ (check-document-equal? s "ade"))
         (s (do-op s 2 'd2 d2))
         (_ (check-document-equal? s "ad"))
         (s (do-op s 3 'i2 i2))
         (_ (check-document-equal? s "aduvw")))
    (void))
  )
