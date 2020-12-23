#lang racket/base

(provide (struct-out server-state)
         (struct-out pending-operation)
         (struct-out server-snapshot)
         make-server
         incorporate-operation-from-client
         extract-operation
         extract-snapshot
         forget-operation-history)

(require racket/match)
(require (only-in racket/list take))

(require "operation.rkt")

(struct server-state (document revision original-ids operations) #:prefab)
(struct pending-operation (base-revision id op) #:prefab)
(struct server-snapshot (revision document) #:prefab)

(define (make-server initial-document)
  (server-state initial-document 0 '() '()))

(define (incorporate-operation-from-client s p)
  (match-define (pending-operation revision original-id op) p)
  (match-define (server-state doc newest-revision original-ids all-operations) s)
  (define index (- newest-revision revision))
  (cond [(or (negative? index) (> index (length all-operations)))
         #f] ;; operation revision out of range
        [else
         (define ops-after-rev (take all-operations index))
         (define-values (_ops-after-rev* op*) (transform-operation ops-after-rev op))
         (server-state (apply-operation op* doc)
                       (+ newest-revision 1)
                       (cons original-id original-ids)
                       (cons op* all-operations))]))

(define (extract-operation s [rev (- (server-state-revision s) 1)])
  (match-define (server-state _ newest-revision original-ids all-operations) s)
  (define index (- newest-revision rev 1))
  (and (not (or (negative? index) (>= index (length all-operations))))
       (pending-operation rev (list-ref original-ids index) (list-ref all-operations index))))

(define (extract-snapshot s)
  (server-snapshot (server-state-revision s)
                   (server-state-document s)))

(define (forget-operation-history s up-to-revision)
  (match-define (server-state _ newest-revision original-ids all-operations) s)
  (define retain-count (- newest-revision up-to-revision))
  (cond [(negative? retain-count)
         (error 'forget-operation-history "Cannot forget operation not yet remembered")]
        [(>= retain-count (length all-operations))
         s]
        [else
         (struct-copy server-state s
                      [original-ids (take original-ids retain-count)]
                      [operations (take all-operations retain-count)])]))

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
