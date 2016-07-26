#lang racket/base

(provide (struct-out simple-document))

(require racket/serialize)
(require "../operation.rkt")
(require "../text.rkt")

(serializable-struct simple-document (text) #:transparent
  #:methods gen:document
  [(define (document-perform d o #:remote? remote?)
     (simple-document (string-perform-text-operation o (simple-document-text d))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test-data
  (define d0 (simple-document "abcde"))
  (define i1 (insertion 2 "xyz"))
  (define i2 (insertion 3 "uvw"))
  (define d1 (deletion 1 2))
  (define d2 (deletion 2 2))
  (provide (all-defined-out)))

(module+ test
  (require rackunit)
  (require (submod ".." test-data))

  (define-syntax-rule (check-equal-values? vs1 vs2 . msg)
    (check-equal? (call-with-values (lambda () vs1) list)
                  (call-with-values (lambda () vs2) list)
                  . msg))

  (check-equal-values? (transform-operation i1 d1) (values (insertion 1 "") (deletion 1 5)))
  (check-equal-values? (transform-operation d1 i1) (values (deletion 1 5) (insertion 1 "")))
  (check-equal-values? (transform-operation i1 i2) (values i1 (insertion 6 "uvw")))
  (check-equal-values? (transform-operation d1 d2) (values (deletion 1 1) (deletion 1 1)))

  (check-equal-values? (transform-operation (compose-operation (insertion 7 "uvw") i1) d1)
                       (values (compose-operation (insertion 2 "uvw") (insertion 1 ""))
                               (deletion 1 5)))

  (check-equal? (apply-operation (compose-operation d1 i1) d0) (simple-document "ayzcde"))
  (check-equal? (apply-operation (compose-operation i1 d1) d0) (simple-document "adxyze"))
  )
