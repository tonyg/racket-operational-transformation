#lang racket/base

(provide gen:operation
         transform-operation

         gen:document
         document-perform
         apply-operation

         compose-operation

         identity-operation
         identity-operation?

         transform-any-pair
         operation-transformer
         swap-values)

(require racket/class)
(require racket/generic)

(define (identity-operation) '())
(define (identity-operation? o) (null? o))

(define-generics operation
  (transform-operation operation other) ;; -> (Values Operation Operation)
  #:defaults ([identity-operation?
               (define (transform-operation o1 o2) (values o1 o2))]
              [pair?
               (define/generic super-transform transform-operation)
               (define (transform-operation o1 o2)
                 (define-values (d* o2*) (super-transform (cdr o1) o2))
                 (define-values (a* o2**) (super-transform (car o1) o2*))
                 (values (cons a* d*) o2**))]
              [object?
               (define (transform-operation o1 o2)
                 (send o1 transform-operation o2))]))

(define-generics document
  (document-perform document operation #:remote? remote?) ;; -> Document
  #:defaults ([object?
               (define (document-perform d o #:remote? remote?)
                 (send d document-perform o #:remote? remote?))]))

(define (apply-operation op doc #:remote? [remote? #f])
  (let walk ((op op) (doc doc))
    (cond
      [(identity-operation? op) doc]
      [(pair? op) (walk (car op) (walk (cdr op) doc))]
      [else (document-perform doc op #:remote? remote?)])))

;; o2, then o1
(define (compose-operation o1 o2)
  (cond
    [(identity-operation? o1) o2]
    [(identity-operation? o2) o1]
    [(pair? o2) (cons o1 o2)]
    [else (list o1 o2)]))

(define (transform-any-pair o1 o2)
  (define-values (o1* d*) (transform-operation o1 (cdr o2)))
  (define-values (o1** a*) (transform-operation o1* (car o2)))
  (values o1** (cons a* d*)))

(define trying-flipped? (make-parameter #f))

(define-syntax-rule (operation-transformer (o1 o2) [pred? body ...] ...)
  (cond
    [(identity-operation? o2) (values o1 o2)]
    [(pair? o2) (transform-any-pair o1 o2)]
    [(pred? o2) body ...] ...
    [(trying-flipped?) (error 'transform-operation "Unimplemented for ~v and ~v" o2 o1)]
    [else (parameterize ((trying-flipped? #t)) (swap-values (transform-operation o2 o1)))]))

(define-syntax-rule (swap-values expr)
  (let-values (((v1 v2) expr)) (values v2 v1)))
