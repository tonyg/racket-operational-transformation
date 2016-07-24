#lang racket/base

(provide (struct-out insertion)
         (struct-out deletion)
         string-perform-text-operation)

(require racket/match)
(require racket/serialize)
(require "operation.rkt")

(serializable-struct insertion (location text) #:transparent
  #:methods gen:operation
  [(define (transform-operation o1 o2)
     (operation-transformer (o1 o2)
       [insertion? (order insertion-location transform-insertion-insertion o1 o2)]
       [deletion? (transform-insertion-deletion o1 o2)]))])

(serializable-struct deletion (location length) #:transparent
  #:methods gen:operation
  [(define (transform-operation o1 o2)
     (operation-transformer (o1 o2)
       [deletion? (order deletion-location transform-deletion-deletion o1 o2)]))])

(define (order a xf o1 o2)
  (if (<= (a o1) (a o2))
      (xf o1 o2)
      (swap-values (xf o2 o1))))

(define (transform-insertion-insertion o1 o2) ;; Precondition: loc1 <= loc2
  (match-define (insertion loc2 str2) o2)
  (values o1 (insertion (+ loc2 (string-length (insertion-text o1))) str2)))

(define (transform-deletion-deletion o1 o2) ;; Precondition: loc1 <= loc2
  (match-define (deletion loc1 len1) o1)
  (match-define (deletion loc2 len2) o2)
  (cond
    [(<= (+ loc1 len1) loc2)          (values (deletion loc1 len1) (deletion (- loc2 len1) len2))]
    [(<= (+ loc2 len2) (+ loc1 len1)) (values (deletion loc1 (- len1 len2)) (deletion loc1 0))]
    [else (values (deletion loc1 (- loc2 loc1)) (deletion loc1 (- (+ loc2 len2) (+ loc1 len1))))]))

(define (transform-insertion-deletion o1 o2)
  (match-define (insertion loc1 str1) o1)
  (match-define (deletion loc2 len2) o2)
  (cond [(and (>= loc1 loc2) (< loc1 (+ loc2 len2)))
         (values (insertion loc2 "") (deletion loc2 (+ len2 (string-length str1))))]
        [(< loc1 loc2)
         (values (insertion loc1 str1) (deletion (+ loc2 (string-length str1)) len2))]
        [else
         (values (insertion (- loc1 len2) str1) (deletion loc2 len2))]))

(define (string-perform-text-operation o d)
  (match o
    [(insertion loc0 str)
     (define loc (max 0 (min (string-length d) loc0)))
     (string-append (substring d 0 loc) str (substring d loc))]
    [(deletion loc len)
     (string-append (substring d 0 (max 0 loc))
                    (substring d (min (+ loc len) (string-length d))))]))
