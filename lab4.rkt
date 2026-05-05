#lang racket
;1:
(define (member? x L)
  (cond
    [(null? L) #f]
    [(equal? (first L) x) #t]
    [else (member? x (cdr L))]))
(define (subset? L1 L2)
  (cond
    [(null? L1) #t]
    [(list? (first L1))
    (and (subset? (first L1) L2)
      (subset? (cdr L1) L2))]
    [(member? (first L1) L2) (subset? (cdr L1) L2)]
    [else #f]))
(define (set-equal? L1 L2)
  (cond
    [(and (subset? L1 L2) (subset? L2 l1))]
    [else #f]))
;2:
(define (union S1 S2)
  (cond
    [(null? S1) S2]
    [(null? S2) S1]
    [(member? (car S2) S1) (union S1 (cdr S2))]
    [else (union (append S1 (list (car S2))) (cdr S2))]))

(define (intersect S1 S2 [acc '()])
  (cond
    [(null? S1) acc]
    [(null? S2) acc]
    [(member? (car S1) S2) (intersect (cdr S1) S2 (append acc (list (car S1))))]
    [else (intersect (cdr S1) S2) acc]))
