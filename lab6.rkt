#lang racket
(define (member? x L)
  (cond
    [(null? L) #f]
    [(equal? (first L) x) #t]
    [else (member? x (cdr L))]))
;1.
(define (Reflexive? L S)
  (cond
    [(null? S) #t]
    [else (and (member? (list (car S) (car S)) L) (Reflexive? L (cdr S)))]))

;2.
