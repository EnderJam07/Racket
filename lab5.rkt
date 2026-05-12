#lang racket
(define (member? x L)
  (cond
    [(null? L) #f]
    [(equal? (first L) x) #t]
    [else (member? x (cdr L))]))

(define (bag-difference B1 B2)
  (cond
    [(null? B1) B1]
    [(null? B2) B1]
    [(member? (car B1) B2) (bag-difference (remove (car B1) B1) (remove (car B1) B2))]
    [else (bag-difference B1 (cdr B2))]))

(define (bag-union B1 B2 [acc '()])
  (cond
    [(null? B1) (append acc B2)]
    [(null? B2) (append acc B1)]
    [(member? (car B2) B1) (bag-union (cdr B1) (remove (car B1) B2) (append acc (list (car B1))))]
    [else (bag-union (cdr B1) B2 (append acc (list (car B1))))]))

(define (bag-intersection B1 B2 [acc '()])
  (cond
    [(null? B1) acc]
    [(null? B2) acc]
    [(member? (car B1) B2) (bag-intersection (cdr B1) (remove (car B1) B2) (append acc (list (car B1))))]
    [else (bag-intersection (cdr B1) (remove (car B1) B2) acc)]))