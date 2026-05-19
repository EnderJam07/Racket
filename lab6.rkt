#lang racket
;helpers:
(define (member? x L)
  (cond
    [(null? L) #f]
    [(equal? (first L) x) #t]
    [else (member? x (cdr L))]))

(define (intersect S1 S2 [acc '()])
  (cond
    [(null? S1) acc]
    [(null? S2) acc]
    [(member? (car S1) S2) (intersect (cdr S1) S2 (append acc (list (car S1))))]
    [else (intersect (cdr S1) S2) acc]))

(define (Find-Internal1 x L)
  (cond
    [(null? L) (car L)]
    [(equal? (caar L) x) (car L)]
    [else (Find-Internal1 x (cdr L))]))

(define (Find-Internal2 x L)
  (cond
    [(null? L) (car L)]
    [(equal? (car (cdr (car L))) x) (cdr (car L))]
    [else (Find-Internal2 x (cdr L))]))

(define (XZ L1 L2)
  (list (car L1) (car (cdr L2))))
;1.
(define (Reflexive? L S)
  (cond
    [(null? S) #t]
    [else (and (member? (list (car S) (car S)) L) (Reflexive? L (cdr S)))]))

;2.
(define (Symmetric? L)
  (cond
    [(null? L) #t]
    [(member? (list (car (cdr (car L))) (caar L)) L) (Symmetric? (intersect (remove (list (caar L) (car (cdr (car L)))) L) (remove (list (car (cdr (car L))) (caar L)) L)))]
    [else #f]))
;3:
(define (Transitive? L)
  (cond
    [(null? L) #t]
    [(and (member? (XZ (car L) (Find-Internal1 (car (cdr (car L))) L)) L) (member? (XZ (Find-Internal2 (car (cdr (car L))) L) (car L)) L)) #t]
    [else #f]))
(Transitive? '((a b) (b c) (a c)))