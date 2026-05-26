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

(define (check-pairs x yz L)
  (cond
    [(null? yz) #t]
    [(member? (list x (cadr (car yz))) L) (check-pairs x (cdr yz) L)]
    [else #f]))
(define (find-connections x L)
  (cond
    [(null? L) '()]
    [(equal? (caar L) x) (cons (car L) (find-connections x (cdr L)))]
    [else (find-connections x (cdr L))]))
(define (build-new-pairs x yz-pairs)
  (cond
    [(null? yz-pairs) '()]
    [else (cons (list x (cadar yz-pairs))
                (build-new-pairs x (cdr yz-pairs)))]))
;1.
(define (Reflexive-Closure L S [full L])
  (cond
    [(null? S) full]
    [(not (member? (list (car S) (car S)) L)) (Reflexive-Closure L (cdr S) (append full (list (list (car S) (car S)))))]
    [else (Reflexive-Closure L (cdr S) full)]))

;2.
(define (Symmetric-Closure L [full L])
  (cond
    [(null? L) full]
    [(member? (list (cadar L) (caar L)) full) (Symmetric-Closure (cdr L) full)]
    [else (Symmetric-Closure (cdr L) (append full (list (list (cadar L) (caar L)))))]))
;3:
(define (Transitive-Closure L [full L] [original-full full])
  (cond
    [(and (null? L) (equal? full original-full)) full]
    [(null? L) (Transitive-Closure full full full)]
    [else
     (Transitive-Closure
      (cdr L)
      (remove-duplicates
       (append full (build-new-pairs (caar L) (find-connections (cadar L) full))))
      original-full)]))