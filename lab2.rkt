#lang racket
(require htdp/testing)
(require racket/trace)
(define (mystery L)
(if (null? L)
L
(append (mystery (cdr L))
(list (car L)))))
(check-expect (mystery '(1 2 3)) '(3 2 1))
(check-expect (mystery '((1 2) (3 4) 5 6)) '(6 5 (3 4) (1 2)))
(trace mystery)
(generate-report)

(define (gen-list start end)
  (if (> start end)
      '()
      (cons start (gen-list (+ start 1) end))))

(gen-list 20 10)