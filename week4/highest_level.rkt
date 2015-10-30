#lang racket
(define (f p g h)
  (lambda (x)
    (if (and (p (g x)) (p (h x))) #t #f)))