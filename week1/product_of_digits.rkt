#lang racket
(define (product-digits n)
  (if (= n 0)
      1
      (* (remainder n 10) (product-digits (quotient n 10)))))