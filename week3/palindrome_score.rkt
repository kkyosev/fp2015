#lang racket
(define (reverse-int n)
  (define (helper n res)
    (cond [(= n 0) res]
          [else (helper (quotient n 10) (+ (* res 10) (remainder n 10)))]))
  (helper n 0))

(define (palindrome? n)
  (= n (reverse-int n)))

(define (p-score n)
  (if (palindrome? n) 1
      (+ 1 (p-score (+ n (reverse-int n))))))