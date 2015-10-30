#lang racket
(define (series1 a b n)
  (cond
    [(= n 1) a]
    [(= n 2) b]
    [else (series1 b (+ a b) (- n 1))]
   )
 )

(define (series a b n)
  (define (helper p q count)
    (if (= count 1) p
        (helper q (+ p q) (- count 1))))
  (helper a b n))

(define (lucas n)
  (series 2 1 n))

(define (fibonacci n)
  (series 1 1 n))

(define (summed-member n)
  (+ (lucas n) (fibonacci n)))

(define (nth-fibonacci-sum n)
  (define (helper n res)
    (if (> n 0) (helper (- n 1) (+ res (fibonacci n)))
        res))
  (helper n 0))

(define (nth-lucas-sum n)
  (define (helper n res)
    (if (> n 0) (helper (- n 1) (+ res (lucas n)))
        res))
  (helper n 0))

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n)))