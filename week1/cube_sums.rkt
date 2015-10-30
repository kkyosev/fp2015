#lang racket
(define (cube-sums? n)
  (define (helper a b)
    (if (= n (+ (* a a a) (* b b b))) #t
        (if (> (* a a a) n) (helper 1 (+ b 1))
            (if (> (* b b b) n) #f
                (helper (+ a 1) b)))))
  (helper 1 1))

(define (count-cube-sums from to)
  (define (helper a res)
    (if (> a to) res
        (if (cube-sums? a) (helper (+ a 1) (+ res 1))
            (helper (+ a 1) res))))
  (helper from 0))
          
  