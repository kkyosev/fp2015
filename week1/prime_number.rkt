#lang racket
(define(div? x y)
(if (= (remainder x y) 0)
    #t
    #f))

(define (divh? x y)
  (cond
    [(= y 2) (div? x y)]
    [(= y 1) #f]
    [else (or (div? x y) (divh? x (- y 1)))]))
  
(define (prime? n)
  (not (divh? n (round (sqrt n)))))