#lang racket
(define (add-frac frac1 frac2)
  (simplify-frac (cons (+ (* (car frac1) (cdr frac2)) (* (cdr frac1) (car frac2))) (* (cdr frac1) (cdr frac2)))))

(define (substract-frac frac1 frac2)
   (simplify-frac (cons (- (* (car frac1) (cdr frac2)) (* (cdr frac1) (car frac2))) (* (cdr frac1) (cdr frac2)))))

(define (mult-frac frac1 frac2)
  (cons (* (car frac1) (car frac2)) (* (cdr frac1) (cdr frac2))))

(define (simplify-frac frac)
  (if (= (gcd (car frac) (cdr frac)) 1) frac
      (simplify-frac (cons (/ (car frac) (gcd (car frac) (cdr frac))) (/ (cdr frac) (gcd (car frac) (cdr frac)))))))