#lang racket
(require "binary.rkt")

(define (odd-one-bin str)
  (define (helper n res)
    (if (> n (string-length str)) res
       (if (string=? "1" (~a (string-ref str (- n 1)))) (helper (+ n 1) (+ res 1))
         (helper (+ n 1) res))))
  (odd? (helper 1 0)))
    
(define (hack-num n)
 (if (and (string=? (to-binary-string n) (string-reverse (to-binary-string n))) (odd-one-bin (to-binary-string n))) #t #f))

(define (next-hack n)
  (if (hack-num (+ n 1)) (+ n 1)
      (next-hack (+ n 1))))