#lang racket
(define (string-reverse str)
  (define (helper n res)
   (if (<= n 0) res
       (helper (- n 1) (string-append res (~a (string-ref str (- n 1)))))))
  (helper (string-length str) ""))

(define (to-binary-string n)
  (define (helper n res)
    (if (<= n 0) res
        (helper (quotient n 2) (string-append (~a (remainder n 2)) res))))
  (helper n ""))

(define (from-binary-string str)
  (define (helper n res)
    (if (> n (string-length str)) res
        (helper (+ n 1) (+ res (* (string->number (~a (string-ref str (- n 1)))) (expt 2 (- (string-length str) n )))))))
  (helper 1 0))

(provide string-reverse)
(provide to-binary-string)
        