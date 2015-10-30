#lang racket
(define (string-repeat str n)
  (if (= n 1) str
      (string-append str (string-repeat str (- n 1)))))

(define (fence n)
  (define (helper p s)
    (cond
      [(> p (round (+ 1 (log n)))) s]
      [(= p (round (+ 1 (log n)))) (helper (+ p 1) (string-append "{-" s "-}"))]
      [else (helper (+ p 1) (string-append "-" s "-"))]))
    (helper 1 (string-append ">" (number->string n) "<")))

; (fence2 n)
;  (string-append "{" (string-repeat "-" (round (+ 1 (log n)))) ">" (number->string n) "<" (string-repeat "-" (round (+ 1 (log n)))) "}"))

(provide string-repeat)