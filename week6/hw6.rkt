#lang racket
(define (string-repeat str n)
  (cond
    [(= n 0) str]
    [(= n 1) str]
    [else (string-append str (string-repeat str (- n 1)))]))

(define (run-length-encode input)
  (define (head str) (substring str 0 1))
  (define (tail str) (substring str 1))
  (define (help str count res)
    (cond
      [(= (string-length str) 1) (string-append res
                                                ((lambda (x) (if (not (= x 1)) (~a x) "")) count)
                                                (head str))]
      [(equal? (head str) (head (tail str))) (help (tail str) (add1 count) res)]
      [(= count 1) (help (tail str) 1 (string-append res (head str)))]
      [else (help (tail str) 1 (string-append res (~a count) (head str)))]))
  (help input 1 ""))

(define (run-length-decode str)
  (define (helper res m str p)
    (cond
      [(= p (string-length str)) res]
      [(string->number (~a (string-ref str p)))
       (helper res (+ (* 10 m) (string->number (~a (string-ref str p)))) str (+ p 1))]
      [else (helper (string-append res (string-repeat (~a (string-ref str p)) m)) 0 str (+ p 1))]))
  (helper "" 0 str 0))