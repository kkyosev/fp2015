#lang racket
(define (prime? x)
  (define (helper count)
    (cond [(= count x) #t]
          [(zero? (remainder x count)) #f]
          [else (helper (add1 count))]))
  (if (= x 1) #t (helper 2)))

(define (trunctable-prime? x)
  (cond [(< x 0) (trunctable-prime? (- x))]
        [(< x 10) (prime? x)]
        [else (and (prime? x) (trunctable-prime? (quotient x 10)))]))

(define (where lste lstp)
  (define (check-preds x lstpr)
    (cond [(empty? lstpr) #t]
          [(not ((first lstpr) x)) #f]
          [else (check-preds x (rest lstpr))]))
  (cond [(empty? lste) '()]
        [(check-preds (first lste) lstp)
         (cons (first lste) (where (rest lste) lstp))]
        [else (where (rest lste) lstp)]))

(define (zero matrix)
  (define (contains-zero? lst count)
    (cond [(empty? lst) -1]
          [(zero? (first lst)) count]
          [else (contains-zero? (rest lst) (add1 count))]))
  (define (make-zeros-list count lst pos)
    (cond [(empty? lst) '()]
          [(= count pos) (cons 0 (rest lst))]
          [else (cons (first lst) (make-zeros-list (add1 count) (rest lst) pos))]))
  (define (make-zeros-matrix matrix pos)
    (cond [(empty? matrix) '()]
          [else (cons (make-zeros-list 0 (first matrix) pos) (make-zeros-matrix (rest matrix) pos))]))
  (define (helper res matr)
    (cond [(empty? matr) res]
          [(= -1 (contains-zero? (first matr) 0)) (helper res (rest matr))]
          [else (helper (make-zeros-matrix res (contains-zero? (first matr) 0)) (rest matr))]))
  (helper matrix matrix))

(define (suml lst)
  (if (empty? lst) 0 (+ (first lst) (suml (rest lst)))))

(define (deep-reverse matrix)
  (cond [(empty? matrix) '()]
        [else (append (deep-reverse (rest matrix)) (list (reverse (first matrix))))]))

(define (magic-square? M)
  (define (sum-row ind)
    (suml (list-ref M ind)))
  (define (sum-column ind matr)
    (if (empty? matr) 0 (+ (list-ref (first matr) ind) (sum-column ind (rest matr)))))
  (define (sum-primary matr count)
    (if (empty? matr) 0 (+ (list-ref (first matr) count) (sum-primary (rest matr) (add1 count)))))
  (define (sum-secondary matr)
    (sum-primary (deep-reverse matr) 0))
  (define sum (sum-row 0))
  (define (helper ind)
    (cond [(= ind (length M)) #t]
          [(not (and
                 (= sum (sum-row ind))
                 (= sum (sum-column ind M))
                 (= sum (sum-primary M 0))
                 (= sum (sum-secondary M)))) #f]
          [else (helper (add1 ind))]))
  (helper 0))

(define (repeater str)
  (define (string-repeat s n)
    (if (= n 0) "" (string-append s (string-repeat s (- n 1)))))
  (lambda (count glue)
    (string-append str (string-repeat (string-append glue str) (- count 1)))))