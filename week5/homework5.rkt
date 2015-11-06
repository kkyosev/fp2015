#lang racket
(define (sum numbers)
  (cond
    [(empty? numbers) 0]
    [else (+ (first numbers) (sum (rest numbers)))]))

(define (member? x items)
  (cond
    [(empty? items) #f]
    [(equal? (first items) x) #t]
    [else (member? x (rest items))]))

(define (length2 items)
  (cond
    [(empty? items) 0]
    [else (+ 1 (length2 (rest items)))]))

(define (list-ref2 items n)
  (define (helper e i)
    (cond
      [(empty? e) (error "List is shorter than expected! ")]
      [(equal? i n) (first e)]
      [else (helper (rest e) (+ i 1))]))
  (helper items 0))

(define (range a b)
  (cond
    [(> a b) '()]
    [else (cons a (range (+ a 1) b))]))

(define (build-list2 n f)
  (define (helper sp i)
   (cond
     [(> i n) sp]
     [else (cons (f i) (helper sp (+ i 1)))]))
  (helper '() 0))
      
(define (append2 l1 l2)
  (cond
    [(empty? l2) '()]
    [(empty? l1) (cons (first l2) (append2 l1 (rest l2)))]
    [else (cons (first l1) (append2 (rest l1) l2))]))

(define (reverse2 items)
  (define (helper res l)
  (cond
    [(empty? l) res]
    [else (helper (cons (first l) res) (rest l))]))
  (helper '() items))

(define (take2 n items)
  (cond
    [(= n 0) '()]
    [else (cons (first items) (take2 (- n 1) (rest items)))]))

(define (drop2 n items)
  (cond
    [(= n 0) items]
    [else (drop2 (- n 1) (rest items))]))

(define (take-while p items)
  (cond
    [(or (not (p (first items))) (empty? items)) '()]
    [else (cons (first items) (take-while p (rest items)))]))

(define (drop-while p items)
  (cond
    [(empty? items) '()]
    [(p (first items)) (drop-while p (rest items))]
    [else items]))

(define (number->list n)
  (define (helper n)
    (cond
      [(<= n 0) '()]
      [else (cons (remainder n 10) (helper (quotient n 10)))]))
  (reverse (helper n)))

(define (list->number ns)
  (define (helper res l)
    (cond
      [(empty? l) res]
      [else (helper (+ (* res 10) (first l)) (rest l))]))
  (helper 0 ns))