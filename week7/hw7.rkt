#lang racket
(define (make-tree node left right)
  (list node left right))

(define (make-leaf node)
  (make-tree node '() '()))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (first tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (count-nodes tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (count-nodes (left tree)) (count-nodes (right tree)))]))

(define (height tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (max (height (left tree)) (height (right tree))))]))

(define (tree-level lvl tree)
  (cond [(empty-tree? tree) '()]
        [(= lvl 1) (list (root tree))]
        [else (append (tree-level (- lvl 1) (left tree))
                      (tree-level (- lvl 1) (right tree)))]))

(define (tree-levels tree)
  (define (helper count)
    (cond [(empty-tree? tree) '()]
          [(> count (height tree)) '()]
          [else (cons (tree-level count tree) (helper (add1 count)))]))
  (helper 1))

(define (tree-map f tree)
  (cond [(empty-tree? tree) '()]
        [else (make-tree (f (root tree)) (tree-map f (left tree)) (tree-map f (right tree)))]))


(define (bst-insert x tree)
  (cond
    [(empty-tree? tree) (make-leaf x)]
    [(< x (root tree)) (make-tree (root tree)
                               (bst-insert x (left tree))
                               (right tree))]
    [else (make-tree (root tree)
                  (left tree)
                  (bst-insert x (right tree)))]))

(define (bst-elem? x tree)
  (cond [(empty-tree? tree) #f]
        [(= x (root tree)) #t]
        [(> x (root tree)) (bst-elem? x (right tree))]
        [else (bst-elem? x (left tree))]))

(define (bst->list tree)
  (cond [(empty-tree? tree) '()]
        [else
         (append (bst->list (left tree)) (list (root tree)) (bst->list (right tree)))]))

(define (bst? tree)
  (define (ascending? l)
    (cond
      [(= (length l) 1) #t]
      [(> (first l) (second l)) #f]
      [else (ascending? (rest l))]))
  (ascending? (bst->list tree)))