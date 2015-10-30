#lang racket
(define (circle? circle-x circle-y radius point-x point-y)
  (if (> (+ (* (- circle-x point-x) (- circle-x point-x)) (* (- circle-y point-y) (- circle-y point-y))) (* radius radius))
         #f
         #t))