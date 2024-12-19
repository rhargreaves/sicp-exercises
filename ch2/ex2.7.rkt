#lang sicp

(#%require rackunit
           racket/trace)

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(check-equal? (add-interval
               (make-interval 1 2)
               (make-interval 3 4)) (cons 4 6))