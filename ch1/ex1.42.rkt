#lang sicp

(#%require rackunit
           racket/trace)

(define (inc n) (+ n 1))
(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(check-equal? ((compose square inc) 6) 49)
