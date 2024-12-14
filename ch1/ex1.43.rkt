#lang sicp

(#%require rackunit
           racket/trace)

(define (inc n) (+ n 1))
(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(check-equal? ((compose square inc) 6) 49)

(define (repeated f n)
  (define (recur n)
    (if (= n 1)
        f
        (compose f (recur (- n 1)))))
  (recur n))

(check-equal? ((repeated square 1) 5) 25)
(check-equal? ((repeated square 2) 5) 625)