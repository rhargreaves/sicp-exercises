#lang sicp

(#%require rackunit
           racket/trace)

(define (double f)
  (lambda (x) (f (f x))))

(define (inc n)
  (+ n 1))

(check-equal? ((double inc) 1) 3)

(check-equal? ((double inc) 5) 7)
(check-equal? ((double (double inc)) 5) 9)
(check-equal? (((double (double double)) inc) 5) 21)

(check-equal? ((double inc) 0) 2)
(check-equal? ((double (double inc)) 0) 4)
(check-equal? (((double (double double)) inc) 0) 16)
