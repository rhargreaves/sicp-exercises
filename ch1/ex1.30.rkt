#lang sicp

(#%require rackunit
           racket/trace)

(define (identity n) n)
(define (inc n) (+ n 1))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-integers a b)
  (sum identity a inc b))

(check-equal? (sum identity 1 inc 3) 6)
(check-equal? (sum identity 1 inc 10) 55)
(check-equal? (sum-integers 1 3) 6)

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(check-equal? (sum-iter identity 1 inc 3) 6)
(check-equal? (sum-iter identity 1 inc 10) 55)