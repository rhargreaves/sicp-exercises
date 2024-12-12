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

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(check-equal? (sum-iter identity 1 inc 3) 6)
(check-equal? (sum-iter identity 1 inc 10) 55)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(check-equal? (factorial 3) 6)
(check-equal? (factorial 4) 24)

; -----

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(check-equal? (accumulate + 0 identity 1 inc 3) 6)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(check-equal? (accumulate-iter + 0 identity 1 inc 3) 6)

