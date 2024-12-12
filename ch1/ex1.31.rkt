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

; ---
; a)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(check-equal? (factorial 0) 1)
(check-equal? (factorial 1) 1)
(check-equal? (factorial 2) 2)
(check-equal? (factorial 3) 6)
(check-equal? (factorial 4) 24)

(define (pi-approx n)
  (define (odd-n n)
    (/ (* n 2) (+ (* n 2) 1)))
  (define (even-n n)
    (/ (* (+ n 1) 2) (+ (* n 2) 1)))  
  (* (product odd-n 1 inc n)
     (product even-n 1 inc n)
     4.0))

(check-equal? (pi-approx 1000) 3.142377365093878)

; b)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-iter n)
  (product-iter identity 1 inc n))

(check-equal? (factorial-iter 2) 2)
(check-equal? (factorial-iter 3) 6)
(check-equal? (factorial-iter 4) 24)
