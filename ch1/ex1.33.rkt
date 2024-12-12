#lang sicp

(#%require rackunit
           racket/trace)

(define (identity n) n)
(define (inc n) (+ n 1))

(define (filtered-accumulate combiner null-value predicate term a next b)
  (if (> a b)
      null-value
      (combiner (if (predicate a) (term a) null-value)
                (filtered-accumulate combiner null-value predicate term (next a) next b))))

(define (always-true n) #t)
(define (always-false n) #f)

(check-equal? (filtered-accumulate + 0 always-true identity 1 inc 3) 6)
(check-equal? (filtered-accumulate + 0 always-false identity 1 inc 3) 0)

; ---

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
   a
   (gcd b (remainder a b))))

; a)

(define (sum-of-primes-squared a b)
  (filtered-accumulate + 0 prime? square a inc b))

(check-equal? (sum-of-primes-squared 1 4) 14)

; b)

(define (product-of-relatively-prime a b)
  (define (relatively-prime? i)
    (= (gcd i b) 1))
  (filtered-accumulate * 1 relatively-prime? identity a inc b))
