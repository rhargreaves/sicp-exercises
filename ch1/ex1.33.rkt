#lang sicp

(#%require rackunit
           racket/trace)

(define (identity n) n)
(define (inc n) (+ n 1))

(define (accumulate-old combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-old combiner null-value term (next a) next b))))

(check-equal? (accumulate-old + 0 identity 1 inc 3) 6)

; ---

(define (filtered-accumulate combiner null-value predicate term a next b)
  (if (> a b)
      null-value
      (combiner (if (predicate a) (term a) null-value)
                (filtered-accumulate combiner null-value predicate term (next a) next b))))

(define (always-true n) #t)
(define (always-false n) #f)

(check-equal? (filtered-accumulate + 0 always-true identity 1 inc 3) 6)
(check-equal? (filtered-accumulate + 0 always-true identity 1 inc 3) 0)
