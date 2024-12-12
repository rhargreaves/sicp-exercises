#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(check-equal? (fixed-point cos 1.0) 0.7390822985224023)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(check-equal? (sqrt 4) 2.000000000000002)

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0))

(check-equal? (golden-ratio) 1.6180327868852458)