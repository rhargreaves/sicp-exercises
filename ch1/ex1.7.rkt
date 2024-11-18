#lang sicp

(#%require rackunit)

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess)
 (< (abs (- guess prev-guess)) 0.0001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 0 x))

; tests

(check-equal? (sqrt 4) 2.000000000000002)
(check-equal? (sqrt 9) 3.000000001396984)
(check-equal? (sqrt (square 20000)) 20000.0)
(check-equal? (sqrt (square 2000000)) 2000000.0)