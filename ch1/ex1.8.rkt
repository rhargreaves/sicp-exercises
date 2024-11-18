#lang sicp

(#%require rackunit)

(define (cube-rt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (cube-rt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess)
 (< (abs (- guess prev-guess)) 0.00000001))

(define (cube x) (* x x x))

(define (cube-rt x)
  (cube-rt-iter 1.0 0 x))

; tests

(check-equal? (cube-rt 8) 2.0)
(check-equal? (cube-rt (cube 20)) 20.0)
(check-equal? (cube-rt (cube 20000)) 20000.0)