#lang sicp

(#%require rackunit)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.0001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; tests

(check-equal? (sqrt 9) 3.000000001396984)

; with new-if - runs out of memory (actually also an infinite loop, will never end)
; this is because the arguments are evaluated before the procedure is applied.
; if is a special form which prevents this behaviour
