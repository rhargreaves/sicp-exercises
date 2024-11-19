#lang sicp

(#%require rackunit)
(#%require racket/trace)

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; a.

(trace sine)
(trace p)

;(sine 12.15) ; p is applied 5 times, as shown in trace

; b.

(define pi 3.141592)

(sine pi)          ; space (max depth) = 5, steps = 9
(sine (* 2 pi))    ; space = 5, steps = 9

(sine 1)           ; space = 4, steps = 7
(sine 0.5)         ; space = 3, steps = 5
(sine 0.1)         ; space = 1, steps = 1

(sine 100)         ; space = 7, steps = 15
(sine 1000)        ; space = 10, steps = 19

; space/steps growth seems logarithmic.
; O(log(n))
