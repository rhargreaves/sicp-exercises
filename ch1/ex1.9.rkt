#lang sicp

; recursive
(define (add a b)
  (if (= a 0)
      b
      (inc (add (dec a) b))))

; iterative
(define (add2 a b)
  (if (= a 0)
      b
      (add2 (dec a) (inc b))))

(add 1 2)
(add2 1 2)