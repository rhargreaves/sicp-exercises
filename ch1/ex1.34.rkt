#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

(f f)
; application: not a procedure;
; expected a procedure that can be applied to arguments
;  given: 2

(f f)
(f 2)
(2 2)

; fails as 2 is not a function