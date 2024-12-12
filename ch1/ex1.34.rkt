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

; presumably it fails as f does not accept the number 2, only another function