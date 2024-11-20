#lang sicp

(#%require rackunit
           racket/trace)

; using new-if causes a divide by zero error in remainder.
(define (new-if predicate true-clause false-clause)
  (cond (predicate true-clause)
        (else false-clause)))

(define (gcd a b)
  (if (= b 0)
   a
   (gcd b (remainder a b))))

(trace gcd)

; Applicative order is when the operands are evaluated first.
; Normal-order is when the arguments are substituted into the expression unevaluated.
; Racket is applicative order (with the special form "if" being an exception)

(gcd 206 40)

; Using normal-order evaluation, remainder is called 1 + 2 + 4 + 7 + 4 + 18 times.
; This can be shown by expanding gcd by hand.
; Remainder is only called 4 times in applicative order.