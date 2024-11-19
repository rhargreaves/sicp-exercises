#lang sicp

(#%require rackunit)
(#%require racket/trace)

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(check-equal? (mul 2 2) 4)
(check-equal? (mul 6 7) 42)

; -----

(define (double x) (* x 2))
(define (halve x) (/ x 2))
(define (even? x) (= (remainder x 2) 0))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

; if b even - recurse: double a, halve b
; if b odd - recurse: a, b minus 1

; (question only asked for even numbers... so implemented too much!)

(check-equal? (fast-mul 2 2) 4)
(check-equal? (fast-mul 2 4) 8)
(check-equal? (fast-mul 1 2) 2)
(check-equal? (fast-mul 20 2) 40)
(check-equal? (fast-mul 20 16) 320)
(check-equal? (fast-mul 2 3) 6)
(check-equal? (fast-mul 2 5) 10)
(check-equal? (fast-mul 11 11) 121)
(check-equal? (fast-mul 10 0) 0)