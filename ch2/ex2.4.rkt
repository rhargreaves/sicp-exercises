#lang sicp

(#%require rackunit
           racket/trace)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(check-equal? (cdr (cons 1 2)) 2)
(check-equal? (cdr (lambda (m) (m 1 2))) 2)
(check-equal? ((lambda (m) (m 1 2)) (lambda (p q) q)) 2)