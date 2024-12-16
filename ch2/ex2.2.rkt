#lang sicp

(#%require rackunit
           racket/trace)

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(let ((seg (make-segment (make-point 1 2) (make-point 3 4))))
  (print-point (start-segment seg))
  (print-point (end-segment seg)))

; ---

(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (end-segment seg)) (x-point (start-segment seg))) 2)
              (/ (+ (y-point (end-segment seg)) (y-point (start-segment seg))) 2)))

(check-equal? (midpoint-segment (make-segment (make-point 1 2) (make-point 3 4)))
              (make-point 2 3))