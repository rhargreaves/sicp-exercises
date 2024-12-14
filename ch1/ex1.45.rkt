#lang sicp

(#%require rackunit
           racket/trace)

(define (inc n) (+ n 1))
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (recur n)
    (if (= n 1)
        f
        (compose f (recur (- n 1)))))
  (recur n))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (quad-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y))))
               1.0))

(define (fifth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 4))))
               1.0))

(define (sixth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 5))))
               1.0))

(define (seventh-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 6))))
               1.0))

(define (eighth-root x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (expt y 7))))
               1.0))

; damps required:
; exp 2, 3 = 1
; exp 4-7 = 2
; exp 8-15? = 3

; d = 2 ^ n
; n = log(d) / log(2)

(define (number-of-averages-needed exp)
  (inexact->exact (floor (/ (log exp) (log 2)))))

(check-equal? (number-of-averages-needed 2) 1)
(check-equal? (number-of-averages-needed 3) 1)
(check-equal? (number-of-averages-needed 4) 2)
(check-equal? (number-of-averages-needed 7) 2)
(check-equal? (number-of-averages-needed 8) 3)
(check-equal? (number-of-averages-needed 15) 3)

(define (nth-root x n)
  (let ((avgs (number-of-averages-needed n)))
    (fixed-point ((repeated average-damp avgs)
                  (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

(check-equal? (nth-root 8 3) 1.9999981824788517)
(check-equal? (nth-root 27 3) 2.9999972321057697)
(check-equal? (nth-root (expt 10 4) 4) 10.0)
