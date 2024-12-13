#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

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

; (check-equal? (fixed-point cos 1.0) 0.7390822985224023)


(check-equal?
 (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
 4.555532270803653)

(check-equal?
 (fixed-point (lambda (x) (* 0.5 (+ x (/ (log 1000) (log x))))) 2.0)
 4.555537551999825)