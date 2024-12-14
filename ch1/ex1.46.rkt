#lang sicp

(#%require rackunit
           racket/trace)

(define (inc n) (+ n 1))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average x y) (/ (+ x y) 2))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (recur n)
    (if (= n 1)
        f
        (compose f (recur (- n 1)))))
  (recur n))

;; (define (fixed-point f first-guess close-enough?)
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;           next
;;           (try next))))
;;   (try first-guess))

(define (always-false guess next) #f)
(define (always-true guess next) #t)
(define (more-than-ten? guess next) (> guess 10))

(define (iterative-improve good-enough? improve-guess)
  (lambda (initial-guess)
    (define (recur guess)
      (let ((next (improve-guess guess)))
        (if (good-enough? guess next)
            next
            (recur next))))
    (recur initial-guess)))

((iterative-improve more-than-ten? inc) 1)

(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess close-enough?)
  ((iterative-improve close-enough? f) first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))
  
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0 close-enough?))

(sqrt 4)
                 