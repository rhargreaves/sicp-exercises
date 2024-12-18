#lang sicp

(#%require rackunit
           racket/trace)

(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-one n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))


; Simplifications:

; ((lambda (y) y) x) == x

; ((lambda (f) (lambda (x) x)) g) == (lambda (x) x)


; 1)

(lambda (f)
  (lambda (x)
    (f (((lambda (g) (lambda (y) y)) f) x))))

; 2)

(lambda (f)
  (lambda (x)
    (f ((lambda (y) y) x))))

; 3)

(lambda (f)
  (lambda (x)
    (f x)))

; ---

;(define zero
;  (lambda (f)
;    (lambda (x)
;      x)))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

; ---

(define (add n m)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

; key to lambda calculus is that the value of the "number" is the
; number of times that the function (f) is applied to x.
; So 0 = no applications.
; 1 = one application (f x)
; 2 = two apps (f (f x)

; can use a function that increments an integer to test the applications of
; functions:

(define (inc n)
  (+ 1 n))

(check-equal? ((zero inc) 0) 0)
(check-equal? ((one inc) 0) 1)
(check-equal? ((two inc) 0) 2)

(check-equal? (((add zero one) inc) 0) 1)
(check-equal? (((add one one) inc) 0) 2)
(check-equal? (((add two two) inc) 0) 4)
