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
