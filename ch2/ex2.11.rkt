#lang sicp

(#%require rackunit
           racket/trace)

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(check-equal? (mul-interval (make-interval 1 2) (make-interval 2 3))
              (make-interval 2 6))

(check-equal? (mul-interval (make-interval -1 -2) (make-interval -2 -3))
              (make-interval 2 6))

(check-equal? (mul-interval (make-interval 1 2) (make-interval -2 3))
              (make-interval -4 6))

(check-equal? (mul-interval (make-interval -1 -2) (make-interval 2 -3))
              (make-interval -4 6))

; ----

(check-equal? (mul-interval (make-interval -1 2) (make-interval 2 3))
              (make-interval -3 6))

(check-equal? (mul-interval (make-interval 1 -2) (make-interval -2 -3))
              (make-interval -3 6))

(check-equal? (mul-interval (make-interval -1 2) (make-interval -2 3))
              (make-interval -4 6))

(check-equal? (mul-interval (make-interval 1 -2) (make-interval 2 -3))
              (make-interval -4 6))

; ----

(check-equal? (mul-interval (make-interval 1 -2) (make-interval 2 3))
              (make-interval -6 3))

(check-equal? (mul-interval (make-interval -1 2) (make-interval -2 -3))
              (make-interval -6 3))

(check-equal? (mul-interval (make-interval 1 -2) (make-interval -2 3))
              (make-interval -6 4))

(check-equal? (mul-interval (make-interval -1 2) (make-interval 2 -3))
              (make-interval -6 4))

; ----

(check-equal? (mul-interval (make-interval -1 -2) (make-interval 2 3))
              (make-interval -6 -2))

(check-equal? (mul-interval (make-interval 1 2) (make-interval -2 -3))
              (make-interval -6 -2))

(check-equal? (mul-interval (make-interval -1 -2) (make-interval -2 3))
              (make-interval -6 4))

(check-equal? (mul-interval (make-interval 1 2) (make-interval 2 -3))
              (make-interval -6 4))



