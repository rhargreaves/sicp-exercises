#lang sicp

(#%require rackunit
           racket/trace)

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (subtract-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width-interval i)
  (- (upper-bound i) (lower-bound i)))

(check-equal? (width-interval (make-interval 2 4)) 2)

; sub

(check-equal? (width-interval
               (subtract-interval (make-interval 4 6) (make-interval 2 4)))
              0)

(check-equal? (-
               (width-interval (make-interval 4 6))
               (width-interval (make-interval 2 4)))
              0)

; add

(check-equal? (width-interval
               (add-interval (make-interval 4 6) (make-interval 2 4)))
              4)

(check-equal? (+
               (width-interval (make-interval 4 6))
               (width-interval (make-interval 2 4)))
              4)

; mul - X

(check-equal? (width-interval
               (mul-interval (make-interval 5 8) (make-interval 2 3)))
              14)

(check-equal? (*
               (width-interval (make-interval 5 8))
               (width-interval (make-interval 2 3)))
              3)


; div - X

(check-equal? (width-interval
               (div-interval (make-interval 5 8) (make-interval 2 3)))
              2.3333333333333335)

(check-equal? (/
               (width-interval (make-interval 5 8))
               (width-interval (make-interval 2 3)))
              3)
