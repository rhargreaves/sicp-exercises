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

(define (div-interval x y)
  (let ((upy (upper-bound y))
        (lby (lower-bound y)))
    (if (< (* upy lby) 0)
        (error "operand spans zero")
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))

(define (width-interval i)
  (- (upper-bound i) (lower-bound i)))

(check-equal? (width-interval
               (div-interval (make-interval 5 8) (make-interval 2 3)))
              2.3333333333333335)

(check-exn #rx"operand spans zero"
           (lambda ()
             (width-interval
               (div-interval (make-interval 5 8) (make-interval -2 3)))))