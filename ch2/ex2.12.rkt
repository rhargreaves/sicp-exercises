#lang sicp

(#%require rackunit
           racket/trace)

(define (make-interval a b)
  (if (> a b)
      (error "a > b")
      (cons a b)))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (mul-interval x y)
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
       (let ((p1 (* lbx lby))
             (p2 (* lbx uby))
             (p3 (* ubx lby))
             (p4 (* ubx uby)))
       (make-interval (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4)))))

(define (make-centre-width c w)
  (make-interval (- c w) (+ c w)))

(define (centre i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(check-equal? (centre (make-centre-width 1 0.2)) 1.0)
(check-equal? (width (make-centre-width 1 0.2)) 0.19999999999999996)

(define (make-centre-percent c p)
  (make-centre-width c (* 2 (* p c))))

(check-equal? (centre (make-centre-percent 1 0.2)) 1.0)
(check-equal? (width (make-centre-percent 1 0.2)) 0.39999999999999997)

(define (percent i)
  (/ (/ (width i) 2) (centre i)))

(check-equal? (percent (make-centre-percent 1 0.2)) 0.19999999999999998)
(check-equal? (percent (make-centre-percent 1 0.05)) 0.05000000000000002)

