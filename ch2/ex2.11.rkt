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
    (cond
      ; case 1 - all positive
      ((and (> lbx 0) (> ubx 0) (> lby 0) (> uby 0))
       (make-interval (* lbx lby) (* ubx uby)))

      ; case 2 - first spans zero, second positive
      ((and (< lbx 0) (> ubx 0) (> lby 0) (> uby 0))
       (make-interval (* lbx uby) (* ubx uby)))

      ; case 3 - first negative, second positive
      ((and (< lbx 0) (< ubx 0) (> lby 0) (> uby 0))
       (make-interval (* lbx uby) (* ubx lby)))

      ; case 4 - first positive, second spans zero
      ((and (> lbx 0) (> ubx 0) (< lby 0) (> uby 0))
       (make-interval (* lby ubx) (* ubx uby)))

      ; case 5 - first spans zero, second spans zero
      ((and (< lbx 0) (> ubx 0) (< lby 0) (> uby 0))
       (let ((p1 (* lbx lby))
             (p2 (* lbx uby))
             (p3 (* ubx lby))
             (p4 (* ubx uby)))
       (make-interval (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4))))

      ; case 6 - first negative, second spans zero
      ((and (< lbx 0) (< ubx 0) (< lby 0) (> uby 0))
       (make-interval (* lbx uby) (* lbx lby)))

      ; case 7 - first positive, second negative
      ((and (> lbx 0) (> ubx 0) (< lby 0) (< uby 0))
       (make-interval (* ubx lby) (* lbx uby)))

      ; case 8 - first spans zero, second negative
      ((and (< lbx 0) (> ubx 0) (< lby 0) (< uby 0))
       (make-interval (* ubx lby) (* lbx lby)))

      ; case 9 - all negative
      ((and (< lbx 0) (< ubx 0) (< lby 0) (< uby 0))
       (make-interval (* ubx uby) (* lbx lby)))
      
      (else
       (error "should be impossible to get here")))))

(check-equal? (mul-interval (make-interval 1 2) (make-interval 2 3))
              (make-interval 2 6))

(check-equal? (mul-interval (make-interval -2 -1) (make-interval -3 -2))
              (make-interval 2 6))

(check-equal? (mul-interval (make-interval 1 2) (make-interval -2 3))
              (make-interval -4 6))

(check-equal? (mul-interval (make-interval -2 -1) (make-interval -3 2))
              (make-interval -4 6))

; ----

(check-equal? (mul-interval (make-interval -1 2) (make-interval 2 3))
              (make-interval -3 6))

(check-equal? (mul-interval (make-interval -1 2) (make-interval -3 -2))
              (make-interval -6 3))

(check-equal? (mul-interval (make-interval -1 2) (make-interval -2 3))
              (make-interval -4 6))

(check-equal? (mul-interval (make-interval -2 1) (make-interval -3 2))
              (make-interval -4 6))

; ----

(check-equal? (mul-interval (make-interval -2 1) (make-interval 2 3))
              (make-interval -6 3))

(check-equal? (mul-interval (make-interval -1 2) (make-interval -3 -2))
              (make-interval -6 3))

(check-equal? (mul-interval (make-interval -2 1) (make-interval -2 3))
              (make-interval -6 4))

(check-equal? (mul-interval (make-interval -1 2) (make-interval -3 2))
              (make-interval -6 4))

; ----

(check-equal? (mul-interval (make-interval -2 -1) (make-interval 2 3))
              (make-interval -6 -2))

(check-equal? (mul-interval (make-interval 1 2) (make-interval -3 -2))
              (make-interval -6 -2))

(check-equal? (mul-interval (make-interval -2 -1) (make-interval -2 3))
              (make-interval -6 4))

(check-equal? (mul-interval (make-interval 1 2) (make-interval -3 2))
              (make-interval -6 4))



