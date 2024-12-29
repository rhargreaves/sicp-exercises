#lang sicp

(#%require rackunit
           racket/trace)

; --- ex2.14

(define (make-interval a b)
  (if (> a b)
      (error "a > b")
      (cons a b)))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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
                      (make-interval (/ 1.0 upy)
                                     (/ 1.0 lby))))))

(define (make-centre-width c w)
  (make-interval (- c w) (+ c w)))

(define (centre i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-centre-percent c p)
  (make-centre-width c (* 2 (* p c))))

(define (percent i)
  (/ (/ (width i) 2) (centre i)))

(check-equal? (percent (make-centre-percent 1 0.2)) 0.19999999999999998)
(check-equal? (percent (make-centre-percent 1 0.05)) 0.05000000000000002)

; ---

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;(par1 (make-centre-percent 5 0.1) (make-centre-percent 6 0.2))
;(par2 (make-centre-percent 5 0.1) (make-centre-percent 6 0.2))

(par1 (make-centre-percent 5 0.05) (make-centre-percent 5 0.05))
(par2 (make-centre-percent 5 0.05) (make-centre-percent 5 0.05))


; -----

(define (foo a b)
  (/ (* a b) (+ a b)))

(define (bar a b)
  (/ 1 (+ (/ 1 a) (/ 1 b))))

; (foo 0.00001 0.00002)
; (bar 0.00001 0.00002)


; --- ex2.15 / ex2.16

; The 2nd equation uses R1 and R2 twice, which means the error bounds will be applied twice.
; This is incorrect, as it's the same resistor, so you shouldn't be applying a second error
; bound to the calculation because there is only one instance of the resistor which would
; have a particular error value. Therefore the error amount should be global across uses.
; Eva is correct.
;
; To get around this - you need to build a structure which represents the resistors as having a
; single error value per run of calculation. This would require twice the runs per resistor,
; with final lower/upper bounds at the end taken from the various runs. Such a structure
; would requiring "walking" through it to ensure that the same error value (state) is used per
; resistor, per run.
;
; Tricky indeed!