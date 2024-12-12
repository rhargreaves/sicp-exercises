#lang sicp

(#%require rackunit
           racket/trace)

(define (identity n) n)
(define (inc n) (+ n 1))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-integers a b)
  (sum identity a inc b))

(check-equal? (sum identity 1 inc 3) 6)
(check-equal? (sum-integers 1 3) 6)

; -----

; PI/8 = 1/(1*3) + 1/(5*7) + 1/(9+11) + ... 
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(check-equal? (* 8 (pi-sum 1 100000)) 3.141572653589795)

; -----

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(check-equal? (* 8 (pi-sum2 1 100000)) 3.141572653589795)

; -----

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(check-equal? (integral cube 0 1 0.01) 0.24998750000000042)
(check-equal? (integral cube 0 1 0.001) 0.249999875000001)

; ------

; (define (sum term a next b)
;   (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

;; (define (simpson-rule f a b n)
;;   (define (sr-next n) (+ n 1))
;;   (define (sr-func x)
;;     (*
;;      (cond ((or (= x 0) (= x n)) 1)
;;            ((even? x) 2)
;;            (else 4))
;;      (f x)))
;;   (sum sr-func (if (= x 0)
;;             a
;;             (+ a (* x (/ (- b a) x)))) sr-next n))

; from https://sicp-solutions.net/post/sicp-solution-exercise-1-29/
; assuming n is even
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x h h))
  (* (+ (f a)
        (* 2 (sum f (add-2h a) add-2h (- b h)))
        (* 4 (sum f (+ a h) add-2h b))
        (f b))
     (/ h 3)))

(check-equal? (integral-simpson cube 0 1.0 100) 0.25000000000000044)
(check-equal? (integral-simpson cube 0 1.0 1000) 0.25000000000000083)

