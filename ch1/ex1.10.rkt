#lang sicp

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))


(f 0) ; 0
(f 1) ; 2
(f 2) ; 4
(f 3) ; 6
(f 4) ; 8
(f 5) ; 10
(f 6) ; 12

; f = 2x

(g 0) ; 0
(g 1) ; 2
(g 2) ; 4
(g 3) ; 8
(g 4) ; 16
(g 5) ; 32
(g 6) ; 64

; g = 2 ^ n  (unless n = 0, which then g = 0)

(h 0) ; 0
(h 1) ; 2  
(h 2) ; 4       = 2 ^ 2
(h 3) ; 16      = 2 ^ 2 ^ 2
(h 4) ; 65536   = 2 ^ 2 ^ 2 ^ 2
(h 5) ; large number
(h 6) ; large number

; h = 2 ^^ n (hyperoperation, specifically tetration)