#lang sicp

(#%require rackunit)
(#%require racket/trace)

(define (double x) (* x 2))
(define (halve x) (/ x 2))
(define (even? x) (= (remainder x 2) 0))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

; if b even - recurse: double a, halve b
; if b odd - recurse: a, b minus 1

(check-equal? (fast-mul 2 2) 4)
(check-equal? (fast-mul 2 4) 8)
(check-equal? (fast-mul 1 2) 2)
(check-equal? (fast-mul 20 2) 40)
(check-equal? (fast-mul 20 16) 320)
(check-equal? (fast-mul 2 3) 6)
(check-equal? (fast-mul 2 5) 10)
(check-equal? (fast-mul 11 11) 121)
(check-equal? (fast-mul 10 0) 0)

(define (fast-mul-iter base n)
  (define (loop a b counter)
     (cond ((= counter 0) a)
           ((even? counter) (loop a (double b) (halve counter)))
           (else (loop (+ a b) b (- counter 1)))))
  (loop 0 base n))

(check-equal? (fast-mul-iter 2 2) 4)
(check-equal? (fast-mul-iter 2 4) 8)
(check-equal? (fast-mul-iter 1 2) 2)
(check-equal? (fast-mul-iter 20 2) 40)
(check-equal? (fast-mul-iter 20 16) 320)
(check-equal? (fast-mul-iter 2 3) 6)
(check-equal? (fast-mul-iter 2 5) 10)
(check-equal? (fast-mul-iter 11 11) 121)
(check-equal? (fast-mul-iter 10 0) 0)
