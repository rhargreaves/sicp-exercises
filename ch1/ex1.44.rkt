#lang sicp

(#%require rackunit
           racket/trace)

(define (inc n) (+ n 1))
(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(check-equal? ((compose square inc) 6) 49)

(define (repeated f n)
  (define (recur n)
    (if (= n 1)
        f
        (compose f (recur (- n 1)))))
  (recur n))

(check-equal? ((repeated square 2) 5) 625)

; ----

(define dx 0.001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))) 3)))

(check-equal? ((smooth square) 3) 9.000000666666667)
; (tan 5)
(check-equal? ((smooth tan) 5) -3.3805430150124103)

; ----

((repeated (lambda (x) ((smooth square) x)) 3) 10)

((repeated (compose smooth square) 3) 10)
