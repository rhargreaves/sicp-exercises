#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(check-equal? (fast-prime? 7 100) #t)
(check-equal? (fast-prime? 19 100) #t)
(check-equal? (fast-prime? 199 100) #t)
(check-equal? (fast-prime? 1999 100) #t)
(check-equal? (fast-prime? 19999 100) #f)
