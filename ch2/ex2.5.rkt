#lang sicp

(#%require rackunit
           racket/trace)

; this is called "unique prime factorization theorem"

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (iter remaining result)
    (if (= 0 (remainder remaining 2))
        (iter (/ remaining 2) (+ result 1))
        result))
  (iter z 0))

(define (cdr z)
  (define (iter remaining result)
    (if (= 0 (remainder remaining 3))
        (iter (/ remaining 3) (+ result 1))
        result))
  (iter z 0))

(check-equal? (cons 1 1) 6)
(check-equal? (car (cons 1 1)) 1)
(check-equal? (cdr (cons 1 1)) 1)

(check-equal? (cons 2 2) 36)
(check-equal? (car (cons 2 2)) 2)
(check-equal? (cdr (cons 2 2)) 2)

(check-equal? (cons 5 6) 23328)
(check-equal? (car (cons 5 6)) 5)
(check-equal? (cdr (cons 5 6)) 6)