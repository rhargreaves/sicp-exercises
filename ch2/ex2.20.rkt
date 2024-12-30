#lang sicp

(#%require rackunit
           racket/trace)

(check-equal? (remainder 1 2) 1)
(check-equal? (remainder 2 2) 0)
(check-equal? (remainder 3 2) 1)

(define (equal-parity? parity n)
  (= (remainder parity 2)
     (remainder n 2)))

(check-equal? (equal-parity? 1 2) #f)
(check-equal? (equal-parity? 2 2) #t)
(check-equal? (equal-parity? 3 3) #t)
(check-equal? (equal-parity? 3 1) #t)

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (same-parity parity . rest)
   (cons parity
         (filter (lambda (n) (equal-parity? parity n))
                 rest)))

(check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
(check-equal? (same-parity 2 3 4 5 6 7) '(2 4 6))

