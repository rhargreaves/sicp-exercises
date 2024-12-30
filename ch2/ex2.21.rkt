#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(check-equal? (square-list '(1 2 3 4)) '(1 4 9 16))

(define (square-list2 items)
  (map square items))

(check-equal? (square-list2 '(1 2 3 4)) '(1 4 9 16))