#lang sicp

(#%require rackunit
           racket/trace)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (if (null? items)
      nil
  (append (reverse (cdr items))
          (cons (car items) nil))))

(check-equal? (append '(1 2) '(3 4)) '(1 2 3 4))
(check-equal? (reverse '()) '())
(check-equal? (reverse (list 1 2 3)) (list 3 2 1))
(check-equal? (reverse (list 1 2 3 4)) (list 4 3 2 1))
