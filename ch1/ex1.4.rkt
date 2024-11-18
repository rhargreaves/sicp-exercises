#lang sicp

(#%require rackunit)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(check-equal? (a-plus-abs-b 2 -2) 4)
(check-equal? (a-plus-abs-b 2 -2) 4)