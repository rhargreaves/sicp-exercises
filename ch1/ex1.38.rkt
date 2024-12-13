#lang sicp

(#%require rackunit
           racket/trace)

(define (cont-frac n d k)
  (define (recur n d i)
    (cond ((= i k) (/ (n i) (d i)))
          (else
           (/ 1 (+ (d i)
                   (recur n d (+ i 1)))))))
  (recur n d 0))

(define true-e 2.718281828459045)                                        

(check-equal?
 (cont-frac (lambda (i) 1.0)
            (lambda (i) (cond ((= i 0) 1)
                              ((= (remainder i 3) 1) (* (+ 2 i) (/ 2 3)))
                              (else 1)))
            10)
 0.7182818352059925)

