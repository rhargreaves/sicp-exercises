#lang sicp

(#%require rackunit
           racket/trace)

(define (tan-cf x k)
  (define (cont-frac n d k)
    (define (recur n d i)
      (cond ((= i k) (/ (n i) (d i)))
            (else
             (/ 1 (- (d i)
                     (recur n d (+ i 1)))))))
    (recur n d 0))
  (cont-frac (lambda (i) (if (= i 0) x (* x x)))
             (lambda (i) (+ (* i 2) 1))
           k))

(check-equal? (tan 1) (tan-cf 1.0 10))