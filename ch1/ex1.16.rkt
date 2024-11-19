#lang sicp

(#%require rackunit)
(#%require racket/trace)

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

; b^n = (b^(n/2))^2    if n is even
; b^n = b.b^(n-1)      if n is odd

(define (fast-expr b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expr b (/ n 2))))
        (else (* b (fast-expr b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

; -----

(trace fast-expr)

;(check-equal? (fast-expr 2 10) 1024)
;(check-equal? (fast-expr 2 32) 4294967296)
;(check-equal? (fast-expr 2 31) 2147483648)

; -----

(define (fast-expr-iter base n)
  (define (loop a b counter)
     (cond ((= counter 0) a)
           ((even? counter) (loop a (square b) (/ counter 2)))
           (else (loop (* a b) b (- counter 1)))))
  (loop 1 base n))

(check-equal? (fast-expr-iter 2 10) 1024)
(check-equal? (fast-expr-iter 2 4) 16)
(check-equal? (fast-expr-iter 2 32) 4294967296)
(check-equal? (fast-expr-iter 2 31) 2147483648)
