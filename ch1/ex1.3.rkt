#lang sicp

(#%require rackunit)

(define (sum-square x y)
  (+ (* x x) (* y y)))

(define (solution a b c)
  (cond ((and (>= a c) (>= b c)) (sum-square a b))
        ((and (>= a b) (>= c b)) (sum-square a c))
        (else (sum-square b c))
      ))

; tests
(test-case "distinct values" 
           (check-equal? (solution 1 2 3) 13)
           (check-equal? (solution 1 3 2) 13)
           (check-equal? (solution 2 1 3) 13)
           (check-equal? (solution 2 3 1) 13)
           (check-equal? (solution 3 1 2) 13)
           (check-equal? (solution 3 2 1) 13))

(test-case "some equal values" 
           (check-equal? (solution 2 2 3) 13)
           (check-equal? (solution 2 3 2) 13)
           (check-equal? (solution 3 2 2) 13))

(test-case "all equal values" 
           (check-equal? (solution 2 2 2) 8))