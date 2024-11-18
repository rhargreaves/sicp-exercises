#lang sicp

(#%require rackunit)

(define (sum-square x y)
  (+ (* x x) (* y y)))

;; old solution which finds the two largest numbers
;; (define (solution a b c)
;;   (cond ((and (>= b c) (or (>= a b) (>= a c))) (sum-square a b))
;;         ((and (>= a b) (or (>= c b) (>= c a))) (sum-square a c))
;;         (else (sum-square b c))
;;       ))

;; better solution which excludes the smallest number
(define (solution a b c)
  (cond ((and (<= a b) (<= a c)) (sum-square b c))
        ((and (<= b c) (<= b a)) (sum-square a c))
        (else (sum-square a b))
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