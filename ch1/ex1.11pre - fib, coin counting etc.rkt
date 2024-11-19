#lang sicp

; 1.2.2 Tree Recursion notes

; fibonacci has exponential complexity (steps grows exponentially with input).
; However, space required is only linearly with n (as we only need to remember those outputs above us in the tree)

(#%require rackunit)
(#%require racket/trace)

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(trace fib)
(trace fib-iter)
(fib 10)

; Change count

; 10c using 1c and 5c:

; 10c = 5c + rem (5c)
; 5c = 5c or 5x1c
; 1c = 1c

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                         (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;(trace cc)
;(count-change 10)