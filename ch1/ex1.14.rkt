#lang sicp

(#%require rackunit)
(#%require racket/trace)

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

; graph traced below

(trace cc)
(cc 11 5)

; space required = max. depth of tree (which is 11 for pennies, plus the 5 kinds of coins)
; Big O(n)

; number of steps
; Big Theta(n ^ x) where x is the number of kinds of coins