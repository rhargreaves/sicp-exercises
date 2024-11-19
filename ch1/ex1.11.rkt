#lang sicp

(#%require rackunit)
(#%require racket/trace)

; f(n) = n (when n < 3)
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) (when n >= 3)

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(check-equal? (f 0) 0)
(check-equal? (f 1) 1)
(check-equal? (f 2) 2)
(check-equal? (f 3) 4)
(check-equal? (f 4) 11)
(check-equal? (f 5) 25)
(check-equal? (f 6) 59)

; f: like fib but now with 3 args: (remembering last 2 calcs)
; a <-- a + 2b + 3c
; b <-- a
; c <-- b

; initial values (n = 3)
; f(n - 1) = f(2) = 2
; f(n - 2) = f(1) = 1
; f(n - 3) = f(0) = 0
; res = 2 + 2(1) + 3(0) = 4

; a = 2, b = 1, c = 0

(define (f2 n)
  (define (f2-loop a b c count)
    (if (= count n)
        a
        (f2-loop (+ a (* 2 b) (* 3 c)) a b (+ count 1))))  
  (if (< n 3)
      n
      (f2-loop 2 1 0 2)))

(check-equal? (f2 0) 0)
(check-equal? (f2 1) 1)
(check-equal? (f2 2) 2)
(check-equal? (f2 3) 4)
(check-equal? (f2 4) 11)
(check-equal? (f2 5) 25)
(check-equal? (f2 6) 59)

