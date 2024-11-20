#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(check-equal? (prime? 7) #t)
(check-equal? (prime? 19) #t)
(check-equal? (prime? 199) #t)
(check-equal? (prime? 1999) #t)
(check-equal? (prime? 19999) #f)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n limit)
  (if (> n limit)
      (display " done ")
      (begin
        (timed-prime-test n)
        (search-for-primes (+ n 2) limit)
        )))

; previous (with next divisor to test = n + 1)
;(search-for-primes 1001 9999) ; 1009, 1013, 1019 - 2, 1, 1 ms = 1.33 ms
;(search-for-primes 10001 99999) ; 10007, 10009, 10037 - 4, 4, 3 ms = 3.67 ms
;(search-for-primes 100001 999999) ; 100003, 100019, 100043 - 11, 10, 10 ms = 10.33 ms 

; new (next divisor = n + 2)
(search-for-primes 1001 1050) ; 1009, 1013, 1019 - 2, 2, 1 ms = 1.66 ms
(search-for-primes 10001 10050) ; 10007, 10009, 10037 - 3, 3, 2 ms = 2.66 ms
(search-for-primes 100001 100050) ; 100003, 100019, 100043 - 8, 8, 8 ms = 8 ms

; ratio between new and old = 0.77, 0.72. Not half. Why?
; Additional overhead of separate next function, +, = and if

  