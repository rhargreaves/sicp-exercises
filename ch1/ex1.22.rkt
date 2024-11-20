#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

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

(search-for-primes 1001 9999) ; 1009, 1013, 1019 - 2, 1, 1 ms
(search-for-primes 10001 99999) ; 10007, 10009, 10037 - 4, 4, 3 ms
(search-for-primes 100001 999999) ; 100003, 100019, 100043 - 11, 10, 10 ms

; sqrt(10) ~~ 3.2
; from 1000 to 10000: 1.3 x 3.2 = 4.16 ms. Matches prediction.
; from 10000 to 100000: 3.67 x 3.2 = 11.7 ms. Matches prediction.
; yes - computational time is proportional to number of steps
      
  