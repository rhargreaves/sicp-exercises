#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(check-equal? (fast-prime? 7 100) #t)
(check-equal? (fast-prime? 19 100) #t)
(check-equal? (fast-prime? 199 100) #t)
(check-equal? (fast-prime? 1999 100) #t)
(check-equal? (fast-prime? 19999 100) #f)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n limit)
  (if (> n limit)
      (display " done ")
      (begin
        (timed-prime-test n)
        (search-for-primes (+ n 2) limit))))

; prime? previous (with next divisor to test = n + 1)
;(search-for-primes 1001 9999) ; 1009, 1013, 1019 - 2, 1, 1 ms = 1.33 ms
;(search-for-primes 10001 99999) ; 10007, 10009, 10037 - 4, 4, 3 ms = 3.67 ms
;(search-for-primes 100001 999999) ; 100003, 100019, 100043 - 11, 10, 10 ms = 10.33 ms 

; prime? new (next divisor = n + 2)
;(search-for-primes 1001 1050) ; 1009, 1013, 1019 - 2, 2, 1 ms = 1.66 ms
;(search-for-primes 10001 10050) ; 10007, 10009, 10037 - 3, 3, 2 ms = 2.66 ms
;(search-for-primes 100001 100050) ; 100003, 100019, 100043 - 8, 8, 8 ms = 8 ms

; fast-prime? (Fermet's Little Theorem);
(search-for-primes 1001 1050) ; 1009, 1013, 1019 - 93, 110, 104 ms = 102.3 ms
(search-for-primes 10001 10050) ; 10007, 10009, 10037 - 114, 124, 128 ms = 122 ms
(search-for-primes 100001 100050) ; 100003, 100019, 100043 - 132, 137, 143 ms = 137 ms
; goes up roughly 20 ms every digit added (logarithmic)


