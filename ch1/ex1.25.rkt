#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

; grows logarithmically with steps & space
(define (fast-expt b n)
  (cond ((= n 0) 1 )
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (square (expmod base (/ exp 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (expmod base (- exp 1) m))
;;                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(trace fast-prime?)
(trace expmod)
(trace fast-expt)


(check-equal? (fast-prime? 19999 100) #f)
;(check-equal? (fast-prime? 19 100) #t)
;(check-equal? (fast-prime? 199 100) #t)
;(check-equal? (fast-prime? 1999 100) #t)
;(check-equal? (fast-prime? 19999 100) #f)

; expmod with fast-expt calculates with the full exp, resulting in a large
; intermediate value. This can lead to slower bignum-type operations.
