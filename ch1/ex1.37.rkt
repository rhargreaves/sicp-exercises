#lang sicp

(#%require rackunit
           racket/trace)

; a)

(define (cont-frac n d k)
  (cond ((= k 0) (/ (n k) (d k)))
        (else
         (/ 1 (+ (d k)
                 (cont-frac n d (- k 1)))))))

(define (golden-ratio) 1.6180)
; (/ 1 (golden-ratio)) ; = 0.6180339887498948

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           9)
; 9 iterations to be accurate to 4 d.p.

; b)


(define (cont-frac-iter n d k)
  (define (iter n d i result)
    (cond ((< i 0) result)
          ((= i k) (iter n d (- i 1) (/ (n i) (d i))))
          (else
           (iter n d (- i 1) (/ (n i) (+ (d i) result))))))
  (iter n d k 0))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                9)
