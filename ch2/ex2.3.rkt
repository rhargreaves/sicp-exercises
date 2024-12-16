#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; -----

(define (len-segment s)
  (sqrt (+ (square (- (x-point (end-segment s)) (x-point (start-segment s))))
           (square (- (y-point (end-segment s)) (y-point (start-segment s)))))))

(check-equal? (len-segment (make-segment (make-point 0 0) (make-point 3 4))) 5)

(define (make-rect s1 s2)
  (cons s1 s2))

(define (adj-seg-rect r)
  (car r))

(define (opp-seg-rect r)
  (cdr r))

(define (perimeter-rect r)
  (* 2 (+ (len-segment (adj-seg-rect r))
          (len-segment (opp-seg-rect r)))))

(check-equal?
 (perimeter-rect (make-rect (make-segment (make-point 0 0) (make-point 0 3))
                            (make-segment (make-point 0 3) (make-point 4 3))))
 14)

; ---

(define (area-rect r)
  (* (len-segment (adj-seg-rect r)) (len-segment (opp-seg-rect r))))
  
(check-equal?
 (area-rect (make-rect (make-segment (make-point 0 0) (make-point 0 3))
                       (make-segment (make-point 0 3) (make-point 4 3))))
 12)
