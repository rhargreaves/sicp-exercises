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

(define (area-rect r)
  (* (len-segment (adj-seg-rect r)) (len-segment (opp-seg-rect r))))
  
(check-equal?
 (area-rect (make-rect (make-segment (make-point 0 0) (make-point 0 3))
                       (make-segment (make-point 0 3) (make-point 4 3))))
 12)

; alternate representations

(define (make-alt-rect w h)
  (cons w h))

(define (width-rect r)
  (car r))

(define (height-rect r)
  (cdr r))

(define (perimeter-alt-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

(define (area-alt-rect r)
  (* (width-rect r) (height-rect r)))

(check-equal?
 (area-alt-rect (make-alt-rect 3 4))
12)

(check-equal?
 (perimeter-alt-rect (make-alt-rect 3 4))
14)

; width and height of the rect are the only abstractions we need;
; but we could still have a rect created by absolute 2D segments as originally done.
; We need to reimplement width-rect and height-rect to support the original rect:

(define (width-orig-rect r)
  (len-segment (adj-seg-rect r)))

(define (height-orig-rect r)
  (len-segment (opp-seg-rect r)))

(check-equal?
 (width-orig-rect (make-rect (make-segment (make-point 0 0) (make-point 0 3))
                             (make-segment (make-point 0 3) (make-point 4 3))))
3)

(check-equal?
 (height-orig-rect (make-rect (make-segment (make-point 0 0) (make-point 0 3))
                              (make-segment (make-point 0 3) (make-point 4 3))))
4)