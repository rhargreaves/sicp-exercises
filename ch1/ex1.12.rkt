#lang sicp

(#%require rackunit)
(#%require racket/trace)

;0:   1
;1:   1   1
;2:   1   2   1
;3:   1   3   3   1
;4:   1   4   6   4   1
;x:         . . .

(define (pascal row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))

(check-equal? (pascal 0 0) 1)

(check-equal? (pascal 1 0) 1)
(check-equal? (pascal 1 1) 1)

(check-equal? (pascal 2 0) 1)
(check-equal? (pascal 2 1) 2)
(check-equal? (pascal 2 2) 1)

(check-equal? (pascal 3 0) 1)
(check-equal? (pascal 3 1) 3)
(check-equal? (pascal 3 2) 3)
(check-equal? (pascal 3 3) 1)

(check-equal? (pascal 4 0) 1)
(check-equal? (pascal 4 1) 4)
(check-equal? (pascal 4 2) 6)
(check-equal? (pascal 4 3) 4)
(check-equal? (pascal 4 4) 1)

(trace pascal)
(pascal 4 2)