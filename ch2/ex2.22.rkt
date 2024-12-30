#lang sicp

(#%require rackunit
           racket/trace)

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(check-equal? (square-list '(1 2 3 4)) '(1 4 9 16))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

; (check-equal? (square-list-iter '(1 2 3 4)) '(16 4 9 16)) ; -- fails
; The order is reversed due to the way the next 'answer' wraps
; the previous accumulated answer. This results in a list in reverse.
; Swapping the cons arguments results in an invalid list structure.
