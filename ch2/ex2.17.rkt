#lang sicp

(#%require rackunit
           racket/trace)

; (list 1 2 3)
; (cons 1 (cons 2 (cons 3 nil)))

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(check-equal? (last-pair (list 1 2 3 4)) (list 4))