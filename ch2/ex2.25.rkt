#lang sicp

(#%require rackunit
           racket/trace)


(check-equal? (cadr (caddr '(1 3 (5 7) 9)))
              7)

(check-equal? (caar '((7)))
              7)

(check-equal? (cadr
               (cadr
                (cadr
                 (cadr
                  (cadr
                   (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))
              7)