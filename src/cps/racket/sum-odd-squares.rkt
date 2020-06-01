#lang racket
(require racket/trace)

(define square
  (lambda (x) (* x x)))

(define sum-odd-squares
  (lambda (tree)
    (cond ((empty? tree) 0)
          ((not (list? tree))
           (if (odd? tree) (square tree) 0))
          (else (+ (sum-odd-squares (car tree))
                   (sum-odd-squares (cdr tree)))))))

(trace sum-odd-squares)

#|
(define sum-odd-squares$
  (lambda (tree cont)
    (cond ((empty? tree) (cont 0))
          ((not (list? tree))
           (if (odd? tree) (square$ tree cont) (cont 0)))
          (else <<CPS-transform (+ (sum-odd-squares (car tree))
                                   (sum-odd-squares (cdr tree)))>> ))))

Let us now consider the consequent of the last branch.
There are two user-procedure calls which are in non-tail position.
We pick the first one, and turn it as the first call of the CPS.

(define sum-odd-squares$
  (lambda (tree cont)
    (cond ((empty? tree) (cont 0))
          ((not (list? tree))
           (if (odd? tree) (square$ tree cont) (cont 0)))
          (else (sum-odd-squares$ (car tree)
                  (lambda (sum-odd-squares-car-res)
                    <<CPS-transform (+ sum-odd-squares-car-res (sum-odd-squares (cdr tree)))>> ))))))

We have pushed the CPS transformation inside - let us complete it:
|#

(define square$
  (lambda (x cont) (cont (* x x))))

(define sum-odd-squares$
  (lambda (tree cont)
    (cond ((empty? tree) (cont 0))
          ((not (list? tree))
           (if (odd? tree) (square$ tree cont) (cont 0)))
          (else (sum-odd-squares$ (car tree)
                  (lambda (sum-odd-squares-car-res)
                    (sum-odd-squares$ (cdr tree)
                      (lambda (sum-odd-square-cdr-res)
                        (cont (+ sum-odd-squares-car-res sum-odd-square-cdr-res))))))))))

(define id (lambda (x) x))
(trace sum-odd-squares$)
