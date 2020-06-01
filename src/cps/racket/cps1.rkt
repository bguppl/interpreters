#lang racket
(require racket/trace)

;; Normal style

(define mult
  (lambda (x y) (* x y)))

(define square
  (lambda (x) (* x x)))

(define add1 (lambda (x) (+ x 1)))

(define h2
  (lambda (x y) (mult (square x) (add1 y))))

(trace add1 square mult)

;; In CPS

(define id (lambda (x) x))

(define square$
  (lambda (x cont) (cont (* x x))))

(define add1$
  (lambda (x cont) (cont (+ x 1))))
  
(define mult$
  (lambda (x y cont) (cont (* x y))))

(define h2-1$
  (lambda (x y cont)
    (square$ x
      (lambda (square-res)
        (add1$ y
               (lambda (add1-res) 
                 (mult$ square-res add1-res cont)))))))

(define h2-2$
  (lambda (x y cont)
    (add1$ y
      (lambda (add1-res)
        (square$ x
          (lambda (square-res) 
            (mult$ square-res add1-res cont)))))))

(trace mult$)
(trace add1$)
(trace square$)
