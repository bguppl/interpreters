#lang racket
(require racket/trace)
(define id (lambda (x) x))

#|
(define filter
  (lambda (pred? list)
    (cond ((empty? list) list)
          ((pred? (car list)) 
           (cons (car list) (filter pred? (cdr list))))
          (else (filter pred? (cdr list))))))

The new configuration we meet is that the test part of the second branch is a user-procedure application (pred? (car list)).
In this case, we must change the structure of the cond so that we first compute the test value outside the cond,
then route the conditional inside the continuation:

(define filter$
  (lambda (pred? list cont)
    (cond ((empty? list) (cont list))
          (else (pred?$ (car list)
                  (lambda (pred-res)
                    <<CPS-transformation 
                    (cond (pred-res (cons (car list) (filter pred? (cdr list))))
                          (else (filter pred? (cdr list))))>> ))))))

The transformation here is that the cond structure is split -
in branches which can be evaluated all in tail-form and those which follow the test evaluation.

|#

(define even?$
  (lambda (x cont) (cont (even? x))))

(define filter$
  (lambda (pred?$ list cont)
    (cond ((empty? list) (cont list))
          (else (pred?$ (car list)
                  (lambda (pred-res)
                    (cond (pred-res (filter$ pred?$ (cdr list)
                                      (lambda (filter-cdr-res)
                                        (cont (cons (car list) filter-cdr-res)))))
                          (else (filter$ pred?$ (cdr list) cont)))))))))

(trace filter$ even?$)

