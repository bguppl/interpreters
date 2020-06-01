#lang racket

#|
(define map
  (lambda (f list)
    (if (empty? list)
        list
        (cons (f (car list))
              (map f (cdr list))))))

This procedure includes two user-procedure calls, nested within a cons application - hence not in tail position.
Therefore, the process is not iterative.
The two nested user procedure calls appear in the arguments of the cons application.

The test of the if-exp and the then-part are all primitive combinations, hence,
no CPS transformation is required besides wrapping the tail value in the cont call.

We select the first argument of cons as the first nested call and move it as the first step of the CPS-transformed version:

(define map$
  (lambda (f$ list cont)
    (if (empty? list)
        (cont list)
        (f$ (car list)
            (lambda (f-res)
              <<CPS-transform (cons f-res (map f (cdr list)))>> )))))

In this transformation, we consider that the f parameter we receive is a user procedure in CPS format as well.
This is necessary, because CPS as a discipline is an all-or-nothing proposition:
all user-defined procedures must be transformed in CPS to be able to obtain a coherent CPS program.

We next observe the remaining segment to be transformed in CPS:
(cons f-res (map f (cdr list))).

The first sub-expression to be evaluated in this context is:
(map f (cdr list))
which we push outside as the first step of the CPS transformation - and we eventually obtain:
|#

(require racket/trace)
(define id (lambda (x) x))
(define square$
  (lambda (x cont) (cont (* x x))))

(define map$
  (lambda (f$ list cont)
    (if (empty? list)
        (cont list)
        (f$ (car list)
            (lambda (f-car-res)
              (map$ f$ (cdr list)
                (lambda (map-f-cdr-res)
                  (cont (cons f-car-res map-f-cdr-res)))))))))

(trace map$)
(trace square$)
