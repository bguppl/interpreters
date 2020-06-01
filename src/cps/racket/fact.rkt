#lang racket

(require racket/trace)

(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))

(define control-context-of-embedded-fact
  (lambda (fact-n-1)
    (* n fact-n-1)))

   (lambda (fact-n-1)
    (if (= n 0)
        1
        (* n fact-n-1)))

(trace fact)

(define fact-iter
  (lambda (n acc)
    (if (= n 1)
        acc
        (fact-iter (- n 1) (* n acc)))))

(trace fact-iter)

(define fact$
  (lambda (n cont)
    (if (= n 1)
        (cont 1)
        (fact$ (- n 1) 
                (lambda (res) (cont (* n res)))))))

(trace fact$)

(define integers-from
  (lambda (n)
    (cons-lzl n (lambda () (integers-from (+ n 1))))))

(define lzl-map
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                  (lambda () (lzl-map f (tail lzl)))))))

(define lzl-filter
  (lambda (p lzl)
    (cond ((empty-lzl? lzl) lzl)
          ((p (head lzl)) (cons-lzl (head lzl) 
                                    (lambda () (lzl-filter p (tail lzl)))))
          (else (lzl-filter p (tail lzl))))))

(define even-square-1
  (lzl-filter (lambda (x) (= (modulo x 2) 0))
              (lzl-map (lambda (x) (* x x))
                        (integers-from 0))))

(define even-square-2
  (lzl-map (lambda (x) (* x x))
              (lzl-filter (lambda (x) (= (modulo x 2) 0))
                        (integers-from 0))))


