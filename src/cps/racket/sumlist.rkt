#lang racket
(require racket/trace)
(define id (lambda (x) x))

;; Signature: sumlist(li)
;; Purpose: Sum the elements of a number list. 
;; If the list includes a non number element -- produce an error.
;; Type: [List -> Number union ???]
(define sumlist
  (lambda (li)
    (cond ((empty? li) 0)
          ((not (number? (car li))) (error "non numeric value!"))
          (else (+ (car li) (sumlist (cdr li)))))))

(trace sumlist)

(define sumlist2
  (lambda (li)
    (letrec ((sumlist$
               ;; [List * [Number -> Number] * [Empty -> Void] -> (Number | Void)]
               (lambda (li succ-cont fail-cont)
                 (cond ((empty? li) (succ-cont 0))
                       ((number? (car li))
                        (sumlist$ (cdr li)
                                  (lambda (sum-cdr) ;success continuation
                                    (succ-cont (+ (car li) sum-cdr)))
                                  fail-cont))
                       ;; error condition: invoke error handler
                       (else (fail-cont))))))
      (trace sumlist$)
      (sumlist$ li
                 (lambda (x) x)
                 (lambda () (display "non numeric value!"))))))

;; In the CPS style, we handle errors by using continuations -
;; instead of using one continuation as we did in the transformation discussed above,
;; we carry 2 continuations: one for the successful computation path, and one for error conditions.
;; This is similar to the definition of Optional with the either operator in TypeScript


