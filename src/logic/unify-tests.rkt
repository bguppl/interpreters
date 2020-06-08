#lang racket
(require "utils.rkt" "auxiliary.rkt" "unify.rkt")

(define unify-formulas-tests
  (lambda ()
    (display "unify-formulas-tests:\t")
    (run-tests
     
     ;; unify embedded terms
     (test (unify-formulas '(member (f (var X1)) (cons (f 2) empty)) '(member (var X) (var L)))
           =>
           '(sub ((var L)            (var X)) 
                 ((cons (f 2) empty) (f (var X1)))))
     
     ;; non-predications formulas
     (test (unify-formulas 'true 'true) => '(sub () ()))
     (test (unify-formulas 'true 'false) => 'fail)
     (test (unify-formulas 'true '(member 1 (var L))) => 'fail)
     
     ;; circular failure: occur-check
     (test (unify-formulas '(member (f (var X)) (cons (var X) (var L))) '(member (var X) (cons (var X) (var L))))
           => 'fail)
     
     ;; multiple occurrences of the same variable
     (test (unify-formulas '(p (var X) (var X)) '(p 1 (var Y)))
           => '(sub ((var Y) (var X)) (1 1)))
     
     (test (unify-formulas '(p (var X) (var X)) '(p 1 2))
           => 'fail)
     
     ;; identity
     (test (unify-formulas '(p (var X)) '(p (var X)))
           => '(sub () ()))
     
     )))

(define unify-terms-tests
  (lambda ()
    (display "unify-terms-tests:\t")
    (run-tests
     (test (unify-terms 1 1) => '(sub () ()))
     (test (try (lambda () (unify-terms 1 2)) 'fail) => 'fail)
     (test (try (lambda () (unify-terms 1 'a)) 'fail) => 'fail)
     (test (unify-terms 'a 'a) => '(sub () ()))
     (test (try (lambda () (unify-terms 'a 'b)) 'fail) => 'fail)
     (test (unify-terms '(var X) '(var X)) => '(sub () ()))
     (test (unify-terms '(var X) 1) => '(sub ((var X)) (1)))
     (test (unify-terms '(var X) 'a) => '(sub ((var X)) (a)))
     (test (unify-terms '(var X) '(var Y)) => '(sub ((var X)) ((var Y))))
     (test (unify-terms '(var X) '(f 1)) => '(sub ((var X)) ((f 1))))
     (test (unify-terms '(f (var X) (var X)) '(f 1 (var Y))) => '(sub ((var Y) (var X)) (1 1)))
     (test (unify-terms '(f (g 2) (var X)) '(f (var X) (var Y))) => '(sub ((var Y) (var X)) ((g 2) (g 2))))

     (test (unify-terms '(var X) '(var X 1)) => '(sub ((var X)) ((var X 1))))
     (test (unify-terms '(var X 1) '(f 1)) => '(sub ((var X 1)) ((f 1))))

     )))

(unify-formulas-tests)
(unify-terms-tests)
