#lang racket

(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equation ADT
;; An equation is implemented as a tagged list of two terms.

;; Constructors:

;; Signature: make-equation(term1, term2)
;; Type: [Term * Term -> Equation]
;; Purpose: Equation value constructor
;; Pre-conditions: -
;; Tests:
;;  (make-equation '(f (var X)) '(f 2))
;;  ==> '(equation (f (var X)) (f 2))
(define make-equation
  (lambda (term1 term2)
    (list 'equation term1 term2)))

;; Signature: equation?(eq)
;; Type: [T -> Boolean]
;; Purpose: membership predicate for Equation
;; Pre-conditions: -
;; Tests: -
(define equation?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (car list) 'equation))))

;; Signature: equation->left(eq)
;; Type: [Equation -> Term]
;; Purpose: Accessor for equation
;; Pre-conditions: -
;; Tests: (equation->left '(equation (f (var X)) (f 2)))
;;         ==> '(f (var X))
(define equation->left
  (lambda (eq)
    (second eq)))

;; Signature: equation->right(eq)
;; Type: [Equation -> Term]
;; Purpose: Accessor for equation
;; Pre-conditions: -
;; Tests: (equation->right '(equation (f (var X)) (f 2)))
;;         ==> '(f 2)
(define equation->right
  (lambda (eq)
    (third eq)))
