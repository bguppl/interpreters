#lang racket

(require "term-equation-adt.rkt"
         "substitution-adt.rkt"
         "LP-ast.rkt"
         "auxiliary.rkt"
         )
(provide (all-defined-out))

;; Signature: unify-formulas(af1, af2)
;; Type: [AtomicFormula * AtomicFormula -> Sub]
;; Purpose: Compute the unifier of 2 atomic formulas - return fail if none exists.
;; Pre-conditions: -
;; Tests: (unify-formulas '(member (f (var X1)) (cons (f 2) empty)) '(member (var X) (var L)))
;;          ==> '(sub ((var X) (var L))
;;                    ((f (var X1)) (cons (f 2) empty)))
;;        (unify-formulas 'true 'true) ==> '(sub () ())
;;        (unify-formulas 'true 'false) ==> 'fail
(define unify-formulas
  (lambda (af1 af2)
    (cond ((or (eq? af1 'true) (eq? af2 'false))
           (if (eq? af1 af2)
               (make-empty-sub)
               'fail))
          (else 
           (let ((pred1 (predication->pred af1))
                 (pred2 (predication->pred af2))
                 (args1 (predication->args af1))
                 (args2 (predication->args af2)))
             (if (and (eq? pred1 pred2)
                      (= (length args1) (length args2)))
                 (try (lambda () (unify-lterms args1 args2))
                      'fail)
                 'fail))))))


;; Signature: unify-lterms(lterms1, lterms2)
;; Type: [List(Term) * List(Term) -> Sub]
;; Purpose: Compute the unifier of 2 lists of terms of equal length/
;; Pre-conditions: Length(lterms1) = Length(lterms2)
;; Tests: 
(define unify-lterms
  (lambda (lterms1 lterms2)
    (let ((equations (map make-equation lterms1 lterms2)))
      (solve-equations equations))))

;; Signature: unify-terms(term1, term2)
;; Type: [Term * Term -> Sub]
;; Purpose: Compute the unifier of 2 terms.
;; Pre-conditions: -
;; Tests: (unify '(f (var X)) '(f 2))
;;          ==> '(sub ((var X)) (2))
(define unify-terms
  (lambda (term1 term2)
    (let ((equations (list (make-equation term1 term2))))
      (solve-equations equations))))

;; term equation solving

;; Signature: solve-equations(equation-list)
;; Type: [List(Equation) -> Sub]
;; Purpose: Solve the term equations and return the resulting substitution
;;          or error, if not solvable
;; Pre-conditions: -
;; Tests: -
(define solve-equations
  (lambda (equations)
    (solve equations (make-empty-sub))))

;; Signature: solve(equations, substitution)
;; Type: [List(Equation) * Substitution -> Substitution]
;; Purpose: Solve the equations, starting from a given substitution.
;;          Returns the resulting substitution, or error, if not solvable
;; Pre-conditions: -
;; Tests: -
(define solve
  (lambda (equations sub)
    (if (empty? equations)
        sub
        (let ((eq (make-equation
                   (sub-apply sub (equation->left (car equations)))
                   (sub-apply sub (equation->right (car equations))))))
          (letrec
              ((solve-var-eq  ; solve one equation when one side is a variable
                (lambda (var-part other-part)
                  (let ((new-sub (if (equal? var-part other-part)
                                     sub
                                     (sub-combine sub
                                                  (make-sub (list var-part)
                                                            (list other-part))))))
                    (solve (cdr equations) new-sub))))
               (both-sides-atomic?
                (lambda (eq)
                  (and (atomic-term? (equation->left eq))
                       (atomic-term? (equation->right eq)))))
               (handle-both-sides-atomic
                (lambda (eq)
                  (if (equal-atomic-terms? (equation->left eq)
                                           (equation->right eq))
                      (solve (cdr equations) sub)
                      (error
                       'solve
                       "equation contains unequal atomic terms: ~e" eq)))))
            (cond
             ((variable? (equation->left eq))
              (solve-var-eq (equation->left eq) (equation->right eq)))
             ((variable? (equation->right eq))
              (solve-var-eq (equation->right eq) (equation->left eq)))
             ((both-sides-atomic? eq)
              (handle-both-sides-atomic eq))
             ((and (compound-term? (equation->left eq))
                   (compound-term? (equation->right eq))
                   (unifiable-structure eq))
              (solve (append (cdr equations) (split-equation eq)) sub))
             (else (error
                    'solve
                    "equation contains unknown term type: ~s" eq))))))))


;; Signature: unifiable-structure(equation)
;; Type: [Equation -> Boolean]
;; Purpose: Compares the structure of the terms of the equation
;; Pre-conditions: -
;; Tests: -
(define unifiable-structure
  (lambda (eq)
   (let ((left (equation->left eq))
         (right (equation->right eq)))
     (or (and (compound-term? left) (compound-term? right)
              (= (length (compound-term->args left))
                 (length (compound-term->args right)))
              (eq? (compound-term->functor left)
                   (compound-term->functor right)))
         (equal-atomic-terms? left right)))))


;; Signature: split-equation(equation)
;; Purpose: For an equation with unifyable type expressions,
;;          create equations for the corresponding components.
;; Type: [Equation -> List(Equation)]
;; Pre-conditions: (and (composite? (equation->left eq))
;;                      (composite? (equation->right eq))
;;                      (unifiable-structure eq))
;; Example: (split-equation
;;           (make-equation '(f (var T) 1)
;;                          '(f 2 (var R)))) ==>
;;            '((equation (var T) 2)
;;              (equation 1 (var R)))
(define split-equation
  (lambda (eq)
    (letrec ((make-equations-from-components
              ;;create equations from corresponding type expressions
              (lambda (l1 l2) (map make-equation l1 l2))))
      (let ((left (equation->left eq))
            (right (equation->right eq)))
        (make-equations-from-components
         (compound-term->args left)
         (compound-term->args right))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
