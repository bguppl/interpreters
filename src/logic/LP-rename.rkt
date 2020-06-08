#lang racket
(require "substitution-ADT.rkt" "LP-ast.rkt")
(provide (all-defined-out))


;; ============================================================
;; LP Renaming

; Signature: fresh-var(var)
; Type: [Variable -> Variable]
; Purpose: Create a new variable that has not been used before
; Pre-conditions: -
; Tests: -
(define fresh-var
  (let ((n 0))
    (lambda (var)
      (set! n (+ n 1))
      (rename-variable var n))))

; Signature: fresh-vars(vars)
; Type: [List(Variable) -> List(Variable)]
; Purpose: Create a new variable that have not been used before for each var in vars
; Pre-conditions: -
; Tests: -
(define fresh-vars
  (lambda (vars)
    (map fresh-var vars)))

; Signature: rename-rule(rule)
; Type: [Rule -> Rule]
; Purpose: Return a copy of rule with all variables renamed consistently
; Pre-conditions: -
; Tests: 
; (rename-rule (make-rule '(member (var X) (cons (var H) (var L))) '((member (var X) (var L)))))
; => '((member (var X 2) (cons (var H 2) (var L 2))) ((member (var X 2) (var L 2))))
(define rename-rule
  (let ((n 0))
    (lambda (rule)
      (set! n (+ n 1))
      (let ((vars (rule->vars rule)))
        (let ((new-vars (map (lambda (var) (rename-variable var n)) vars )))
          (sub-apply-rule (make-sub vars new-vars) rule))))))

; Signature: rename-procedure(procedure)
; Type: [Procedure -> Procedure]
; Purpose: Rename all variables in procedure in a consistent manner
; Pre-conditions: -
; Tests: -
(define rename-procedure
  (lambda (procedure)
    (let ((rules (procedure->rules procedure)))
      (make-procedure (map rename-rule rules)))))

; Signature: rename-program(program)
; Type: [Program -> Program]
; Purpose: Rename all variables in program in a consistent manner
; Pre-conditions: -
; Tests: -
(define rename-program
  (lambda (program) 
    (make-program (map rename-procedure (program->procedures program)))))


