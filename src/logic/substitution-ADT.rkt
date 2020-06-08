#lang racket

(require "auxiliary.rkt" "LP-ast.rkt")

(provide (all-defined-out))


;;;;;;;;;;; Implementation of the Substitution ADT ;;;;;;;;;;;;;;;;;;
;; A substitution is represented as a 2 element list of equal length
;; lists of variables and terms.
;; The empty substitution is the list (() ()).

;; Constructors:

; Signature: make-sub(variables, terms)
; Type: [LIST(Variable)*LIST(Term) -> Substitution]
; Purpose: Create a substitution in which the i-th element of 'variables'
;          is mapped to the i-th element of 'terms'.
; Pre-conditions: (length variables) = (length terms)
;                 @@variables has no repetitions (set)
; Tests: (make-sub '((var x) (var y) (var z))
;                  '(a 1 (var t)))
;          ==> '(sub ((var x) (var y) (var z)) (a 1 (var t)))
;          (make-sub '((var x)) '((f (var x))))
;          ==> error make-sub: circular substitution:
;              (((var x))
;               ((f (var x))))
(define make-sub
  (lambda (variables terms)
    ;; if any variable substituting expression is circular
    (if (ormap occur? variables terms)
        (error 'make-sub "circular substitution: variables are ~s, terms are ~s"
               variables terms)
        (list 'sub variables terms))))

; Signature: occur?(var, term)
; Type: [Variable * Term -> Boolean]
; Purpose: check whether var occurs in term
; Pre-conditions: -
; Tests:
; (occur? '(var X) '(f a b)) => #f
; (occur? '(var X) '(f (var X) b)))  => #t
(define occur?
  (lambda (var term)
    (cond ((symbol? term) #f)
          ((number? term) #f)
          ((variable? term) (equal? var term))
          ((compound-term? term)
           (ormap (lambda (t) (occur? var t)) (compound-term->args term)))
          (else (error 'occur? "bad term ~s" term)))))

(define make-empty-sub
  (lambda ()
    (make-sub '() '())))

(define empty-sub (make-empty-sub))

;; Selectors

; Signature: sub->variables(sub)
; Type: [Sub -> LIST(Variable)]
; Purpose: Accessor of sub
; Pre-conditions: -
; Tests: (sub->variables
;            (make-sub '((var x) (var y)) '(1 2)))
;           ==> '((var x) (var y))
(define sub->variables
  (lambda (sub)
    (second sub)))

; Signature: sub->terms(sub)
; Type: [Sub -> LIST(Term)]
; Purpose: Accessor of sub
; Pre-conditions: -
; Tests: (sub->terms
;           (make-sub '((var x) (var y)) '(1 2)))
;          ==> '(1 2)
(define sub->terms
  (lambda (sub)
    (third sub)))

; Signature: sub->get-var(sub,var)
; Type: [Sub * Variable -> Term]
; Purpose: Get the value of a variable in the substitution
; Pre-conditions: sub is a non empty substitution that includes var.
; Tests: (sub->get-var
;            (make-sub '((var x) (var y)) '(1 2)) '(var y))) ==> 2
(define sub->get-var
  (lambda (sub var)
    (letrec
        ((lookup
          (lambda (vars terms)
            (cond ((or (empty-sub? sub) (not (member var vars)))
                   (error 'sub->expression-of-variable
                     "var is not a variable of sub: sub is ~s. var is ~s"
                     sub var))
                  ((equal? var (car vars)) (car terms))
                  (else (lookup (cdr vars) (cdr terms)))))))
      (lookup (sub->variables sub) (sub->terms sub)))))


; Signature: extend-sub(sub,var,term)
; Type: [Sub * Variable * Term -> Sub]
; Purpose: compute a new sub sub' where sub'[var] = term and for all other v in sub->variables(sub), sub'[var] = sub[var]
; Tests:
; (extend-sub
;   (make-sub '((var X1) (var X2))
;             '(1 2))
;   '(var S1)
;   'a) ==>
; '(sub ((var S1) (var X1) (var X2))
;       (a 1 2))
;    
; (extend-sub
;   (make-sub '((var T1) (var T2))
;             '(1 (var S1))) 
;   '(var S1)
;   'a) ==>
;  '(sub ((var S1) (var T1) (var T2))
;        (a 1 a))  --- NOTE the substitution of (var S1)
;
(define extend-sub
  (lambda (sub var term)
    (cond ((occur? var term)
           (error 'extend-sub
                  "Var occurs in term. Var is ~s, term is ~s"
                  var term))
          ((empty-sub? sub) (make-sub (list var) (list term)))
          (else
           (let ((vars (sub->variables sub))
                 (terms (sub->terms sub))
                 (new-sub (make-sub (list var) (list term))))
             (if (member var vars)
                 (make-sub
                  vars
                  (map (lambda (sub-term)
                         (sub-apply new-sub sub-term))
                       terms))
                 (make-sub
                  (cons var vars)
                  (cons term
                        (map (lambda (sub-term)
                               (sub-apply new-sub sub-term))
                             terms)))))))))


;; ============================================================
;; Membership predicates

; Signature: sub?(sub)
; Type: [T -> Boolean]
; Purpose: Membership predicate
; Pre-conditions: -
; Tests:
; (sub? (make-sub '(x y z) (map te-parse '(T1 T2 (T1 -> T2))))) => #t
; (sub? (make-sub (list) (list))) ==> #t
; (sub? '()) ==> #f
(define sub?
  (lambda (sub)
    (and (list? sub)
         (= (length sub) 3)
         (eq? (car sub) 'sub)
         (let ((vars (second sub))
               (terms (third sub)))
           (and (list? vars)
                (andmap variable? vars)
                (list? terms)
                (andmap term? terms)
                (= (length vars) (length terms)))))))


; Signature: empty-sub?(sub)
; Type: [T -> Boolean]
; Purpose: Membership predicate
; Pre-conditions: -
; Tests:
; (empty-sub? (make-sub '((var x)) '(1))) ==> #f
; (empty-sub? (make-sub (list) (list))) ==> #t
(define empty-sub?
  (lambda (sub)
    (and (sub? sub)
         (empty? (sub->variables sub))
         (empty? (sub->terms sub)))))

; Signature: non-empty-sub?(sub)
; Type: [T -> Boolean]
; Purpose: Membership predicate
; Pre-conditions: -
; Tests:
; (non-empty-sub? (make-sub '((var x)) '(1))) ==> #t
; (non-empty-sub? (make-sub (list) (list))) ==> #f
(define non-empty-sub?
  (lambda (sub) (and (sub? sub) (not (empty-sub? sub)))))


;; ============================================================
;; Equality

; Signature: sub-equal?(sub1,sub2)
; Type: Client view: [Sub * Sub -> Boolean]
; Purpose: Check that 2 substitutions are equal - ignore order of vars.
; Pre-conditions: -
; Tests:
; (sub-equal? (make-sub '((var x) (var y))
;                       '(1 2))
;             (make-sub '((var y) (var x))
;                       '(2 1)))
; ==> #t
; (sub-equal? (make-sub (list) (list)) (make-sub (list) (list))) ==> #t
(define sub-equal?
  (lambda (sub1 sub2)
    (and (sub? sub1)
         (sub? sub2)
         (set-equal? (sub->variables sub1) (sub->variables sub2))
         (andmap (lambda (var)
                   (equal? (sub->get-var sub1 var)
                           (sub->get-var sub2 var)))
                 (sub->variables sub1)))))

; ============================================================
; Signature: sub-apply(sub,term)
; Type: [Sub * Term -> Term]
; Purpose: apply sub on term (return term with variables of sub replaced by their values in sub)
; Tests:
; (sub-apply
;   (make-sub '((var T1) (var T2))
;             '((f (var T2)) 2))
;   '(f (var T1) (var T2))))
; ==>  '(f (f (var T2)) 2)
(define sub-apply
  (lambda (sub term)
    (cond ((empty-sub? sub) term)
          ((symbol? term) term)
          ((number? term) term)
          (else
           (let ((vars (sub->variables sub))
                 (terms (sub->terms sub))
                 (sub-apply-this (lambda (term) (sub-apply sub term))))
             (cond ((variable? term)
                    (if (member term vars)
                        (sub->get-var sub term)
                        term))
                   ((compound-term? term)
                    (make-compound-term
                     (compound-term->functor term)
                     (map sub-apply-this (compound-term->args term))))
                   (else term)))))))

(define sub-apply-atomic-formula
  (lambda (sub atomic-formula)
    (cond ((eq? atomic-formula 'true) atomic-formula)
          ((eq? atomic-formula 'false) atomic-formula)
          (else (let ((args (predication->args atomic-formula))
                      (pred (predication->pred atomic-formula)))
                  (make-predication pred 
                                    (map (lambda (term) (sub-apply sub term))
                                         args)))))))

(define sub-apply-query
  (lambda (sub query)
    (make-query (map (lambda (atomic-formula) (sub-apply-atomic-formula sub atomic-formula))
                     (query->goals query)))))


(define sub-apply-rule
  (lambda (sub rule)
    (make-rule (sub-apply-atomic-formula sub (rule->head rule))
               (map (lambda (af) (sub-apply-atomic-formula sub af)) 
                    (rule->body rule)))))

; Signature: sub-restrict(sub, vars)
; Type: [Sub * List(Variable) -> Sub]
; Purpose: return a sub s such that s(v) = sub(v) for all v in vars, and s is not defined on any other var.
; Pre-conditions: all vars are defined in sub.
; Tests: -
(define sub-restrict
  (lambda (sub vars)
    (make-sub vars
              (map (lambda (var) (sub->get-var sub var))
                   vars))))
    
;; ============================================================
;; Signature: sub-combine(sub1,sub2)
;; Type: [Sub * Sub -> Sub]
;; Purpose: Returns the composition of substitutions s.t.:
;;  (sub-apply result term) === (sub-apply sub2 (sub-apply sub1 term))
;; Pre-conditions: -
;; Tests:
;; (sub-combine
;;   (make-sub '((var T1) (var T2))
;;             '((var S1) 2))
;;   (make-sub '((var S1))
;;             '(3))) ==>
;; '(sub ((var S1) (var T1) (var T2))
;;       (3 3 2))

(define sub-combine
  (lambda (sub1 sub2)
    (cond ((empty-sub? sub1) sub2)
          ((empty-sub? sub2) sub1)
          (else (letrec ((combine
                          (lambda (sub vars terms)
                            (if (empty? vars)
                                sub
                                (combine
                                 (extend-sub sub (car vars) (car terms))
                                 (cdr vars) (cdr terms))))))
                  (combine sub1 (sub->variables sub2) (sub->terms sub2)))))))


; ============================================================
; Signature: normalize-vars(sub)
; Type: [Sub -> Sub]
; Purpose: Renamed variables appear with arbitrary numbers - like (var X 137)
;          this makes it difficult to write tests that expect renamed variables.
;          normalize-sub replaces the numbers with systematic numbers 1,2,3... in a 
;          predictable manner.
; Pre-conditions: -
; Tests:
; (normalize-vars (make-sub '((var X) (var Y)) '((var X 137) (var X 21))))
; =>
; '(sub ((var X) (var Y)) '((var X 2) (var X 1)))
; (normalize-vars (make-sub '((var X) (var Y)) '(1 (p 5))))
; =>
; '(sub ((var X) (var Y)) (1 (p 5)))
(define normalize-vars
  (lambda (sub)
    (let* ((terms (sub->terms sub))
           (nvars (remove-duplicates (filter renamed-variable? (flatmap term->vars terms))))
           (range (range 1 (length nvars)))
           (sorted-nvars (sort nvars < #:key third))
           (normalized-nvars (map rename-variable sorted-nvars range)))
      (sub-restrict
       (sub-combine sub (make-sub sorted-nvars normalized-nvars))
       (sub->variables sub)))))