#lang racket

(require "auxiliary.rkt")
(provide (all-defined-out))

;; ============================================================
;; Concrete syntax in (BNF) for Logic Programming:
;; <program>          ->  <procedure>+
;; <procedure         ->  (<rule> | <fact>)+    with identical predicate and arity
;; <rule>             ->  <head> ':-' <body>'.'
;; <fact>             ->  <head>'.'
;; <head>             ->  <atomic-formula>
;; <body>             ->  (<atomic-formula>',')* <atomic-formula>
;; <atomic-formula>   ->  'true' | 'false' | <predication>
;; <predication>      ->  <predicate>'('(<term>',')* <term>')'
;; <predicate>        ->  <constant>
;; <term>         ->  <constant> | <variable> | <compound-term>
;; <constant>         ->  A string starting with a lower case letter or a number.
;; <variable>         ->  A string starting with an upper case letter.
;; <compound-term>    ->    <symbol>'(' (<term>',')* <term> ')'
;; <query>            ->  '?-' (<atomic-formula>',')* <atomic-formula> '.'
;;
;; Abstract syntax:
;; ----------------
;; <program>: program(procedures: List(procedure))
;; <procedure>: procedure(rules: List(rule)) - Note that facts are converted to rules of the form (Head :- true.)
;; <rule>: rule(head: atomic-formula, body: list(atomic-formula), id: number) - id added automatically by the parser to uniquely identify rules for debugging purposes.
;; <atomic-formula>: predication | constant
;; <predication>: predication(predicate: constant, terms: List(term))
;; <term>: constant | variable
;; <constant>: constant(name: symbol)
;; <variable>: variable(name: symbol, number: number) -- number keeps track of renamed variables
;; <query>:  query(goals: List(atomic-formula))


;; ====================================
;; LP-ast-interface:
;;
;;  program: (a list of procedures)
;;    (make-program procedures-set)
;;    (program->predicates program)
;;    (program->procedure program predicate)
;;    (program->procedure->numbered-rules program predicate)
;;    (program->procedure->rule program predicate number)
;;
;;  procedure: (a list of rules with heads for the same pred/n)
;;    (make-procedure rules)
;;    (procedure->predicate procedure)
;;    (procedure->numbered-rules procedure)
;;
;;  rule:
;;     (make-rule head body)
;;     (rule->head rule)
;;     (rule->body rule)
;;
;;  atomic-formula: #t | #f | <predication>
;;     (make-atomic-formula #t)
;;     (make-atomic-formula #f)
;;     (make-predication predicate terms)

;;  variable:
;;     (make-variable name)
;;     (variable->name var)
;;     (rename-variable var number)
;;
;;  compound term:
;;     (make-compound-term functor terms)
;;
;;  query:
;;     (make-query atomic-formulas)
;;     (query->goals query)


;; ============================================================
;; Program: A program is represented as a list representation of a set of procedures

;;  -------------------------------------------------------------
;; % Signature: append(List1, List2, List3)/3
;; % Purpose: List3 is the concatenation of List1 and List2.
;; append([], Xs, Xs).
;; append([X|Xs], Y, [X|Zs] ) :- append(Xs, Y, Zs).
;;
;; member(X, Ys) :- append(Zs, [X|Xs], Ys).
;;  -------------------------------------------------------------
;;
;;  Is encoded as:
;;
;;  -------------------------------------------------------------
;; ( ((append 3) (0 ((append empty (var Xs) (var Xs))
;;                    true))
;;               (1 ((append (cons (var X) (var Xs)) (var Y) (cons (var X) (var Zs)))
;;                   (append (var Xs) (var Y) (var Zs)))) )
;;   ((member 2) (0 ((member (var X) (var Ys))
;;                   (append (var Zs) (cons (var X) (var Xs)) (var Ys)) )) )
;; )
;;  -------------------------------------------------------------
;;
;; Concrete representations:
;; - constant --> symbol
;; - variable --> (var symbol)
;; - atomic-formula pred(terms) --> (pred . terms)
;; - rule Head :- Body --> ( head . body )
;; - procedure:   rule1 ... rulen --> ( (predicate arity) (1 rule1) (2 rule2) ... (n rulen) )
;; - program: predicate1 ... predicaten --> ( <predicate1> ... <predicaten> )


;;  Signature: make-program(procedures)
;;  Type: [List(Procedure) -> Program]
;;  Purpose: Value constructor for Program
;;  Pre-conditions: -
;;  Tests: -
(define make-program
  (lambda (procedures) procedures))

;;  Signature: program->procedures(program)
;;  Type: [Program -> List(Procedure)]
;;  Purpose: Accessor
;;  Pre-conditions: -
;;  Tests: -
(define program->procedures
  (lambda (program) program))

;;  Signature: program->predicates(program)
;;  Type: [Program -> List(Pair(Symbol, Number))]
;;  Purpose: Value constructor for Program
;;  Pre-conditions: -
;;  Tests:
(define program->predicates
  (lambda (program)
    (map first program)))

;;  Signature: program->procedure(exp, predicate)
;;  Type: [Program * Predicate -> Procedure]
;;  Purpose: Accessor for Procedure in Program
;;  Pre-conditions: -
;;  Tests:
(define program->procedure
  (lambda (program predicate)
    (let ((procedure (assoc predicate program)))
      (if procedure
          procedure
          (error 'program->procedure
                 "Program does not include predicate ~s" predicate)))))

;;  Signature: program->procedure->numbered-rules(exp, predicate)
;;  Type: [Program * Pair(Symbol,Number) -> Numbered-Rules]
;;  Purpose: Accessor for Procedure in Program - procedure represented as list of numbered rules.
;;  Pre-conditions: -
;;  Tests:
(define program->procedure->numbered-rules
  (lambda (program predicate)
    (cdr (program->procedure program predicate))))

;;  Signature: program->procedure->rules(exp, predicate)
;;  Type: [Program * Pair(Symbol,Number) -> Rules]
;;  Purpose: Accessor for Rules in Program
;;  Pre-conditions: -
;;  Tests:
(define program->procedure->rules
  (lambda (program predicate)
    (map cdr
         (program->procedure->numbered-rules program predicate))))


;;  Signature: program->procedure->rules(exp, predicate, number)
;;  Type: [Program * Symbol * Number -> Rule]
;;  Purpose: Accessor for Rule in Program
;;  Pre-conditions: number is within [0...number of rules defined for predicate - 1]
;;  Tests:
(define program->procedure->rule
  (lambda (program predicate number)
    (let ((nrules (program->procedure->numbered-rules program predicate)))
      (if (and (> number -1) (< number (length nrules)))
          (cdr (list-ref nrules number))
          (error 'program->procedure->rule
                 "Program does not include rule ~s for predicate ~s" number predicate)))))


;; ============================================================
;; Procedure: A list of rules that have as head the same pred/n

;;  Signature: make-procedure(rules)
;;  Type: [List(Rule) -> Procedure]
;;  Purpose: Value constructor for Procedure (computes pred/n and numbers the rules)
;;  Pre-conditions: rules is non empty.
;;  Tests:
(define make-procedure
  (lambda (rules)
    (let ((nrules (enumerate rules))
          (heads (map rule->head rules)))
      (let ((pred/n (predication->predicate (first heads))))
        (cons pred/n nrules)))))

;;  Signature: procedure->predicate(procedure)
;;  Type: [Procedure -> List(Symbol, Number)]
;;  Purpose: Accessor for procedures
;;  Pre-conditions: -
;;  Tests:
(define procedure->predicate
  (lambda (procedure)
    (first procedure)))

;;  Signature: procedure->numbered-rules(procedure)
;;  Type: [Procedure -> List(List(Number, Rule))]
;;  Purpose: Accessor for procedures
;;  Pre-conditions: -
;;  Tests:
(define procedure->numbered-rules
  (lambda (procedure)
    (cdr procedure)))

;;  Signature: procedure->rules(procedure)
;;  Type: [Procedure -> List(Rule)]
;;  Purpose: Accessor for procedures
;;  Pre-conditions: -
;;  Tests:
(define procedure->rules
  (lambda (procedure)
    (map cdr (cdr procedure))))


;; ============================================================
;; Rule: head / body

;;  Signature: make-rule(head, body)
;;  Type: [AtomicFormula * List(AtomicFormula) -> Rule]
;;  Purpose: Value constructor for Rule.
;;  Pre-conditions: -
;;  Tests:
(define make-rule
  (lambda (head body)
    (cons head body)))

;;  Signature: make-fact(head)
;;  Type: [AtomicFormula -> Rule]
;;  Purpose: Value constructor for Fact.
;;  Pre-conditions: -
;;  Tests: -
(define make-fact
  (lambda (head)
    (cons head (list 'true))))

;;  Signature: rule->head(rule)
;;  Type: [Rule -> AtomicFormula]
;;  Purpose: Accessor for Rule.
;;  Pre-conditions: -
;;  Tests:
(define rule->head
  (lambda (rule)
    (first rule)))

;;  Signature: rule->body(rule)
;;  Type: [Rule -> List(AtomicFormula)]
;;  Purpose: Accessor for Rule.
;;  Pre-conditions: -
;;  Tests:
(define rule->body
  (lambda (body)
    (cdr body)))

;;  Signature: rule->vars(rule)
;;  Type: [Rule -> Set(Variable)]
;;  Purpose: Accessor for Rule.
;;  Pre-conditions: -
;;  Tests:
(define rule->vars
  (lambda (rule)
    (let ((head (rule->head rule))
          (body (rule->body rule)))
      (remove-duplicates
       (append (atomic-formula->vars head)
               (flatmap atomic-formula->vars body))))))

;; ============================================================
;; Predication: (pred terms)

;;  Signature: make-predication(pred, terms)
;;  Type: [Symbol * List(Term) -> Predication]
;;  Purpose: Value constructor for Predication.
;;  Pre-conditions: -
;;  Tests:
(define make-predication
  (lambda (pred terms)
    (cons pred terms)))

;;  Signature: predication->pred(predication)
;;  Type: [Predication -> Symbol]
;;  Purpose: Accessor for Predication.
;;  Pre-conditions: -
;;  Tests:
(define predication->pred
  (lambda (predication)
    (car predication)))

;;  Signature: predication->args(predication)
;;  Type: [Predication -> Symbol]
;;  Purpose: Accessor for Predication.
;;  Pre-conditions: -
;;  Tests: -
(define predication->args
  (lambda (predication)
    (cdr predication)))

;;  Signature: predication->arity(predication)
;;  Type: [Predication -> Number]
;;  Purpose: Number of args in Predication.
;;  Pre-conditions: -
;;  Tests: -
(define predication->arity
  (lambda (predication)
    (length (predication->args predication))))

;;  Signature: predication->predicate(predication)
;;  Type: [Predication -> List(Symbol,Number)]
;;  Purpose: Predication -> (pred n) which serves as key in programs to retrieve procedures.
;;  Pre-conditions: -
;;  Tests: -
(define predication->predicate
  (lambda (predication)
    (list (predication->pred predication)
          (predication->arity predication))))

;;  Signature: predication?(predication)
;;  Type: [T -> Boolean]
;;  Purpose: Membership predicate
;;  Pre-conditions: -
;;  Tests:
(define predication?
  (lambda (x)
    (and (list? x)
         (not (empty? x))
         (symbol? (car x))
         (andmap term? (cdr x)))))

;;  Signature: predication->vars(predication)
;;  Type: [Predication -> List(Variable)]
;;  Purpose: List of variables that occur in Predication
;;  Pre-conditions: -
;;  Tests:
(define predication->vars
  (lambda (predication)
    (flatmap term->vars (predication->args predication))))


;; ============================================================
;; Atomic-formula: <predication> | true | false

;;  Signature: atomic-formula?(x)
;;  Type: [T -> Boolean]
;;  Purpose: Membership predicate
;;  Pre-conditions: -
;;  Tests:
(define atomic-formula?
  (lambda (x)
    (or (eq? x 'true)
        (eq? x 'false)
        (predication? x))))

;;  Signature: atomic-formula->vars(atomic-formula)
;;  Type: [Atomic-formula -> List(Variable)]
;;  Purpose: Membership predicate
;;  Pre-conditions: -
;;  Tests:
(define atomic-formula->vars
  (lambda (atomic-formula)
    (cond ((or (eq? atomic-formula 'true)
               (eq? atomic-formula 'false)) empty)
          (else (predication->vars atomic-formula)))))

;; ============================================================
;; Term: Symbol | Number | Variable | Compound-term


;;  Signature: term?(x)
;;  Type: [T -> Boolean]
;;  Purpose: Type predicate for terms
;;  Pre-conditions: -
;;  Tests:
(define term?
  (lambda (x)
    (or (symbol? x)
        (number? x)
        (variable? x)
        (compound-term? x))))

;; ============================================================
;; Variable: (var name) or (var name number) for renamed variables.

;;  Signature: make-variable(name)
;;  Type: [Symbol -> Var]
;;  Purpose: Value constructor for Variables.
;;  Pre-conditions: -
;;  Tests:
(define make-variable
  (lambda (name)
    (list 'var name)))

;;  Signature: rename-variable(name, number)
;;  Type: [Var * Number -> Var]
;;  Purpose: (var X) -> (var X N)
;;  Pre-conditions: -
;;  Tests:
(define rename-variable
  (lambda (var number)
    (list 'var (variable->name var) number)))

;;  Signature: var->name(var)
;;  Type: [Var -> Symbol]
;;  Purpose: Accessor for Variable.
;;  Pre-conditions: -
;;  Tests:
(define variable->name
  (lambda (var)
    (second var)))

;;  Signature: variable?(x)
;;  Type: [T -> Boolean]
;;  Purpose: Type predicate for terms
;;  Pre-conditions: -
;;  Tests:
(define variable?
  (lambda (x)
    (and (list? x)
         (eq? (car x) 'var)
         (symbol? (cadr x))
         (or (empty? (cddr x))
             (and (number? (third x))
                  (empty? (cdddr x)))))))

(define renamed-variable?
  (lambda (x)
    (and (list? x)
         (= (length x) 3)
         (eq? (car x) 'var)
         (symbol? (second x))
         (number? (third x)))))

;; ============================================================
;; Compound Terms: (functor . terms)

;;  Signature: make-compound-term(functor terms)
;;  Type: [Symbol * List(Term) -> Var]
;;  Purpose: Value constructor for Variables.
;;  Pre-conditions: -
;;  Tests:
(define make-compound-term
  (lambda (functor terms)
    (cons functor terms)))

;;  Signature: compound-term->functor(term)
;;  Type: [Compound-term -> Symbol]
;;  Purpose: Accessor for Compound-term
;;  Pre-conditions: -
;;  Tests:
(define compound-term->functor
  (lambda (term)
    (car term)))

;;  Signature: compound-term->args(term)
;;  Type: [Compound-term -> List(Term)]
;;  Purpose: Accessor for Compound-term
;;  Pre-conditions: -
;;  Tests:
(define compound-term->args
  (lambda (term)
    (cdr term)))

;;  Signature: compound-term?(x)
;;  Type: [T -> Boolean]
;;  Purpose: Type predicate for compound-term
;;  Pre-conditions: -
;;  Tests:
(define compound-term?
  (lambda (x)
    (and (list? x)
         (symbol? (car x))
         (andmap term? (cdr x)))))

;;  Signature: atomic-term?(x)
;;  Type: [T -> Boolean]
;;  Purpose: Type predicate for atomic-term
;;  Pre-conditions: -
;;  Tests:
(define atomic-term?
  (lambda (x)
    (or (symbol? x)
        (number? x)
        (variable? x))))

;;  Signature: equal-atomic-terms?(term1, term2)
;;  Type: [Term * Term -> Boolean]
;;  Purpose: Equality predicate for atomic-terms
;;  Pre-conditions: -
;;  Tests:
(define equal-atomic-terms?
  (lambda (term1 term2)
    (equal? term1 term2)))

;;  Type: [Term -> List(Variable)]
(define term->vars
  (lambda (term)
    (cond ((variable? term) (list term))
          ((atomic-term? term) empty)
          ((compound-term? term) (flatmap term->vars (compound-term->args term))))))


;; ============================================================
;; Query

;;  Signature: make-query(goals)
;;  Type: [List(Atomic-formula) -> Query]
;;  Purpose: Value constructor for queries.
;;  Pre-conditions: -
;;  Tests:
(define make-query
  (lambda (goals)
    (list 'query goals)))

;;  Signature: query->goals(x)
;;  Type: [Query -> List(Atomic-formula)]
;;  Purpose: Accessor for Query
;;  Pre-conditions: -
;;  Tests:
(define query->goals
  (lambda (query)
    (second query)))

(define success-query?
  (lambda (query)
    (or (empty? (query->goals query))
        (andmap (lambda (g) (eq? g 'true)) (query->goals query)))))

(define query->vars
  (lambda (query)
    (remove-duplicates
     (flatmap atomic-formula->vars (query->goals query)))))


