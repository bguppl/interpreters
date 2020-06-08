#lang racket

(require "lazy-tree-ADT.rkt"
         "LP-ast.rkt"
         "LP-rename.rkt"
         "substitution-ADT.rkt"
         "unify.rkt"
         "auxiliary.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A proof-tree is implemented as LZT(PT-node)
; where PT-node contains a query and a sub.
; For any node in the tree, the sub is the composition of all subs from root to the current node.
; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PT-node: tuple Query / Substitution
(define make-PT-node
  (lambda (query sub)
    (list query sub)))

(define PT-node->query
  (lambda (PT-node) (first PT-node)))

(define PT-node->sub
  (lambda (PT-node) (second PT-node)))

; Rule-sub: 
(define make-rule-sub
  (lambda (rule sub)
    (list rule sub)))

(define rule-sub->rule first)
(define rule-sub->sub second)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Signature: answer-query(query, program)
; Type: [Query * Program -> List(Sub)]
; Purpose: collect all answers from a finite proof tree
; Pre-conditions: -
; Tests: -
(define answer-query
  (lambda (query program)
    (letrec ((node-expander (lambda (node)
                              (LP-node-expander node program))))
      (let* ((pt (expand-lzt (make-PT-node query empty-sub) node-expander))  ;build proof-tree (lazy)
             (answers (lzt-filter pt LP-success-leaf?))) ;this expands the tree exhaustively
        ;the collected answers is a list of success-leaves nodes: '((true true..) substitution)
        ;we need to select the substitutions:
        (restrict-to-query-vars query (map PT-node->sub answers)))
    )))

; Signature: answer-query-first(query, program)
; Type: [Query * Program -> Sub | #f]
; Purpose: find the first answer in a proof tree in depth-first order of #f if not found.
; Pre-conditions: -
; Tests: -
(define answer-query-first
  (lambda (query program)
    (letrec ((node-expander (lambda (node)
                              (LP-node-expander node program))))
      (let* ((pt (expand-lzt (make-PT-node query empty-sub) node-expander))  ;build proof-tree (lazy)
             (answer (lzt-find-first pt LP-success-leaf?)))
        (if answer
            (sub-restrict (PT-node->sub answer) (query->vars query))
            #f)))))

; Signature: answer-query-lzl(query, program)
; Type: [Query * Program -> LZL(Sub)]
; Purpose: Return a LZL of answers
; Pre-conditions: -
; Tests: -
(define answer-query-lzl
  (lambda (query program)
    (letrec ((node-expander (lambda (node)
                              (LP-node-expander node program))))
      (let* ((pt (expand-lzt (make-PT-node query empty-sub) node-expander))
             (answer-PT-nodes-lzl (lzt-filter->lzl pt LP-success-leaf?)))
        (lzl-map (lambda (PT-node)
                   (sub-restrict (PT-node->sub PT-node)
                                 (query->vars query)))
                 answer-PT-nodes-lzl)))))

; Signature: restrict-to-query-vars(query, subs)
; Type: [Query * List(Sub) -> List(Sub)]
; Purpose: filter each sub in subs to keep only free vars in query
; Pre-conditions: -
; Tests: -
(define restrict-to-query-vars 
  (lambda (query subs)
    (let ((vars (query->vars query)))
      (map (lambda (sub) (sub-restrict sub vars))
           subs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Signature: LP-node-expander(PT-node, program)
; Type: [PT-Node * Program -> List(PT-Node)]
; Purpose: For a given query, produce all possible query expansions in the input program, and their associated
;         substitutions. The substitutions are already combined with the substitution of the given query-sub pair.
; Pre-conditions: -
; Tests: -
(define LP-node-expander
  (lambda (PT-node program)
    (let ((query (PT-node->query PT-node))
          (sub (PT-node->sub PT-node)))
      (if (success-query? query)
          empty
          (let* ((selected-goal (Gsel query))
                 (rule-subs (Rsel selected-goal program))
                 (new-queries (map (lambda (rule-sub) (expand-query query selected-goal rule-sub))
                                   rule-subs))
                 (new-subs (map (lambda (rule-sub) (sub-combine sub (rule-sub->sub rule-sub)))
                                rule-subs)))
            (map make-PT-node new-queries new-subs))
          ))
    ))

; Signature: expand-query(query, goal, rule-sub)
; Type: [Query * AtomicFormula * RuleSub -> Query]
; Purpose: Given a rule-sub (rule sub) and a query (G1 ... Gi-1 Goal Gi+1 ... Gn)
;          where rule is ( Head -> Body )
;          and Unify(Goal, Head) = sub
;          compute [G1 ... Gi-1 Body Gi+1 ... Gn) o sub
(define expand-query 
  (lambda (query goal rule-sub)
    (let ((prefix-suffix (split-list (query->goals query) goal))) 
       (sub-apply-query (rule-sub->sub rule-sub)
                        (make-query (append (car prefix-suffix)
                                            (rule->body (rule-sub->rule rule-sub))
                                            (cdr prefix-suffix)))))
    ))


(define LP-success-leaf?
  (lambda (PT-node)
    (success-query? (PT-node->query PT-node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Signature: Gsel(query)
; Type: [Query -> Predication]
; Purpose: select one goal in the query that is not true.
; Pre-conditions: (and (not (success-query? query)) (not (empty? (query->goals query))))
; Tests: -
(define Gsel
  (lambda (query)
    (first (filter (lambda (goal) (not (eq? goal 'true))) 
                   (query->goals query)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Signature: Rsel(goal, program)
; Type: [AtomicFormula * Program -> List(Rule-Sub)]
; Purpose: Compute the list of pairs (rule, sub) that match goal in the program.
; Pre-conditions: -
; Tests: -
(define Rsel 
  (lambda (goal program)
    (let* ((predicate (predication->predicate goal))   ;; Predicate is a list (pred arity)
           (rules (map rename-rule (program->procedure->rules program predicate)))
           (rule-subs (map (lambda (rule) (make-rule-sub 
                                           rule
                                           (unify-formulas goal (rule->head rule))))
                           rules)))
      (filter (lambda (rule-sub) (not (eq? 'fail (rule-sub->sub rule-sub))))
              rule-subs))))


