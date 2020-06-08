#lang racket

(require "auxiliary.rkt")

(provide (all-defined-out))

;Lazy tree (LZT): 
;  A lazy tree is: empty-lzt is a lazy tree.
;                  If n is a node, and lzt1...lztn are the lazy trees whose roots expand n for some node-expander, 
;                      then (n lzt1 ... lztn) is a composite lazy tree
;                  If n cannot be expanded, then (n) is a leaf lazy tree.
;
;  A lazy tree is represented as a "lazy tree-list" whose head is the root-node and whose tail is a 
;  REGULAR list of lazy-trees:
;   (root (lambda () (list lzt1 lzt2 ... lztn)))
;  empty-lzt represents the empty lazy-tree
;  A leaf is represented by: (root (lambda () empty-lzt))

; Lazy-trees can be built by any computation in general in the closure that computes the children of the tree.
; (root (lambda () <any computation that returns a list of lzts>))
; The most regular way to construct a lzt is to use a uniform node-expander function:
; (root (lambda () <a function which computes a list of children given a node>))
; A node expander has type: [Node -> List(Lzt(node))]
; On a leaf-node, the node expander returns empty.
; On non-leaf nodes, the node expander returns a flat list of nodes. 
;
; That is - the node expander constructs a lazy tree given a function which computes the direct children of a node.
; The lzt constructor expand-lzt(node, node-expander) handles this uniform tree expansion method
; It is the key function of this ADT.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LZT(Node) value constructors

; Signature: make-lzt(node, closure)
; Type: [Node * [() -> List(LZT(Node))] -> LZT(Node)]
; Purpose: LZT value constructor
; Pre-conditions: -
; Tests: (make-lzt h (lambda () (list lzt1 ... lztn))) => '(h . #<Closure>)
(define make-lzt cons)

; Signature: make-lzt-leaf(node, closure)
; Type: [Node * [() -> List(LZT(Node))] -> LZT(Node)]
; Purpose: LZT value constructor
; Pre-conditions: -
; Tests: (make-lzt-leaf h (lambda () empty-lzt)) => '(h . #<Closure>)
(define make-lzt-leaf cons)

(define empty-lzt empty)

; Signature: expand-lzt(node, node-expander)
; Type: [Node * [Node -> List(Node)] -> LZT(Node)]
; Purpose: Main lzt constructor, using a node-expander procedure
; Pre-conditions: -
; Tests: (expand-lzt node (lambda (node) ...)) => '(node . #<Closure>)
; Note: expand-lzt explicitly expands root one level down - and prepares the recursive
; expansion of the children node.
(define expand-lzt
  (lambda (root node-expander)
    (let ((children-nodes (node-expander root)))
      (make-lzt root
                (lambda () (map (lambda (node) (expand-lzt node node-expander))
                                children-nodes))))
    )) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Selectors


; Signature: lzt->root(lzt)
; Type: [LZT(Node) -> Node]
; Purpose: Selector of LZT(Node)
; Pre-conditions: -
; Tests: (lzt->root lzt) => node
(define lzt->root car)

; Signature: leaf->data(lzt)
; Type: [LZT(Node) -> Node]
; Purpose: Selector of LZT(Node)
; Pre-conditions: -
; Tests: -
(define leaf-data lzt->root)

; Signature: lzt->branches(lzt)
; Type: [LZT(Node) -> List(LZT(Node))]
; Purpose: Selector of LZT(Node)
; Pre-conditions: -
; Tests: -
(define lzt->branches 
  (lambda (lzt) 
      ((cdr lzt))
    ))

; Signature: lzt->first-branch(lzt)
; Type: [LZT(Node) -> LZT(Node)]
; Purpose: Selector of LZT(Node)
; Pre-conditions: -
; Tests: -
(define lzt->first-branch
  (lambda (lzt)
    (car (lzt->branches lzt))))

; Signature: lzt->rest-branches(lzt)
; Type: [LZT(Node) -> List(LZT(Node))]
; Purpose: Selector of LZT(Node)
; Pre-conditions: -
; Tests: -
(define lzt->rest-branches
  (lambda (lzt)
    (cdr (lzt->branches lzt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;predicates

; Signature: empty-lzt?(x)
; Type: [T -> Boolean]
; Purpose: Membership predicate
; Pre-conditions: -
; Tests: -
(define empty-lzt? empty?)

; Signature: lzt?(lzt)
; Type: [T -> Boolean]
; Purpose: Membership predicate
; Pre-conditions: -
; Tests: -
(define lzt?
  (lambda (lzt) 
    (or (empty-lzt? lzt)
        (and (pair? lzt) (procedure? (cdr lzt))))
    ))

; Signature: leaf-lzt?(lzt)
; Type: [T -> Boolean]
; Purpose: Membership predicate
; Pre-conditions: -
; Tests: -
(define leaf-lzt?
  (lambda (lzt) 
    (and (lzt? lzt) (not (empty-lzt? lzt)) (empty? (lzt->branches lzt)))
    ))

; Signature: composite-lzt?(lzt)
; Type: [T -> Boolean]
; Purpose: Membership predicate
; Pre-conditions: -
; Tests: -
(define composite-lzt?
  (lambda (lzt)
    (and (lzt? lzt)
         (not (or (empty-lzt? lzt) (leaf-lzt? lzt))))
    ))


(define lzt->take-branches
  (lambda (lzt n)
    (cons (lzt->root lzt)
          (if (= n 0)
              (list)
              (map (lambda (b) 
                     (lzt->take-branches b (- n 1)))
                   (lzt->branches lzt))))
    ))
               

(define lzt->nth-level 
  (lambda (lzt n)
    (if (= n 0)
        (list (lzt->root lzt))
        (flatmap (lambda (b) (lzt->nth-level b (- n 1)))
                 (lzt->branches lzt)))
    ))



; Signature: lzt-filter(lzt, filterP)
; Type: [LZT(Node) * [Node -> Boolean] -> List(Node)]
; Purpose: Collect filtered nodes in a finite lazy tree; Depth-first order
; Pre-conditions: -
; Tests: -
(define lzt-filter
  (lambda (lzt filterP)
    (letrec ((collect (lambda (lzt)
                        (let ((children (flatmap collect (lzt->branches lzt))))
                          (if (filterP (lzt->root lzt))
                              (cons (lzt->root lzt) children)
                              children)))))
      (if (empty-lzt? lzt)
          empty
          (collect lzt)))))

; Signature: lzt-find-first(lzt, filterP)
; Type: [LZT(Node) * [Node -> Boolean] -> Node U #f]
; Purpose: Find first node in lazy tree Depth-first traversal that satisfies filterP - #f if not found.
; Pre-conditions: -
; Tests: -
(define lzt-find-first
  (lambda (lzt filterP)
    (letrec ((collect 
              (lambda (lzt)
                (if (filterP (lzt->root lzt))
                    (lzt->root lzt)
                    (find-first-in-trees (lzt->branches lzt)))))
             (find-first-in-trees 
              (lambda (lzts)
                (cond ((empty? lzts) #f)
                      ((collect (car lzts)))
                      (else (find-first-in-trees (cdr lzts)))))))
      (if (empty-lzt? lzt)
          #f
          (collect lzt)))))


; Signature: lzt-filter->lzl(lzt, filterP)
; Type: [LZT(Node) * [Node -> Boolean] -> Lzl(Node)]
; Purpose: Collect filtered nodes in depth-first-order and return the results as a lazy list.
; Pre-conditions: -
; Tests: -
(define lzt-filter->lzl
  (lambda (lzt filterP)
    (letrec (; [LZT(Node) -> LZL(Node)]
             (collect 
              (lambda (lzt)
                (if (filterP (lzt->root lzt))
                    (make-lzl (lzt->root lzt)
                              (lambda () (collect-in-trees (lzt->branches lzt))))
                    (collect-in-trees (lzt->branches lzt)))))
             ; [List(LZT(Node)) -> LZL(Node)]
             (collect-in-trees
              (lambda (lzts)
                (if (empty? lzts)
                    empty-lzl
                    (let ((first-lzl (collect (first lzts))))
                      (if (empty-lzl? first-lzl)
                          (collect-in-trees (cdr lzts))
                          (append-lzl first-lzl 
                                      (lambda () (collect-in-trees (cdr lzts))))))))))
      (if (empty-lzt? lzt)
          empty-lzl
          (collect lzt)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LZL-ADT
(define make-lzl cons)
(define empty-lzl empty)
(define empty-lzl? empty?)
(define lzl->first car)
(define lzl->rest
  (lambda (lzl)
    ((cdr lzl))))

; Signature: lzl-take(lzl,n)
; Type: [LZL(T) * Number -> List(T)]
; Purpose: Return up to N elements at the front of lzl
; Pre-conditions: -
; Tests: -
(define lzl-take
  (lambda (lzl n)
    (cond ((= n 0) empty-lzl)
          ((empty-lzl? lzl) empty-lzl)
          (else (cons (lzl->first lzl)
                      (lzl-take (lzl->rest lzl) (- n 1)))))))

; Signature: lzl-map(f, lzl)
; Type: [[T1->T2] * LZL(T1) -> LZL(T2)]
; Purpose: Map f over LZL
; Pre-conditions: -
; Tests: -
(define lzl-map
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        empty-lzl
        (make-lzl (f (lzl->first lzl))
                  (lambda () (lzl-map f (lzl->rest lzl)))))))
    
; Signature: lzl-append(lzl1, cont)
; Type: [LZL(T) * [()->LZL(T)] -> LZL(T)]
; Purpose: Append an LZL with a continuation returning an LZL.
; Pre-conditions: -
; Tests: -
(define append-lzl
    (lambda (lzl1 cont)
      (if (empty? lzl1)
          (cont)
          (make-lzl (lzl->first lzl1)
                    (lambda () (append-lzl (lzl->rest lzl1) cont))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
