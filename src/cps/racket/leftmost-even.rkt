#lang racket

;; Define Tree ADT
(define make-tree list)
(define add-subtree cons)
(define make-leaf (lambda (x) x))
(define empty-tree? empty?)
(define first-subtree car)
(define rest-subtrees cdr)
(define leaf-data (lambda (x) x))
(define composite-tree? list?)
(define leaf? (lambda (x) (not (composite-tree? x))))

;; Signature: leftmost-even(tree)
;; Purpose: Find the leftmost even leaf of an unlabeled tree whose leaves are labeled by numbers. 
;; If no leaf is even, return #f.
;; Type: [List -> Number | Boolean]
;; Examples: (leftmost-even '((1 2) (3 4 5))) ==> 2
;;           (leftmost-even '((1 1) (3 3) 5)) ==> #f
(define leftmost-even
  (lambda (tree)
    (leftmost-even$ tree 
                    (lambda (x) x) 
                    (lambda () #f))))

(define leftmost-even$
  (lambda (tree succ-cont fail-cont)
    (cond ((empty-tree? tree) (fail-cont))
          ((leaf? tree)
           (if (even? (leaf-data tree))
               (succ-cont tree)
               (fail-cont)))
          (else ; Composite tree
            (leftmost-even$ (first-subtree tree)
                            succ-cont
                            (lambda () (leftmost-even$ (rest-subtrees tree) ; (@)
                                                       succ-cont
                                                       fail-cont)))))))

(define leftmost-even-1
  (lambda (tree)
    (letrec ((iter (lambda (tree)
                     (cond ((empty-tree? tree) #f)
                           ((leaf? tree)
                            (if (even? (leaf-data tree)) 
                                (leaf-data tree) 
                                #f))
                           (else
                             (let ((res-first (iter (first-subtree tree))))
                               (if res-first
                                   res-first
                                   (iter (rest-subtrees tree)))))))))
       (iter tree))))
