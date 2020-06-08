#lang racket

;; Load tests runner file.
(require "utils.rkt"
         "auxiliary.rkt"
         "lazy-tree-ADT.rkt")

;;;;;;;;;;
;; Finite trees
;;;;;;;;;;

(define lzt1
  (let ((kb '(10 20)))
    (expand-lzt 1 
                (lambda (label) 
                  (if (> label 12)
                      empty
                      (map (lambda (kbi)(+ label kbi))
                           kb))))))

(define number-lazy-tree-tests
  (lambda ()
    (display "number-lazy-tree-tests:\t")
    (run-tests
     (test (lzt->root lzt1) =>  1)
     (test (map lzt->root (lzt->branches lzt1)) =>  '(11 21))
     (test (lzt->root (lzt->first-branch lzt1)) =>  11)
     (test (map lzt->root (lzt->rest-branches lzt1)) => '(21))
     (test (lzt? lzt1) =>  #t)
     (test (empty-lzt? lzt1) =>  #f)
     (test (composite-lzt? lzt1) =>  #t)
     (test (leaf-lzt? lzt1) =>  #f)
     (test (lzt->take-branches lzt1 1) =>  '(1 (11) (21)))
     (test (lzt->take-branches lzt1 2) =>  '(1 (11 (21) (31)) (21)))
     (test (lzt->nth-level lzt1 1) => '(11 21))
     (test (lzt->nth-level lzt1 2) => '(21 31))
     )))

(define lzt2 
  (let ((kb '(10 21)))
    (expand-lzt 1 
                (lambda (label) 
                  (if (> label 12)
                      empty
                      (map (lambda (kbi)(+ label kbi))
                           kb))))))

(define collect-labels-lazy-tree-tests
  (lambda ()
    (display "collect-labels-lazy-tree-tests:\t")
    (run-tests
     (test (lzt-filter lzt2 odd?) =>  '(1 11 21))
     (test (lzt-filter lzt2 even?) =>  '(32 22))
     (test (lzt-filter lzt2 (lambda (x) #t)) =>  '(1 11 21 32 22))
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infinite trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A linear tree of all the integers (equivalent to a lzl)
(define tints (expand-lzt '0 (lambda (node) (list (+ node 1)))))

(define tints-tests
  (lambda ()
    (display "tints-tests:\t")
    (run-tests
     (test (lzt->root tints) => 0)
     (test (length (lzt->branches tints)) => 1)
     (test (lzt->nth-level tints 10) => '(10))
     (test (lzt->take-branches tints 3) => '(0 (1 (2 (3)))))
     (test (lzt-find-first tints (lambda (n) (> n 10))) => 11)
     (test (lzl-take (lzt-filter->lzl tints odd?) 10) => '(1 3 5 7 9 11 13 15 17 19)))))
     

; A tree whose nodes are lists of 1s
; At level 1 - 1 node of length 1
; At level n - n nodes of length n each with n+1 nodes...
; Level n has n! nodes of length n (root is level 0).
; ()
; (1)
; ((1 1) (1 1))
; ((1 1 1) (1 1 1) (1 1 1)) ((1 1 1) (1 1 1) (1 1 1))
; ...
(define make-ones
  (expand-lzt '()
              (lambda (node)
                (let ((n (length node)))
                  (cons (cons 1 node)
                        (map (lambda (ln) (cons 1 node)) node))))))

(define ones-tests
  (lambda ()
    (display "ones-tests:\t")
    (run-tests
     (test (lzt->root make-ones) => '())
     (test (length (lzt->branches make-ones)) => 1)
     (test (length (lzt->nth-level make-ones 4)) => 24)
     (test (andmap (lambda (node) (= (length node) 4))
                   (lzt->nth-level make-ones 4)) => #t)
     (test (lzt->take-branches make-ones 3) =>
           '(() ((1) ((1 1) ((1 1 1)) ((1 1 1)) ((1 1 1))) ((1 1) ((1 1 1)) ((1 1 1)) ((1 1 1))))))
     (test (lzt-find-first make-ones (lambda (node) (> (length node) 4))) => '(1 1 1 1 1))
     (test (lzl-take (lzt-filter->lzl make-ones
                                      (lambda (node) (> (length node) 4))) 2) => '((1 1 1 1 1) (1 1 1 1 1 1))) ;; DF traversal
     )))



     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tints-tests)
(ones-tests)
(number-lazy-tree-tests)
(collect-labels-lazy-tree-tests)

