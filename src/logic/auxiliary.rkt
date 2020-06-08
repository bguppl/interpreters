#lang racket

(provide (all-defined-out))
;; Auxiliaries

; Signature: try(f, ans)
; Type: [Closure * T -> T]
; Purpose: exception handling - execute the closure f, if any error raised, return ans.
; Pre-conditions: -
; Tests: (try (lambda () (error "bad")) 'fail) ==> 'fail
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

;; Flatten a heterogeneous list
(define flatten
  (lambda (tree)
    (if (not (list? tree))
        (list tree)
        (foldl append (list) (map flatten tree)))))

(define flatmap 
   (lambda (proc seq)
      (foldr append (list) (map proc seq))))

(define set-equal?
  (lambda (set1 set2)
    (and (list? set1)
         (list? set2)
         (eq? (length set1) (length set2))
         (empty? (filter (lambda (el) (not (member el set2)))
                         set1))
         (empty? (filter (lambda (el) (not (member el set1)))
                         set2)))))

(define remove-duplicates
  (lambda (l)
    (letrec ((iter (lambda (l acc)
                     (if (empty? l)
                         acc
                         (if (member (car l) acc)
                             (iter (cdr l) acc)
                             (iter (cdr l) (cons (car l) acc)))))))
      (iter l empty))))

; Signature: enumerate(list)
; Type: [List(T) -> List(Pair(Number,T))
; Purpose: return a list of the form ((0 . x0) (1 . x1) ...) from a list (x0 x1 ...)
; Pre-conditions: -
; Tests: (enumerate '(a b c)) ==> '((0 . a) (1 . b) (2 . c))
(define enumerate
  (lambda (list)
    (letrec ((iter (lambda (i l)
                     (if (empty? l) 
                         empty
                         (cons (cons i (car l)) 
                               (iter (+ i 1) (cdr l)))))))
      (iter 0 list))))

; Signature: range(n1,n2)
; Type: [Number * Number -> List(Number)]
; Purpose: return the range of numbers [n1, n1+1, ..., n2]
; Pre-conditions: -
; Tests: (range 1 3) => '(1 2 3)
(define range 
  (lambda (n1 n2) 
    (cond ((> n1 n2) empty) 
          (else (cons n1 (range (+ n1 1) n2))))))

; Signature: split-list(l, elt)
; Type: [List(T) * T -> Pair(List(T), List(T))
; Purpose: split a list of items into a pair (prefix . suffix) s.t. (prefix elt suffix) = l
; Pre-conditions: (member elt l)
; Tests: (split '(1 2 3 4 5) 3) ==> ((1 2) 4 5)
(define split-list
  (lambda (l elt)
    (letrec ((iter (lambda (prefix suffix)
                     (if (equal? (car suffix) elt)
                         (cons prefix (cdr suffix))
                         (iter (append prefix (list (car suffix))) (cdr suffix))))))
      (iter empty l))))


