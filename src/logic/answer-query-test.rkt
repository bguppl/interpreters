#lang racket

(require "utils.rkt" "Substitution-adt.rkt" "LP-ast.rkt" "answer-query.rkt" "lazy-tree-ADT.rkt")

;Example programs:
;
;% Signature: part(Name).
;part(a).
;part(b).
;part(c).
(define parta (make-fact '(part a)))
(define partb (make-fact '(part b)))
(define partc (make-fact '(part c)))
(define part-proc (make-procedure (list parta partb partc)))

;red(a).
;green(b).
;yellow(c).
(define reda (make-fact '(red a)))
(define red-proc (make-procedure (list reda)))
(define greenb (make-fact '(green b)))
(define green-proc (make-procedure (list greenb)))
(define yellowc (make-fact '(yellow c)))
(define yellow-proc (make-procedure (list yellowc)))

(define part-prog (make-program (list part-proc red-proc green-proc yellow-proc)))

; ?- part(X)
(define query-partX (make-query (list '(part (var X)))))

; ?- red(X)
(define query-redX (make-query (list '(red (var X)))))

; ?- part(X), red(X)
(define query-part-redX (make-query (list '(part (var X)) '(red (var X)))))

; ?- part(X), yellow(X)
(define query-part-yellowX (make-query (list '(part (var X)) '(yellow (var X)))))


(run-tests
 (test (Rsel (Gsel query-partX) part-prog)
       =>
       '((((part a) true) (sub ((var X)) (a))) (((part b) true) (sub ((var X)) (b))) (((part c) true) (sub ((var X)) (c)))))

 (test (Rsel (Gsel query-redX) part-prog)
       =>
       '((((red a) true) (sub ((var X)) (a)))))
 
 (test (answer-query query-partX part-prog)
       =>
       '((sub ((var X)) (a)) (sub ((var X)) (b)) (sub ((var X)) (c))))
 
 (test (answer-query query-redX part-prog)
       =>
       '((sub ((var X)) (a))))
 
 (test (answer-query query-part-redX part-prog)
       =>
       '((sub ((var X)) (a))) )

 (test (answer-query query-part-yellowX part-prog)
       =>
       '((sub ((var X)) (c))) )

 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;% Signature: append(List1, List2, List3)/3
;% Purpose: List3 is the concatenation of List1 and List2.
;append([], Xs, Xs).
;append([X|Xs], Y, [X|Zs] ) :- append(Xs, Y, Zs).
;
;member(X, Ys) :- append(Zs, [X|Xs], Ys).

(define app1 (make-rule '(append empty (var Xs) (var Xs)) (list 'true)))
(define app2 (make-rule 
              '(append (cons (var X) (var Xs)) (var Y) (cons (var X) (var Zs)))
              '((append (var Xs) (var Y) (var Zs))) ))
(define app-proc (make-procedure (list app1 app2)))

(define mem1 (make-rule '(member (var X) (var Ys)) 
                        '((append (var Zs) (cons (var X) (var Xs)) (var Ys))) ))
(define mem-proc (make-procedure (list mem1)))

(define prog-app-mem
  (make-program (list app-proc mem-proc)))

; ?- member(X, [1])
(define query-member1 (make-query (list '(member (var X) (cons 1 empty)))))

; ?- member(1, [X])
(define query-memberX (make-query (list '(member 1 (cons (var X) empty)))))

; ?- member(1, [2])
(define query-member12 (make-query (list '(member 1 (cons 2 empty)))))

; ?- member(X, [1,2,3])
(define query-member123 (make-query (list '(member (var X) (cons 1 (cons 2 (cons 3 empty)))))))

(run-tests
 (test (answer-query query-member1 prog-app-mem)
       =>
       '((sub ((var X)) (1))))

 (test (answer-query query-memberX prog-app-mem)
       =>
       '((sub ((var X)) (1))))

  (test (answer-query query-member12 prog-app-mem)
       =>
       '())
  
  (test (answer-query query-member123 prog-app-mem)
        =>
        '((sub ((var X)) (1)) (sub ((var X)) (2)) (sub ((var X)) (3))))

  (test (answer-query-first query-member123 prog-app-mem)
        =>
        '(sub ((var X)) (1)))

  )


; ?- append([1],[2],[1,2])
(define query-append12 (make-query (list '(append (cons 1 empty) (cons 2 empty) (cons 1 (cons 2 empty))))))

; ?- append([1],[2],X)
(define query-append12X (make-query (list '(append (cons 1 empty) (cons 2 empty) (var X)))))

; ?- append([1],X,[1,2])
(define query-append1X12 (make-query (list '(append (cons 1 empty) (var X) (cons 1 (cons 2 empty))))))

; ?- append(X,[2],[1,2])
(define query-appendX212 (make-query (list '(append (var X) (cons 2 empty) (cons 1 (cons 2 empty))))))

; ?- append(X,Y,[1,2,3,4])
(define query-appendXY1234 (make-query (list '(append (var X) (var Y) (cons 1 (cons 2 (cons 3 (cons 4 empty))))))))

; An infinite proof-tree
; ?- append(X,X,Z)
(define query-appendXXZ (make-query (list '(append (var X) (var X) (var Z)))))

(run-tests
  (test (answer-query query-append12 prog-app-mem)
        =>
        '((sub () ())))

  (test (answer-query query-append12X prog-app-mem)
        =>
        '((sub ((var X)) ((cons 1 (cons 2 empty))))))
 
  (test (answer-query query-append1X12 prog-app-mem)
        =>
        '((sub ((var X)) ((cons 2 empty)))))
        
  (test (answer-query query-appendX212 prog-app-mem)
        =>
        '((sub ((var X)) ((cons 1 empty)))))

  (test (answer-query query-appendXY1234 prog-app-mem)
        =>
        '((sub ((var Y) (var X)) ((cons 1 (cons 2 (cons 3 (cons 4 empty)))) empty))
          (sub ((var Y) (var X)) ((cons 2 (cons 3 (cons 4 empty))) (cons 1 empty)))
          (sub ((var Y) (var X)) ((cons 3 (cons 4 empty)) (cons 1 (cons 2 empty))))
          (sub ((var Y) (var X)) ((cons 4 empty) (cons 1 (cons 2 (cons 3 empty)))))
          (sub ((var Y) (var X)) (empty (cons 1 (cons 2 (cons 3 (cons 4 empty)))))))  )

  (test (answer-query-first query-appendXY1234 prog-app-mem)
        =>
        '(sub ((var Y) (var X)) ((cons 1 (cons 2 (cons 3 (cons 4 empty)))) empty)))

  (test (lzl-take (answer-query-lzl query-appendXY1234 prog-app-mem) 2)
        =>
        '((sub ((var Y) (var X)) ((cons 1 (cons 2 (cons 3 (cons 4 empty)))) empty))
          (sub ((var Y) (var X)) ((cons 2 (cons 3 (cons 4 empty))) (cons 1 empty)))))
  
  ;; An infinite proof-tree
  ;; ?- append(X,X,Z)
  (test (map normalize-vars (lzl-take (answer-query-lzl query-appendXXZ prog-app-mem) 2))
        =>
        '((sub ((var Z) (var X)) (empty empty)) 
          (sub ((var Z) (var X)) ((cons (var X 1) (cons (var X 1) empty)) (cons (var X 1) empty)))))

  (test (map normalize-vars (lzl-take (answer-query-lzl query-appendXXZ prog-app-mem) 3))
        =>
        '((sub ((var Z) (var X)) (empty empty))
          (sub ((var Z) (var X)) ((cons (var X 1) (cons (var X 1) empty)) (cons (var X 1) empty)))
          (sub ((var Z) (var X)) ((cons (var X 1) (cons (var X 2) (cons (var X 1) (cons (var X 2) empty)))) (cons (var X 1) (cons (var X 2) empty))))))

)

