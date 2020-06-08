#lang racket
(require "utils.rkt" "auxiliary.rkt" "LP-ast.rkt" "LP-rename.rkt")

;Example program:
;
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


; Program tests
(run-tests
 (test (program->predicates prog-app-mem)  => '((append 3) (member 2))
       )
 
 (test (program->procedure prog-app-mem '(append 3))
       =>
       '((append 3)
         (0 (append empty (var Xs) (var Xs)) true)
         (1 (append (cons (var X) (var Xs)) (var Y) (cons (var X) (var Zs)))
            (append (var Xs) (var Y) (var Zs))))
       )
       
 (test (program->procedure->rule prog-app-mem '(append 3) 1) 
       =>
       '((append (cons (var X) (var Xs)) (var Y) (cons (var X) (var Zs))) 
         (append (var Xs) (var Y) (var Zs)))
       )
 )


(rename-program prog-app-mem)
