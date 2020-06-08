#lang racket

(require "substitution-adt.rkt" "utils.rkt" "LP-ast.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: try(f, ans)
; Type: [Closure * T -> T]
; Purpose: exception handling - execute the closure f, if any error raised, return ans.
; Pre-conditions: -
; Tests: (try (lambda () (error "bad")) 'fail) ==> 'fail
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

;; The empty substitution
(define sub0 (make-empty-sub))

(define sub1
  (make-sub '((var T1) (var T2) (var T3))
            '(1 (f (var T4) 2) (var T9))))

(define sub2
  (make-sub '((var T4) (var T5) (var T6))
            '([f (var T1) 3]
              b
              (var T7))))

(define sub3
  (make-sub '((var T7) (var T8) (var T9))
            '(7 (g (var T5) n (var T3)) 9)))

(define sub4
  (make-sub '((var T1) (var T2) (var T3))
            '(1 (h (var T10)) (j (var T5) 3))))

(define sub5
  (make-sub '((var T1) (var T2) (var T3))
            '(1 (p (var T5) 2) 3)))

(define sub-to-vars-terms-list
  (lambda (sub)
    (let ((vars (sub->variables sub))
          (terms (sub->terms sub)))
      (map list vars terms))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sub-combination-tests
  (lambda ()
    (display "sub-combination-tests:\t")
    (run-tests

     (test
      (let ((vars-terms
             (sub-to-vars-terms-list
              (sub-combine
               (make-sub '((var T1) (var T2))
                         '((m Number (var S1)) (f Number (var S4))))
               (make-sub '((var T3))
                         '((f Number (var S2))))))))
        (set=? (list->set vars-terms)
               (list->set '(((var T3) (f Number (var S2)))
                            ((var T1) (m Number (var S1)))
                            ((var T2) (f Number (var S4)))))))
      => #t)

     (test
      (let
          ((vars-terms
            (sub-to-vars-terms-list
             (sub-combine
              (make-sub '((var T1) (var T2))
                        '([m Number (var S1)]
                          [f Number (var T5)]))
              (make-sub '((var T3) (var T4) (var T5))
                        '([f Number (var S2)]
                          [m Number (var S1)]
                          Boolean))))))
        (set=? (list->set vars-terms)
               (list->set '(((var T3) (f Number (var S2)))
                            ((var T1) (m Number (var S1)))
                            ((var T2) (f Number Boolean))
                            ((var T4) (m Number (var S1)))
                            ((var T5) Boolean)))))
      => #t)

     ;; {T1:(* Number S1), T2:(-> (* T5) T4)} O
     ;; {S1:Boolean, T3:(-> (* Number) S2), T5:(-> (* Number) S1), T4:Boolean}
     (test
      (let
          ((vars-terms
            (sub-to-vars-terms-list
             (sub-combine
              (make-sub '((var T1) (var T2))
                        '([m Number (var S1)] [f (var T5) (var T4)]))
              (make-sub '((var S1) (var T3) (var T5) (var T4))
                        '(Boolean
                          [f Number (var S2)]
                          [f Number (var S1)]
                          Boolean))))))
        (set=? (list->set vars-terms)
               (list->set '(((var T3) (f Number (var S2)))
                            ((var T1) (m Number Boolean))
                            ((var T2) (f (f Number (var S1)) Boolean))
                            ((var T5) (f Number (var S1)))
                            ((var T4) Boolean)
                            ((var S1) Boolean)))))
      => #t)

     (test
      (let ((vars-terms
             (sub-to-vars-terms-list
              (sub-combine
               (make-sub '((var T1) (var T2) (var T3))
                         '((var S1) [m (var S2) Number] Boolean))
               (make-sub '((var S1) (var S2))
                         '([m (var T5) (f (m Number (var T2)) (var T2))] (var T3)))))))
        (set=? (list->set vars-terms)
               (list->set '(((var T1) (m (var T5) (f (m Number (var T2)) (var T2))))
                            ((var T2) (m (var T3) Number))
                            ((var T3) Boolean)
                            ((var S1) (m (var T5) (f (m Number (var T2)) (var T2))))
                            ((var S2) (var T3))))))
      => #t)

     (test
      (let ((vars-terms
             (sub-to-vars-terms-list
              (sub-combine
               (make-sub '((var T1) (var T2) (var T3))
                         '(Number [m (var T4) Number] (var T9)))
               (make-sub '((var T4) (var T5) (var T6))
                         '([f (var T1) Number] Boolean (var T7)))))))
        (set=? (list->set vars-terms)
               (list->set '(((var T6) (var T7))
                            ((var T5) Boolean)
                            ((var T4) (f (var T1) Number))
                            ((var T1) Number)
                            ((var T2) (m (f (var T1) Number) Number))
                            ((var T3) (var T9))))))
      => #t)

     ;; Assert error on cicular substitution
     (test
      (try
       (lambda()
         (sub-combine
          (make-sub '((var T3) (var S1) (var T4) (var T5))
                    '(Boolean [f Number (var T2)] [m Number (var S1)] Boolean))
          (make-sub '((var T1) (var T2))
                    '([m Number (var S1)] [f (var T3) (var S1)])))))
      => 'error)

     ;; Assert T8 is removed after applying sub2 to sub1
     ;; {T7:Number, T8:(-> (* T5 Number) T3)} o {T5:T7, T8:Boolean}
     (test
      (let
          ((vars-terms
            (sub-to-vars-terms-list
             (sub-combine
              (make-sub '((var T7) (var T8))
                        '(Number [f (m (var T5) Number) (var T3)]))
              (make-sub '((var T5) (var T8))
                        '((var T7) Boolean))))))
        (set=? (list->set vars-terms)
               (list->set '(((var T5) (var T7))
                            ((var T7) Number)
                            ((var T8) (f (m (var T7) Number) (var T3)))))))
      => #t)

     )))


(define sub-apply-tests
  (lambda ()
    (display "sub-apply-tests:\t")
    (run-tests

     (test
      (sub-apply (make-sub '((var X)) '(1))
             (make-compound-term 'f '((var X))))
      => '(f 1))
     )))
     

(define extend-sub-tests
  (lambda ()
    (display "extend-sub-tests:\t")
    (run-tests

     (test
      (let ((vars-terms
             (sub-to-vars-terms-list
              (extend-sub
               (make-sub '((var T1) (var T2) (var T3))
                         '((var S1) [m (var S2) Number] Boolean))
               '(var S1)
               '[m (var T21) (f (m Number (var T23)) (var T22))]))))
        (set=? (list->set vars-terms)
               (list->set '(((var S1) (m (var T21) (f (m Number (var T23)) (var T22))))
                            ((var T1) (m (var T21) (f (m Number (var T23)) (var T22))))
                            ((var T2) (m (var S2) Number))
                            ((var T3) Boolean)))))
           => #t)

     ;; Assert error due to circular substitution
     (test
      (try
       (lambda()
         (extend-sub
          (make-sub '((var T1) (var T2) (var T3))
                    '((var S1) [m (var S2) Number] Boolean))
          '(var S1)
          '[m (var T1) (f (m Number (var T23)) (var T22))])))
      => 'error)

     ;; Assert error due to circular substitution
     (test
      (try
       (lambda()
         (extend-sub
          (make-sub '((var T1) (var T2) (var T3))
                    '((var S1) [m (var S2) Number] Boolean))
          '(var S1)
          '[m (var S1) (f (m Number (var T23)) (var T22))])))
      => 'error)

     )))


(define sub-equal-tests
  (lambda ()
    (display "sub-equal-tests:\t")
    (run-tests

     (test
      (sub-equal?
       (make-sub '((var x) (var y) (var z))
                 '(Number Boolean (f Number Boolean)))
       (make-sub '((var y) (var x) (var z))
                 '(Boolean Number (f Number Boolean))))
      => #t)

     (test (sub-equal? (make-empty-sub) (make-empty-sub))
           => #t)

     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sub-combination-tests)
(sub-apply-tests)
(extend-sub-tests)
(sub-equal-tests)
