#lang racket

;; ========================================================
;; Example: a simple coroutine

;; Consider the following function g1
(define f1
  (lambda (n)
    (print (+ n 1))
    (+ n 1)))
(define f2
  (lambda (n)
    (print (+ n 2))
    (+ n 2)))
(define f3
  (lambda (n)
    (print (+ n 3))
    (+ n 3)))
        
(define g1
  (lambda (n)
    (f1 n)
    (f2 n)
    (f3 n)))

;; We would want to obtain a coroutine version of g1 similar
;; to function* / yield in JavaScript.
;; function* g2(n) {
;;   yield f1(n);
;;   yield f2(n);
;;   yield f3(n);
;; }
;; function f1(n) { return n+1; }
;; function f2(n) { return n+2; }
;; function f3(n) { return n+3; }
;; const g11 = g1(0)
;; g11.next()
;; --> { value: 1, done: false }
;; g11.next()
;; --> { value: 2, done: false }
;; g11.next()
;; --> { value: 3, done: false }
;; g11.next()
;; --> { value: undefined, done: true }

;; ========================================================
;; The basis of the model is to use (lambda () ...)
;; to delay computation we do not want to execute 
;; immediately.

;; We introduce a yield function that takes 2 arguments:
;; - The value to return to the caller
;; - The continuation to be executed when the generator is resumed
(define yield
  (lambda (res cont)
    (cons res cont)))

;; The type returned by yield is an iterator
;; It has 2 fields:
;; - Next: execute the next step of the computation of the generator
;; - Value: access the current value of the generator
;; And a state predicate 
;; - done?: determines whether the generator has reached the end of the 
;;          computation.

;; The accessors are iter->next, iter->value and iter->done?
;; iter->next either returns 'done or a new iterator.
(define iter->next
  (lambda (iter)
    (if (iter->done? iter)
        iter
        (let ((cont (cdr iter)))
          (if (eq? cont 'done)
              cont
              (cont))))))

(define iter->value
  (lambda (iter)
    (if (iter->done? iter)
        iter
        (car iter))))

(define iter->done?
  (lambda (iter)
    (eq? iter 'done)))


;; ========================================================
;; Let us now use the iterator datatype to implement the coroutine.

;; The implementation is "unpleasant" because we need to 
;; explicitly add (lambda () ...) to delay the next steps
;; of the computation after a yield.

;; g1 with interruptions and resume
(define g2
  (lambda (n)
    (yield (f1 n) 
           (lambda ()
             (yield (f2 n)
                    (lambda ()
                      (yield (f3 n)
                             'done)))))))

;; Invoke g2 and resume it
(let ((iter (g2 0)))
  ;; Iter is of the form (res . continuation)
  (print (iter->value iter))
  (iter->next iter))

;; ========================================================
;; An infinite generator with yield
(define integers
  (lambda (from)
    (letrec ((loop (lambda (n)
                     (yield n (lambda () (loop (+ n 1)))))))
      (loop from))))

(define id1 (integers 0))


;; ========================================================
;; Generator / Iterator manipulators

(define concat
  (lambda (elts item)
    (append elts (list item))))

(define iter->list
  (lambda (iter)
    (letrec ((loop (lambda (iter res)
                     (if (iter->done? iter)
                         res
                         (loop (iter->next iter)
                               (concat res (iter->value iter)))))))
      (loop iter (list)))))

(define iter->take
  (lambda (iter n)
    (letrec ((loop (lambda (iter n res)
                     (if (<= n 0)
                         res
                         (if (iter->done? iter)
                             res
                             (loop (iter->next iter)
                                   (- n 1)
                                   (concat res (iter->value iter))))))))
      (loop iter n (list)))))

(define iter->take*
  (lambda (iter n)
    (if (= n 0)
        'done
        (yield (iter->value iter)
               (lambda () (iter->take* (iter->next iter) (- n 1)))))))

(define iter->for-each
  (lambda (proc iter)
    (cond ((iter->done? iter) iter)
          (else (proc (iter->value iter))
                (iter->for-each proc (iter->next iter))))))

(define iter->map
  (lambda (proc iter)
    (if (iter->done? iter)
        iter
        (yield (proc (iter->value iter))
               (lambda () (iter->map proc (iter->next iter)))))))


;; =========================================================
(iter->list (g2 0))
(iter->take (integers 0) 10)
(iter->take (iter->map (lambda (x) (* x x)) (integers 0)) 10)
(iter->for-each (lambda (i) (print i)) (iter->take* (integers 0) 10))


;; =========================================================
;; A mutable alternative of generators that "feels" like
;; JS generators.

(define make-generator
  (lambda (thunk)
    (let ((gen (yield 'init thunk)))
      (lambda (op)
        (cond ((eq? op 'next!)
               (set! gen (iter->next gen))
               (iter->value gen))
              ((eq? op 'value)
               (iter->value gen))
              (else (error "Unknown operation")))))))

;; g1 with interruptions and resume with mutable generator
(define g3
  (lambda (n)
    (make-generator 
     (lambda ()
       (yield (f1 n) 
              (lambda ()
                (yield (f2 n)
                       (lambda ()
                         (yield (f3 n)
                                'done)))))))))

(define gen (g3 0))
(gen 'next!)
(gen 'next!)
(gen 'value)
(gen 'next!)
(gen 'next!)

