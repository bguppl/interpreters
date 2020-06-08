#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  ERRORS  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This defines a new `error', which throws a new kind of exception.
;; This way, it is possible to distinguish errors raised by student code
;; from errors raised by Racket.  See the `test' below: it is
;; intentionally catching only these kind of errors.

;; The new exception type, a subtype of racket errors
(define-struct (exn:fail:ppl exn:fail) ())

;; Define `error' now -- as a macro.  This is not really needed, but if
;; it's defined as a function, then errors will have this function as
;; the topmost call in the backtrace.  Defining it as a macro avoids
;; this, and make the error look like it comes straight from user code.
;; (Note that `syntax/loc' makes the macro result appear as if it's
;; coming from user code.)
(provide error)
(define-syntax (error stx)
  (syntax-case stx (set!)
    [(set! x . _)
     ;; (set! error ...) is forbidden
     (raise-syntax-error 'set! "cannot mutate module-required identifier"
                         stx #'x)]
    ;; plain (error ...) uses
    [(_ sym fmt arg ...)
     (syntax/loc stx
       (raise (make-exn:fail:ppl
               (format "~a: ~a" sym (format fmt arg ...))
               (current-continuation-marks))))]
    ;; broken (error ...) uses: use the plain function value, so it will
    ;; throw the expected kind of errors (this is a problem of misusing
    ;; `error' itself, so those would be the usual kind of errors.)
    [(_ sym . xs) (syntax/loc stx (error-as-value sym . xs))]
    ;; finally, uses of `error' as a value
    [_ (syntax/loc stx error-as-value)]))
;; This is a definition of an error function that can be used as a
;; value, when `error' is used as an expression, rather than as a
;; function call.  (Such uses could be made into a syntax error, but
;; it's better to stick to a uniform language, where almost everything
;; has a value.)  Note that it actually uses the above macro in its
;; definition, but this use is in a call, so there is no infinite loop
;; problem.  Also, the `let' is not really needed, but it makes the
;; resulting function look like `#<procedure:error>' instead of
;; `#<procedure:error-as-value>'...
(define error-as-value
  (let ([error (lambda (sym fmt . args)
                 (error sym "~a" (apply format fmt args)))])
    error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  TESTS  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a `test' form that can be used in one of several ways:
;; * (test <expr> => <result>)
;;     tests that the expression evaluates to the expected result
;; * (test <cond>)
;;     tests that the condition is true (evaluates to a non-#f result)
;; * (test <expr> =error> "text")
;;     tests that evaluating the expression throws an error with the given
;;     text, which is actually a simple "glob" pattern (where "?" is any
;;     character, "*" is any sequence of characters, and "[abc]" is any
;;     character in that set); note that it catches only errors that were
;;     thrown using the above `error'.
;; * (test input: "text" ...stuff...)
;;     same as (test ...stuff...), but with the given text as the input (which
;;     can be read as usual)
;; * (test <expr> =output> "text")
;;     tests that the generated output is the given text (not a pattern, but
;;     ignores most differences in whitespaces)
;; * (test <expr> =output> "text" "more text")
;;     same, but allows splitting the output into multiple strings for
;;     convenience
;; * (test <expr> => <result> =output> "text")
;;     combines checking the result and the output
;; These tests must be run inside a `run-tests' expression, and at the end of
;; that expression the results are reported.  If there were errors, and this is
;; running in DrRacket, then the bad tests would all get highlighted.
;; Alternatively, they can be run on the REPL without `run-tests', and then
;; each test result gets reported as soon as it is run.

;; (irrelevant here, kept so it matches my original code better)
(define test-inspector   (make-parameter (current-inspector)))
(define test-postprocess (make-parameter values))
(define (install-test-inspector)
  (test-inspector (current-inspector))
  (current-inspector (make-inspector))
  (print-struct #t))

;; parameter holding (mcons number-of-tests (list (cons failure-msg loc) ...))
;; or #f when not in a testing block
(define tests-state (make-parameter #f))

;; a "test context", that allows tests in it, and reports at the end
(provide run-tests)
(define-syntax-rule (run-tests t ...)
  (if (tests-state)
    (error 'run-tests "cannot be nested")
    (parameterize ([tests-state (mcons 0 '())]) t ... (test-report))))

;; struct for raising test errors with multiple source locations
(define-struct (exn:test exn:fail:user) (locs)
  #:property prop:exn:srclocs (lambda (e) (exn:test-locs e)))

;; report test results, first argument for "ok" message, second for errors
(define (test-report [o (current-output-port)] [e (current-error-port)])
  (define s (tests-state))
  (define (report)
    (define num   (mcar s))
    (define fails (map car (mcdr s)))
    (define locs  (map cdr (mcdr s)))
    (cond [(zero? num) (fprintf e "No tests performed!\n")]
          [(null? fails) (fprintf o "~a tests passed.\n" num)]
          ;; HACK: DrRacket always shows an icon which will pop up a stack
          ;; trace, but that doesn't make sense for a test failure report.
          ;; Worse, it would jump right into this file, one of the last things
          ;; students need to see when they're debugging their code.  Prevent
          ;; DrRacket from showing that icon, using an empty stack context.
          [else (raise
                 (make-exn:test
                  (format "Test failures!\n  ~a\n~a/~a tests failed"
                          (string-append*
                           (reverse (add-between fails "\n  ")))
                          (length fails) num)
                  (continuation-marks #f)
                  locs))]))
  (if s (report) (error 'test-report "internal error (no test data)")))

(provide test)
;; parse all possible test combinations, not pretty, but the goal is student
;; code, not super-generic
(define-for-syntax (test-gen stx loc)
  (syntax-case* stx (=> =error> =output> input:)
                (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
    [(_ expr)
     (quasisyntax/loc stx (test-1 expr 'expr '#,loc))]
    [(_ expr1 => expr2)
     (quasisyntax/loc stx (test-2 expr1 'expr1 expr2 'expr2 '#,loc))]
    [(_ input: inp x xs ...)
     (quasisyntax/loc stx
       (parameterize ([current-input-port (open-input-string inp)])
         #,(test-gen #'(test x xs ...) loc)))]
    [(_ expr =output> out)
     (quasisyntax/loc stx
       (test-o 'expr (lambda () expr) out '#,loc))]
    [(_ expr1 => expr2 =output> out)
     (quasisyntax/loc stx
       (test-o 'expr1 (lambda () (test-2 expr1 'expr1 expr2 'expr2 '#,loc))
               out '#,loc))]
    [(_ expr1 =output> out => expr2)
     (test-gen #'(test expr1 =output> out => expr2) loc)]
    [(_ expr1 =output> out1 out2 xs ...)
     (test-gen #'(test expr1 =output> (string-append out1 out2) xs ...) loc)]
    [(_ expr =error> msg-re)
     (quasisyntax/loc stx
       (test-e (lambda () expr) 'expr msg-re '#,loc))]))
(define-syntax (test stx)
  (define context (syntax-local-context))
  ;; (unless (memq context '(module top-level))
  ;;   (raise-syntax-error 'test "can only be used as a top-level expression"
  ;;                       stx))
  (define loc (list (eq? 'top-level context)
                    (syntax-source stx) (syntax-line stx) (syntax-column stx)
                    (syntax-position stx) (syntax-span stx)))
  (test-gen stx loc))

(define (test-error loc fmt . args)
  (let ([s (tests-state)]
        [msg (parameterize ([current-inspector (test-inspector)]
                            [print-struct #t])
               (apply format fmt args))]
        [top? (car loc)]
        [loc (apply make-srcloc (cdr loc))])
    (if (and (not top?) (mpair? s))
      (begin (set-mcdr! s (cons (cons msg loc) (mcdr s)))
             (set-mcar! s (add1 (mcar s))))
      (raise (make-exn:test (string-append "Test failure: " msg)
                            (current-continuation-marks)
                            (list loc))))))
(define (test-ok loc expr)
  (let ([s (tests-state)])
    (if (and (not (car loc)) (mpair? s))
      (set-mcar! s (add1 (mcar s)))
      (printf "Test passed\n"))))

(define (in-test-context! loc)
  (unless (or (car loc) (tests-state))
    (error 'test "invalid use (not in `run-tests' or in the REPL)")))

(define (test-1 val expr loc)
  (in-test-context! loc)
  (if ((test-postprocess) val)
    (test-ok loc expr)
    (test-error loc "~.s is false" expr)))
(define (test-2 val1 expr1 val2 expr2 loc)
  (in-test-context! loc)
  (parameterize ([current-inspector (test-inspector)])
    (let ([val1 ((test-postprocess) val1)]
          [val2 ((test-postprocess) val2)])
      (if (equal? val1 val2)
        (test-ok loc expr1)
        (test-error loc "~.s: expected ~e, got ~e" expr1 val2 val1)))))
(define (test-o expr thunk expected loc)
  (in-test-context! loc)
  ;; ignore whitespace sequences
  (define (clean str)
    (let* ([str (regexp-replace* #px"[[:space:]]+" str " ")]
           [str (regexp-replace #rx"^ " str "")]
           [str (regexp-replace #rx" $" str "")])
      str))
  (define outp (open-output-string))
  (parameterize ([current-output-port outp]) (thunk))
  (define output (get-output-string outp))
  (if (equal? (clean output) (clean expected))
    (test-ok loc expr)
    (test-error loc "~.s: expected output of ~e, got ~e"
                expr expected output)))
(define (test-e thunk expr msg-re loc)
  (in-test-context! loc)
  ;; catch only our `error'
  (define r (with-handlers ([exn:fail:ppl? (lambda (e) e)])
              ((test-postprocess) (thunk))))
  ;; assume normal code does not return an exception
  (cond
    [(not (exn? r)) (test-error loc "~.s did not signal an error" expr)]
    [(not (regexp-match-positions (simple-glob->regexp msg-re)
                                  (exn-message r)))
     (test-error loc "mismatched error message in ~.s (expecting \"~a\"): ~a"
                 expr msg-re (exn-message r))]
    [else (test-ok loc expr)]))

;; error glob patterns
(define glob-item-re
  (regexp (string-append "(?:"
                         "[\\]." ; escaped item
                         "|"
                         "[*?]"  ; wildcards -- the only 1-character match
                         ")")))
(define (simple-glob->regexp glob)
  (let loop ([i 0] [ps (regexp-match-positions* glob-item-re glob)] [r '()])
    (if (null? ps)
      (regexp (string-append* (reverse (cons (regexp-quote (substring glob i))
                                             r))))
      (loop (cdar ps) (cdr ps)
            ;; length=1 is only for `*' or `?'
            (cons (if (= 1 (- (cdar ps) (caar ps)))
                    (if (equal? #\* (string-ref glob (caar ps))) ".*" ".")
                    (substring glob (caar ps) (cdar ps)))
                  (if (= i (caar ps))
                    r (cons (regexp-quote (substring glob i (caar ps)))
                            r)))))))

(install-test-inspector)

