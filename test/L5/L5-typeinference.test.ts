import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { inferTypeOf, typeofExp } from "../../src/L5/L5-typeinference";
import { parseL5Exp, Exp } from "../../src/L5/L5-ast";
import { makeExtendTEnv, makeEmptyTEnv } from "../../src/L5/TEnv";
import { makeNumTExp } from "../../src/L5/TExp";
import { verifyTeOfExprWithInference } from "../shared/test-helpers";
import { parse as p } from "../../src/shared/parser";

describe('L5 Type Inference', () => {
    describe('inferTypeOf', () => {
        it('infers the type of atoms', () => {
            expect(inferTypeOf("5")).toEqual(E.of("number"));
            expect(inferTypeOf("#t")).toEqual(E.of("boolean"));
        });

        it('infers the type of primitive procedures', () => {
            expect(inferTypeOf("+")).toEqual(E.of("(number * number -> number)"));
            expect(inferTypeOf("-")).toEqual(E.of("(number * number -> number)"));
            expect(inferTypeOf("*")).toEqual(E.of("(number * number -> number)"));
            expect(inferTypeOf("/")).toEqual(E.of("(number * number -> number)"));
            expect(inferTypeOf("=")).toEqual(E.of("(number * number -> boolean)"));
            expect(inferTypeOf("<")).toEqual(E.of("(number * number -> boolean)"));
            expect(inferTypeOf(">")).toEqual(E.of("(number * number -> boolean)"));
            expect(inferTypeOf("not")).toEqual(E.of("(boolean -> boolean)"));
        });

        it("infers the type of primitive op applications", () => {
            expect(inferTypeOf("(+ 1 2)")).toEqual(E.of("number"));
            expect(inferTypeOf("(- 1 2)")).toEqual(E.of("number"));
            expect(inferTypeOf("(* 1 2)")).toEqual(E.of("number"));
            expect(inferTypeOf("(/ 1 2)")).toEqual(E.of("number"));

            expect(inferTypeOf("(= 1 2)")).toEqual(E.of("boolean"));
            expect(inferTypeOf("(< 1 2)")).toEqual(E.of("boolean"));
            expect(inferTypeOf("(> 1 2)")).toEqual(E.of("boolean"));

            expect(inferTypeOf("(not (< 1 2))")).toEqual(E.of("boolean"));
        });

        it('infers the type of generic primitive op application', () => {
            expect(inferTypeOf("(eq? 1 2)")).toEqual(E.of("boolean"));
            expect(inferTypeOf('(string=? "a" "b")')).toEqual(E.of("boolean"));
            expect(inferTypeOf('(number? 1)')).toEqual(E.of("boolean"));
            expect(inferTypeOf('(boolean? "a")')).toEqual(E.of("boolean"));
            expect(inferTypeOf('(string? "a")')).toEqual(E.of("boolean"));
            expect(inferTypeOf('(symbol? "a")')).toEqual(E.of("boolean"));
            expect(inferTypeOf('(list? "a")')).toEqual(E.of("boolean"));
            expect(inferTypeOf('(pair? "a")')).toEqual(E.of("boolean"));
        });

        it('infers the type of a VarRef in a given TEnv', () => {
            expect(pipe(
                p("x"),
                E.chain(parseL5Exp),
                E.chain(exp => typeofExp(exp, makeExtendTEnv(["x"], [makeNumTExp()], makeEmptyTEnv())))
            )).toEqual(E.of(makeNumTExp()));
        });

        it('infers the type of "if" expressions', () => {
            expect(inferTypeOf("(if (> 1 2) 1 2)")).toEqual(E.of("number"));
            expect(inferTypeOf("(if (= 1 2) #t #f)")).toEqual(E.of("boolean"));
        });

        it('infers the type of procedures', () => {
            expect(inferTypeOf("(lambda ((x : number)) : number x)")).toEqual(E.of("(number -> number)"));
            expect(inferTypeOf("(lambda ((x : number)) : boolean (> x 1))")).toEqual(E.of("(number -> boolean)"));
            expect(inferTypeOf("(lambda((x : number)) : (number -> number) (lambda((y : number)) : number (* y x)))")).toEqual(E.of("(number -> (number -> number))"));
            expect(inferTypeOf("(lambda((f : (number -> number))) : number (f 2))")).toEqual(E.of("((number -> number) -> number)"));
            expect(inferTypeOf("(lambda((x : number)) : number (let (((y : number) x)) (+ x y)))")).toEqual(E.of("(number -> number)"));
        });

        it('infers the type of "let" expressions', () => {
            expect(inferTypeOf("(let (((x : number) 1)) (* x 2))")).toEqual(E.of("number"));
            expect(inferTypeOf(`(let (((x : number) 1)
                                      ((y : number) 2))
                                  (lambda((a : number)) : number (+ (* x a) y)))`)).toEqual(E.of("(number -> number)"));
        });

        it('infers the type of "letrec" expressions', () => {
            expect(inferTypeOf("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) p1)")).toEqual(E.of("(number -> number)"));
            expect(inferTypeOf("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) (p1 2))")).toEqual(E.of("number"));
            expect(inferTypeOf(`(letrec (((odd? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #f (even? (- n 1)))))
                                         ((even? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #t (odd? (- n 1))))))
                                  (odd? 12))`)).toEqual(E.of("boolean"));
        });

        it('infers the type of "define" expressions as "void"', () => {
            expect(inferTypeOf("(define (foo : number) 5)")).toEqual(E.of("void"));
            expect(inferTypeOf("(define (foo : (number * number -> number)) (lambda((x : number) (y : number)) : number (+ x y)))")).toEqual(E.of("void"));
            expect(inferTypeOf("(define (x : (Empty -> number)) (lambda () : number 1))")).toEqual(E.of("void"));
            expect(inferTypeOf(`(define (x : (T1 -> (T1 -> number))) (lambda ((x : T1)) : (T1 -> number) (lambda((y : T1)) : number 5)))`)).toEqual(E.of("void"));
        });

        it('infers the type of polymorphic functions', () => {
            expect(inferTypeOf("(lambda((x : T1)) : T1 x)")).toEqual(E.of("(T1 -> T1)"));
            expect(inferTypeOf(`(let (((x : number) 1))
                                  (lambda((y : T) (z : T)) : T
                                    (if (> x 2) y z)))`)).toEqual(E.of("(T * T -> T)"));
        });

        it('infers the type of parameter-less procedures', () => {
            expect(inferTypeOf("(lambda () : number 1)")).toEqual(E.of("(Empty -> number)"));
        });
    });

    describe('typeOfExp', () => {
        it('infers return type', () => {
            expect(verifyTeOfExprWithInference("(lambda ((x : number)) x)", "(number -> number)")).toEqual(E.of(true));
        });

        it('infers parameter type', () => {
            expect(verifyTeOfExprWithInference('(lambda (x) : number x)', "(number -> number)")).toEqual(E.of(true));
        });

        it('infers both parameter and return types', () => {
            expect(verifyTeOfExprWithInference('(lambda (x) (> x 1))', "(number -> boolean)")).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference('(lambda (x) (lambda (y) (* x y)))', "(number -> (number -> number))")).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference('(let ((x 1)) (* x 2))', "number")).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference(`
                (let ((x 1)
                      (y 2))
                  (lambda (a) (+ (* x a) y)))`, "(number -> number)")).toEqual(E.of(true));

            expect(verifyTeOfExprWithInference(`
                (lambda (x)
                  (let ((y x)) (+ x y)))`, "(number -> number)")).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference('(letrec ((p1 (lambda (x) (* x x)))) p1)', '(number -> number)')).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference('(letrec ((p1 (lambda (x) (* x x)))) (p1 2))', 'number')).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference('(lambda () 1)', "(Empty -> number)")).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference(`
                (letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                         (even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))))
                  (odd? 12))`, 'boolean')).toEqual(E.of(true));
        });

        it('infers unannotated polymorphic functions', () => {
            expect(verifyTeOfExprWithInference(`(lambda (x) x)`, '(T1 -> T1)')).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference(`(lambda (f) (f 2))`, '((number -> T) -> T)')).toEqual(E.of(true));
            expect(verifyTeOfExprWithInference(`(let ((x 1)) (lambda (y z) (if (> x 2) y z)))`, '(T * T -> T)')).toEqual(E.of(true));
        });

        it('returns an error when the generic type should take two concrete types', () => {
            expect(verifyTeOfExprWithInference(`
                (letrec ((id (lambda (x) x)))
                  (if (id #t) (id 1) (id 2)))`, "Error")).toSatisfy(E.isLeft);
        });
    });
});
