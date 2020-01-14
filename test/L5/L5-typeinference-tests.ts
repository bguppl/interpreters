import { expect } from 'chai';
import * as assert from "assert";
import { inferTypeOf, typeofExp } from "../../src/L5/L5-typeinference";
import { parse } from "../../src/L5/L5-ast";
import { makeExtendTEnv, makeEmptyTEnv } from "../../src/L5/TEnv";
import { makeNumTExp } from "../../src/L5/TExp";
import { verifyTeOfExprWithInference } from "./test-helpers";

describe('L5 Type Inference', () => {
    describe('inferTypeOf', () => {
        it('infers the type of atoms', () => {
            expect(inferTypeOf("5")).to.equal("number");
            expect(inferTypeOf("#t")).to.equal("boolean");
        });

        it('infers the type of primitive procedures', () => {
            expect(inferTypeOf("+")).to.equal("(number * number -> number)");
            expect(inferTypeOf("-")).to.equal("(number * number -> number)");
            expect(inferTypeOf("*")).to.equal("(number * number -> number)");
            expect(inferTypeOf("/")).to.equal("(number * number -> number)");
            expect(inferTypeOf("=")).to.equal("(number * number -> boolean)");
            expect(inferTypeOf("<")).to.equal("(number * number -> boolean)");
            expect(inferTypeOf(">")).to.equal("(number * number -> boolean)");
            expect(inferTypeOf("not")).to.equal("(boolean -> boolean)");
        });

        it('infers the type of a VarRef in a given TEnv', () => {
            expect(typeofExp(parse("x"), makeExtendTEnv(["x"], [makeNumTExp()], makeEmptyTEnv()))).to.deep.equal(makeNumTExp());
        });

        it('infers the type of "if" expressions', () => {
            expect(inferTypeOf("(if (> 1 2) 1 2)")).to.equal("number");
            expect(inferTypeOf("(if (= 1 2) #t #f)")).to.equal("boolean");
        });

        it('infers the type of procedures', () => {
            expect(inferTypeOf("(lambda ((x : number)) : number x)")).to.equal("(number -> number)");
            expect(inferTypeOf("(lambda ((x : number)) : boolean (> x 1))")).to.equal("(number -> boolean)");
            expect(inferTypeOf("(lambda((x : number)) : (number -> number) (lambda((y : number)) : number (* y x)))")).to.equal("(number -> (number -> number))");
            expect(inferTypeOf("(lambda((f : (number -> number))) : number (f 2))")).to.equal("((number -> number) -> number)");
            expect(inferTypeOf("(lambda((x : number)) : number (let (((y : number) x)) (+ x y)))")).to.equal("(number -> number)");
        });

        it('infers the type of "let" expressions', () => {
            expect(inferTypeOf("(let (((x : number) 1)) (* x 2))")).to.equal("number");
            expect(inferTypeOf(`(let (((x : number) 1)
                                      ((y : number) 2))
                                  (lambda((a : number)) : number (+ (* x a) y)))`)).to.equal("(number -> number)");
        });

        it('infers the type of "letrec" expressions', () => {
            expect(inferTypeOf("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) p1)")).to.equal("(number -> number)");
            expect(inferTypeOf("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) (p1 2))")).to.equal("number");
            expect(inferTypeOf(`(letrec (((odd? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #f (even? (- n 1)))))
                                         ((even? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #t (odd? (- n 1))))))
                                  (odd? 12))`)).to.equal("boolean");
        });

        it('infers the type of "define" expressions as "void"', () => {
            expect(inferTypeOf("(define (foo : number) 5)")).to.equal("void");
            expect(inferTypeOf("(define (foo : (number * number -> number)) (lambda((x : number) (y : number)) : number (+ x y)))")).to.equal("void");
            expect(inferTypeOf("(define (x : (Empty -> number)) (lambda () : number 1))")).to.equal("void");
            expect(inferTypeOf(`(define (x : (T1 -> (T1 -> number))) (lambda ((x : T1)) : (T1 -> number) (lambda((y : T1)) : number 5)))`)).to.equal("void");
        });

        it('infers the type of polymorphic functions', () => {
            expect(inferTypeOf("(lambda((x : T1)) : T1 x)")).to.equal("(T1 -> T1)");
            expect(inferTypeOf(`(let (((x : number) 1))
                                  (lambda((y : T) (z : T)) : T
                                    (if (> x 2) y z)))`)).to.equal("(T * T -> T)");
        });

        it('infers the type of parameter-less procedures', () => {
            expect(inferTypeOf("(lambda () : number 1)")).to.equal("(Empty -> number)");
        });
    });

    describe('typeOfExp', () => {
        it('infers return type', () => {
            verifyTeOfExprWithInference("(lambda ((x : number)) x)", "(number -> number)");
        });

        it('infers parameter type', () => {
            verifyTeOfExprWithInference('(lambda (x) : number x)', "(number -> number)");
        });

        it('infers both parameter and return types', () => {
            verifyTeOfExprWithInference('(lambda (x) (> x 1))', "(number -> boolean)");
            verifyTeOfExprWithInference('(lambda (x) (lambda (y) (* x y)))', "(number -> (number -> number))");
            verifyTeOfExprWithInference('(let ((x 1)) (* x 2))', "number");
            verifyTeOfExprWithInference(`
                (let ((x 1)
                      (y 2))
                  (lambda (a) (+ (* x a) y)))`, "(number -> number)");

            verifyTeOfExprWithInference(`
                (lambda (x)
                  (let ((y x)) (+ x y)))`, "(number -> number)");
            verifyTeOfExprWithInference('(letrec ((p1 (lambda (x) (* x x)))) p1)', '(number -> number)');
            verifyTeOfExprWithInference('(letrec ((p1 (lambda (x) (* x x)))) (p1 2))', 'number');
            verifyTeOfExprWithInference('(lambda () 1)', "(Empty -> number)");
            verifyTeOfExprWithInference(`
                (letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                         (even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))))
                  (odd? 12))`, 'boolean');
        });

        it('infers unannotated polymorphic functions', () => {
            verifyTeOfExprWithInference(`(lambda (x) x)`, '(T1 -> T1)');
            verifyTeOfExprWithInference(`(lambda (f) (f 2))`, '((number -> T) -> T)');
            verifyTeOfExprWithInference(`(let ((x 1)) (lambda (y z) (if (> x 2) y z)))`, '(T * T -> T)');
        });

        it('returns an error when the generic type should take two concrete types', () => {
            verifyTeOfExprWithInference(`
                (letrec ((id (lambda (x) x)))
                  (if (id #t) (id 1) (id 2)))`, "Error");
        });
    });
});
