import { expect } from 'chai';
import { inferTypeOf, typeofExp } from "../../src/L5/L5-typeinference";
import { parseL5Exp, Exp } from "../../src/L5/L5-ast";
import { makeExtendTEnv, makeEmptyTEnv } from "../../src/L5/TEnv";
import { makeNumTExp } from "../../src/L5/TExp";
import { verifyTeOfExprWithInference } from "./test-helpers";
import { makeOk, bind, isFailure } from '../../src/shared/result';
import { parse as p } from "../../src/shared/parser";

describe('L5 Type Inference', () => {
    describe('inferTypeOf', () => {
        it('infers the type of atoms', () => {
            expect(inferTypeOf("5")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf("#t")).to.deep.equal(makeOk("boolean"));
        });

        it('infers the type of primitive procedures', () => {
            expect(inferTypeOf("+")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(inferTypeOf("-")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(inferTypeOf("*")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(inferTypeOf("/")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(inferTypeOf("=")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(inferTypeOf("<")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(inferTypeOf(">")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(inferTypeOf("not")).to.deep.equal(makeOk("(boolean -> boolean)"));
        });

        it('infers the type of a VarRef in a given TEnv', () => {
            expect(bind(bind(p("x"), parseL5Exp), (exp: Exp) => typeofExp(exp, makeExtendTEnv(["x"], [makeNumTExp()], makeEmptyTEnv())))).to.deep.equal(makeOk(makeNumTExp()));
        });

        it('infers the type of "if" expressions', () => {
            expect(inferTypeOf("(if (> 1 2) 1 2)")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf("(if (= 1 2) #t #f)")).to.deep.equal(makeOk("boolean"));
        });

        it('infers the type of procedures', () => {
            expect(inferTypeOf("(lambda ((x : number)) : number x)")).to.deep.equal(makeOk("(number -> number)"));
            expect(inferTypeOf("(lambda ((x : number)) : boolean (> x 1))")).to.deep.equal(makeOk("(number -> boolean)"));
            expect(inferTypeOf("(lambda((x : number)) : (number -> number) (lambda((y : number)) : number (* y x)))")).to.deep.equal(makeOk("(number -> (number -> number))"));
            expect(inferTypeOf("(lambda((f : (number -> number))) : number (f 2))")).to.deep.equal(makeOk("((number -> number) -> number)"));
            expect(inferTypeOf("(lambda((x : number)) : number (let (((y : number) x)) (+ x y)))")).to.deep.equal(makeOk("(number -> number)"));
        });

        it('infers the type of "let" expressions', () => {
            expect(inferTypeOf("(let (((x : number) 1)) (* x 2))")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf(`(let (((x : number) 1)
                                      ((y : number) 2))
                                  (lambda((a : number)) : number (+ (* x a) y)))`)).to.deep.equal(makeOk("(number -> number)"));
        });

        it('infers the type of "letrec" expressions', () => {
            expect(inferTypeOf("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) p1)")).to.deep.equal(makeOk("(number -> number)"));
            expect(inferTypeOf("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) (p1 2))")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf(`(letrec (((odd? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #f (even? (- n 1)))))
                                         ((even? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #t (odd? (- n 1))))))
                                  (odd? 12))`)).to.deep.equal(makeOk("boolean"));
        });

        it('infers the type of "define" expressions as "void"', () => {
            expect(inferTypeOf("(define (foo : number) 5)")).to.deep.equal(makeOk("void"));
            expect(inferTypeOf("(define (foo : (number * number -> number)) (lambda((x : number) (y : number)) : number (+ x y)))")).to.deep.equal(makeOk("void"));
            expect(inferTypeOf("(define (x : (Empty -> number)) (lambda () : number 1))")).to.deep.equal(makeOk("void"));
            expect(inferTypeOf(`(define (x : (T1 -> (T1 -> number))) (lambda ((x : T1)) : (T1 -> number) (lambda((y : T1)) : number 5)))`)).to.deep.equal(makeOk("void"));
        });

        it('infers the type of polymorphic functions', () => {
            expect(inferTypeOf("(lambda((x : T1)) : T1 x)")).to.deep.equal(makeOk("(T1 -> T1)"));
            expect(inferTypeOf(`(let (((x : number) 1))
                                  (lambda((y : T) (z : T)) : T
                                    (if (> x 2) y z)))`)).to.deep.equal(makeOk("(T * T -> T)"));
        });

        it('infers the type of parameter-less procedures', () => {
            expect(inferTypeOf("(lambda () : number 1)")).to.deep.equal(makeOk("(Empty -> number)"));
        });
    });

    describe('typeOfExp', () => {
        it('infers return type', () => {
            expect(verifyTeOfExprWithInference("(lambda ((x : number)) x)", "(number -> number)")).to.deep.equal(makeOk(true));
        });

        it('infers parameter type', () => {
            expect(verifyTeOfExprWithInference('(lambda (x) : number x)', "(number -> number)")).to.deep.equal(makeOk(true));
        });

        it('infers both parameter and return types', () => {
            expect(verifyTeOfExprWithInference('(lambda (x) (> x 1))', "(number -> boolean)")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(lambda (x) (lambda (y) (* x y)))', "(number -> (number -> number))")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(let ((x 1)) (* x 2))', "number")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`
                (let ((x 1)
                      (y 2))
                  (lambda (a) (+ (* x a) y)))`, "(number -> number)")).to.deep.equal(makeOk(true));

            expect(verifyTeOfExprWithInference(`
                (lambda (x)
                  (let ((y x)) (+ x y)))`, "(number -> number)")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(letrec ((p1 (lambda (x) (* x x)))) p1)', '(number -> number)')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(letrec ((p1 (lambda (x) (* x x)))) (p1 2))', 'number')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(lambda () 1)', "(Empty -> number)")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`
                (letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                         (even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))))
                  (odd? 12))`, 'boolean')).to.deep.equal(makeOk(true));
        });

        it('infers unannotated polymorphic functions', () => {
            expect(verifyTeOfExprWithInference(`(lambda (x) x)`, '(T1 -> T1)')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`(lambda (f) (f 2))`, '((number -> T) -> T)')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`(let ((x 1)) (lambda (y z) (if (> x 2) y z)))`, '(T * T -> T)')).to.deep.equal(makeOk(true));
        });

        it('returns an error when the generic type should take two concrete types', () => {
            expect(verifyTeOfExprWithInference(`
                (letrec ((id (lambda (x) x)))
                  (if (id #t) (id 1) (id 2)))`, "Error")).to.satisfy(isFailure);
        });
    });
});
