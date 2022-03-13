import { makeTVar } from '../../src/L5/TExp';
import { makeSub } from '../../src/L5/L5-substitution-adt';
import { solveEquations, makeEquation } from '../../src/L5/L5-type-equations';
import { verifyTeOfExprWithEquations } from './test-helpers';
import { makeOk, isFailure } from '../../src/shared/result';

describe('L5 Type Equations', () => {
    it('solves equations', () => {
        expect(solveEquations([makeEquation(makeTVar("T1"), makeTVar("T2"))])).toEqual(makeSub([makeTVar("T1")], [makeTVar("T2")]));
    });

    it('infers the types of atoms', () => {
        expect(verifyTeOfExprWithEquations("3", "number")).toEqual(makeOk(true));
    });

    it('infers the type of applications', () => {
        expect(verifyTeOfExprWithEquations("(+ 1 2)", "number")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(+ (+ 1 2) 3)", "number")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(> 1 2)", "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(> (+ 1 2) 2)", "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("((lambda (x) (+ x 1)) 3)", "number")).toEqual(makeOk(true));
    });

    it('infers the type of primitive procedures', () => {
        expect(verifyTeOfExprWithEquations("+", "(number * number -> number)")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations(">", "(number * number -> boolean)")).toEqual(makeOk(true));
    });

    it("infers the type of primitive op applications", () => {
        expect(verifyTeOfExprWithEquations("(+ 1 2)", "number")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(- 1 2)", "number")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(* 1 2)", "number")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(/ 1 2)", "number")).toEqual(makeOk(true));

        expect(verifyTeOfExprWithEquations("(= 1 2)", "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(< 1 2)", "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(> 1 2)", "boolean")).toEqual(makeOk(true));

        expect(verifyTeOfExprWithEquations("(not (< 1 2))", "boolean")).toEqual(makeOk(true));
    });

    it('infers the type of generic primitive op application', () => {
        expect(verifyTeOfExprWithEquations("(eq? 1 2)", "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations('(string=? "a" "b")', "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations('(number? 1)', "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations('(boolean? "a")', "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations('(string? "a")', "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations('(symbol? "a")', "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations('(list? "a")', "boolean")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations('(pair? "a")', "boolean")).toEqual(makeOk(true));
    });

    it('infers the type of procedures', () => {
        expect(verifyTeOfExprWithEquations("(lambda (x) (+ x 1))", "(number -> number)")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(lambda (x) (x 1))", "((number -> T) -> T)")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(lambda (x) (+ (+ x 1) (+ x 1)))", "(number -> number)")).toEqual(makeOk(true));

        // f: [N->N]
        // ==> (lambda(x) (- (f 3) (f x)))             : [N->N]
        // ==> (lambda(f) (lambda(x) (- (f 3) (f x)))) : [[N->N]->[N->N]]
        expect(verifyTeOfExprWithEquations("(lambda (f) (lambda (x) (- (f 3) (f x))))",
                       "((number -> number) -> (number -> number))")).toEqual(makeOk(true));
    });

    it('cannot infer the type of a circular type', () => {
        expect(verifyTeOfExprWithEquations("(lambda (x) (x x))", "T")).toSatisfy(isFailure);
    });

    it('cannot infer the type of a free variable without context', () => {
        expect(verifyTeOfExprWithEquations("x", "T")).toEqual(makeOk(true));
    });

    it('infers the type of a free variable in context', () => {
        expect(verifyTeOfExprWithEquations("(+ x 1)", "number")).toEqual(makeOk(true));
    });

    it('cannot infer the type with insufficient context', () => {
        expect(verifyTeOfExprWithEquations("(f 1)", "T")).toEqual(makeOk(true));
    });

    it('infers the types of primitive procedure applications with free variables', () => {
        expect(verifyTeOfExprWithEquations("(> (f 1) 0)", "boolean")).toEqual(makeOk(true));
    });

    it('infers the types of unused parameters in procedures', () => {
        expect(verifyTeOfExprWithEquations("(lambda (x) 1)", "(T -> number)")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("(lambda (x y) x)", "(T1 * T2 -> T1)")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("((lambda (x) 1) 2)", "number")).toEqual(makeOk(true));
    });

    it('returns an error for an incorrect number of parameters passed to procedure', () => {
        expect(verifyTeOfExprWithEquations("((lambda () 1) 2)", "Error")).toSatisfy(isFailure);
        expect(verifyTeOfExprWithEquations("((lambda (x) 1))", "Error")).toSatisfy(isFailure);
    });

    it('infers the type of "compose"', () => {
        // g: [T1->T2]
        // f: [T2->T3]
        // ==> (lambda(n) (f (g n)))               : [T1->T3]
        // ==> (lambda(f g) (lambda(n) (f (g n)))) : [[T2-T3]*[T1->T2]->[T1->T3]]
        expect(verifyTeOfExprWithEquations("(lambda (f g) (lambda (n) (f (g n))))",
                       "((T2 -> T3) * (T1 -> T2) -> (T1 -> T3))")).toEqual(makeOk(true));
    });

    it('infers the type of higher-order functions', () => {
        expect(verifyTeOfExprWithEquations("((lambda (x) (x 1 2)) +)", "number")).toEqual(makeOk(true));
        expect(verifyTeOfExprWithEquations("((lambda (x) (x 1)) (lambda (y) y))", "number")).toEqual(makeOk(true));
    });

    it('infers the type of thunks', () => {
        expect(verifyTeOfExprWithEquations("(lambda () (lambda (x) (+ (+ x 1) (+ x 1))))", "(Empty -> (number -> number))")).toEqual(makeOk(true));
    });
});
