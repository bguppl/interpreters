import { expect } from 'chai';
import { makeTVar } from '../../src/L5/TExp';
import { makeSub } from '../../src/L5/L5-substitution-adt';
import { solveEquations, makeEquation } from '../../src/L5/L5-type-equations';
import { verifyTeOfExprWithEquations } from './test-helpers';

describe('L5 Type Equations', () => {
    it('solves equations', () => {
        expect(solveEquations([makeEquation(makeTVar("T1"), makeTVar("T2"))])).to.deep.equal(makeSub([makeTVar("T1")], [makeTVar("T2")]));
    });

    it('infers the types of atoms', () => {
        verifyTeOfExprWithEquations("3", "number")
    });

    it.only('infers the type of applications', () => {
        verifyTeOfExprWithEquations("(+ 1 2)", "number");
        // verifyTeOfExprWithEquations("(+ (+ 1 2) 3)", "number");
        // verifyTeOfExprWithEquations("(> 1 2)", "boolean");
        // verifyTeOfExprWithEquations("(> (+ 1 2) 2)", "boolean");
        // verifyTeOfExprWithEquations("((lambda (x) (+ x 1)) 3)", "number");
    });

    it('infers the type of primitive procedures', () => {
        verifyTeOfExprWithEquations("+", "(number * number -> number)");
        verifyTeOfExprWithEquations(">", "(number * number -> boolean)");
    });

    it('infers the type of procedures', () => {
        verifyTeOfExprWithEquations("(lambda (x) (+ x 1))", "(number -> number)");
        verifyTeOfExprWithEquations("(lambda (x) (x 1))", "((number -> T) -> T)");
        verifyTeOfExprWithEquations("(lambda (x) (+ (+ x 1) (+ x 1)))", "(number -> number)");

        // f: [N->N]
        // ==> (lambda(x) (- (f 3) (f x)))             : [N->N]
        // ==> (lambda(f) (lambda(x) (- (f 3) (f x)))) : [[N->N]->[N->N]]
        verifyTeOfExprWithEquations("(lambda (f) (lambda (x) (- (f 3) (f x))))",
                       "((number -> number) -> (number -> number))");
    });

    it('cannot infer the type of a circular type', () => {
        verifyTeOfExprWithEquations("(lambda (x) (x x))", "T");
    });

    it('cannot infer the type of a free variable without context', () => {
        verifyTeOfExprWithEquations("x", "T");
    });

    it('infers the type of a free variable in context', () => {
        verifyTeOfExprWithEquations("(+ x 1)", "number");
    });

    it('cannot infer the type with insufficient context', () => {
        verifyTeOfExprWithEquations("(f 1)", "T");
    });

    it('infers the types of primitive procedure applications with free variables', () => {
        verifyTeOfExprWithEquations("(> (f 1) 0)", "boolean");
    });

    it('infers the types of unused parameters in procedures', () => {
        verifyTeOfExprWithEquations("(lambda (x) 1)", "(T -> number)");
        verifyTeOfExprWithEquations("(lambda (x y) x)", "(T1 * T2 -> T1)");
        verifyTeOfExprWithEquations("((lambda (x) 1) 2)", "number");
    });

    it('returns an error for an incorrect number of parameters passed to procedure', () => {
        verifyTeOfExprWithEquations("((lambda () 1) 2)", "Error");
        verifyTeOfExprWithEquations("((lambda (x) 1))", "Error");
    });

    it('infers the type of "compose"', () => {
        // g: [T1->T2]
        // f: [T2->T3]
        // ==> (lambda(n) (f (g n)))               : [T1->T3]
        // ==> (lambda(f g) (lambda(n) (f (g n)))) : [[T2-T3]*[T1->T2]->[T1->T3]]
        verifyTeOfExprWithEquations("(lambda (f g) (lambda (n) (f (g n))))",
                       "((T2 -> T3) * (T1 -> T2) -> (T1 -> T3))");
    });

    it('infers the type of higher-order functions', () => {
        verifyTeOfExprWithEquations("((lambda (x) (x 1 2)) +)", "number");
        verifyTeOfExprWithEquations("((lambda (x) (x 1)) (lambda (y) y))", "number");
    });

    it('infers the type of thunks', () => {
        verifyTeOfExprWithEquations("(lambda () (lambda (x) (+ (+ x 1) (+ x 1))))", "(Empty -> (number -> number))");
    });
});
