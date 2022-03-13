import { makeVarRef, parseL3Exp, Exp } from '../../src/L3/L3-ast';
import { height, occursFree, referencedVars } from '../../src/L3/freeVars';
import { Result, makeOk, bind } from "../../src/shared/result";
import { parse as parseSexp } from "../../src/shared/parser";

const p = (x: string): Result<Exp> => bind(parseSexp(x), parseL3Exp);

describe('height', () => {
    it('calculates the height of the AST', () => {
        expect(bind(p("x"), e => makeOk(height(e)))).toEqual(makeOk(1));
        expect(bind(p("(lambda (x) (* x x))"), e => makeOk(height(e)))).toEqual(makeOk(2));
    });
});

describe('occursFree', () => {
    it('returns false for atoms that are not the variable itself', () => {
        expect(bind(p("1"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p("#t"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p('"s"'), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p("'s"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p("y"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
    });

    it('returns true when the expression is the variable itself', () => {
        expect(bind(p("x"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
    });

    it('returns the correct result for procedures', () => {
        expect(bind(p("(lambda () x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(lambda (x) x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p("(lambda (y) x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(lambda (y) (lambda (z) x))"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(lambda (x) (lambda (z) x))"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p("(lambda (y x) x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
    });

    it('returns the correct result for "if" expressions', () => {
        expect(bind(p("(if x 1 2)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(if #t x 2)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(if #t 1 x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(if #t 1 2)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
    });

    it('returns the correct result for applications', () => {
        expect(bind(p("(+ 1 x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(+ 1 2)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
    });

    it.skip('returns the correct result for "let" expressions', () => {
        expect(bind(p("(let () x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(let ((x 1)) x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p("(let ((y 1)) x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(let ((y 1)) (lambda (z) x))"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(let ((x 1)) (lambda (z) x))"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p("(let ((y 1) (x 2)) x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(false));
        expect(bind(p("(let ((y x) (x 2)) x)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
        expect(bind(p("(let ((y x) (x 2)) z)"), e => makeOk(occursFree("x", e)))).toEqual(makeOk(true));
    });
});

describe('referencesVars', () => {
    it('returns referenced variables for a given expression', () => {
        expect(bind(p("(lambda (y) (lambda (z) x))"), e => makeOk(referencedVars(e)))).toEqual(makeOk([makeVarRef("x")]));
        expect(bind(p("(+ x y)"), e => makeOk(referencedVars(e)))).toEqual(makeOk([makeVarRef("x"), makeVarRef("y")]));
        expect(bind(p("(if x 1 2)"), e => makeOk(referencedVars(e)))).toEqual(makeOk([makeVarRef("x")]));
        expect(bind(p("(plus x 1)"), e => makeOk(referencedVars(e)))).toEqual(makeOk([makeVarRef("plus"), makeVarRef("x")]));
    });
});
