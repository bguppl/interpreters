import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { makeVarRef, parseL3Exp, Exp, parseL3 } from '../../src/L3/L3-ast';
import { height, occursFree, referencedVars } from '../../src/L3/freeVars';
import { parse as parseSexp } from "../../src/shared/parser";

const p = (x: string): E.Either<string, Exp> => pipe(parseSexp(x), E.chain(parseL3Exp));

describe('height', () => {
    it('calculates the height of the AST', () => {
        expect(pipe(p("x"), E.map(height))).toEqual(E.of(1));
        expect(pipe(p("(lambda (x) (* x x))"), E.map(height))).toEqual(E.of(2));
        expect(pipe(p("(define f (lambda (x) (if 1 'a (* x x))))"), E.map(height))).toEqual(E.of(4));
        expect(pipe(parseL3("(L3 (let ((x 1)) x))"), E.map(height))).toEqual(E.of(3));
    });
});

describe('occursFree', () => {
    it('returns false for atoms that are not the variable itself', () => {
        expect(pipe(p("1"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("#t"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p('"s"'), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("'s"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("y"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
    });

    it('returns true when the expression is the variable itself', () => {
        expect(pipe(p("x"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
    });

    it('returns the correct result for procedures', () => {
        expect(pipe(p("(lambda () x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(lambda (x) x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("(lambda (y) x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(lambda (y) (lambda (z) x))"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(lambda (x) (lambda (z) x))"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("(lambda (y x) x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
    });

    it('returns the correct result for "if" expressions', () => {
        expect(pipe(p("(if x 1 2)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(if #t x 2)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(if #t 1 x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(if #t 1 2)"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
    });

    it('returns the correct result for applications', () => {
        expect(pipe(p("(+ 1 x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(+ 1 2)"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
    });

    it('returns the correct result for define', () => {
        expect(pipe(p("(define x (+ 1 y))"), E.map(e => occursFree("y", e)))).toEqual(E.of(true));
        expect(pipe(p("(define x (+ 1 x))"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("(define x (+ 1 y))"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
    });

    it.skip('returns the correct result for "let" expressions', () => {
        expect(pipe(p("(let () x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(let ((x 1)) x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("(let ((y 1)) x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(let ((y 1)) (lambda (z) x))"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(let ((x 1)) (lambda (z) x))"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("(let ((y 1) (x 2)) x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(false));
        expect(pipe(p("(let ((y x) (x 2)) x)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
        expect(pipe(p("(let ((y x) (x 2)) z)"), E.map(e => occursFree("x", e)))).toEqual(E.of(true));
    });
});

describe('referencesVars', () => {
    it('returns referenced variables for a given expression', () => {
        expect(pipe(p("(lambda (y) (lambda (z) x))"), E.map(e => referencedVars(e)))).toEqual(E.of([makeVarRef("x")]));
        expect(pipe(p("(+ x y)"), E.map(e => referencedVars(e)))).toEqual(E.of([makeVarRef("x"), makeVarRef("y")]));
        expect(pipe(p("(if x #t 'a)"), E.map(e => referencedVars(e)))).toEqual(E.of([makeVarRef("x")]));
        expect(pipe(p('(plus x 1 "a")'), E.map(e => referencedVars(e)))).toEqual(E.of([makeVarRef("plus"), makeVarRef("x")]));
        expect(pipe(p("(define x (+ y 1))"), E.map(e => referencedVars(e)))).toEqual(E.of([makeVarRef("y")]));
        expect(pipe(parseL3("(L3 (define y 2) (define x (+ y 1)))"), E.map(e => referencedVars(e)))).toEqual(E.of([makeVarRef("y")]));
    });
});
