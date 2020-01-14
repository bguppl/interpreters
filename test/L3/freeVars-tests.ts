import { expect } from 'chai';
import { makeVarRef, parseL3 } from '../../src/L3/L3-ast';
import { height, occursFree, referencedVars } from '../../src/L3/freeVars';

describe('height', () => {
    it('calculates the height of the AST', () => {
        expect(height(parseL3("x"))).to.equal(1);
        expect(height(parseL3("(lambda (x) (* x x))"))).to.equal(2);
    });
});

describe('occursFree', () => {
    it('returns false for atoms that are not the variable itself', () => {
        expect(occursFree("x", parseL3("1"))).to.be.false;
        expect(occursFree("x", parseL3("#t"))).to.be.false;
        expect(occursFree("x", parseL3('"s"'))).to.be.false;
        expect(occursFree("x", parseL3("'s"))).to.be.false;
        expect(occursFree("x", parseL3("y"))).to.be.false;
    });

    it('returns true when the expression is the variable itself', () => {
        expect(occursFree("x", parseL3("x"))).to.be.true;
    });

    it('returns the correct result for procedures', () => {
        expect(occursFree("x", parseL3("(lambda () x)"))).to.be.true;
        expect(occursFree("x", parseL3("(lambda (x) x)"))).to.be.false;
        expect(occursFree("x", parseL3("(lambda (y) x)"))).to.be.true;
        expect(occursFree("x", parseL3("(lambda (y) (lambda (z) x))"))).to.be.true;
        expect(occursFree("x", parseL3("(lambda (x) (lambda (z) x))"))).to.be.false;
        expect(occursFree("x", parseL3("(lambda (y x) x)"))).to.be.false;
    });

    it('returns the correct result for "if" expressions', () => {
        expect(occursFree("x", parseL3("(if x 1 2)"))).to.be.true;
        expect(occursFree("x", parseL3("(if #t x 2)"))).to.be.true;
        expect(occursFree("x", parseL3("(if #t 1 x)"))).to.be.true;
        expect(occursFree("x", parseL3("(if #t 1 2)"))).to.be.false;
    });

    it('returns the correct result for applications', () => {
        expect(occursFree("x", parseL3("(+ 1 x)"))).to.be.true;
        expect(occursFree("x", parseL3("(+ 1 2)"))).to.be.false;
    });

    it.skip('returns the correct result for "let" expressions', () => {
        expect(occursFree("x", parseL3("(let () x)"))).to.be.true;
        expect(occursFree("x", parseL3("(let ((x 1)) x)"))).to.be.false;
        expect(occursFree("x", parseL3("(let ((y 1)) x)"))).to.be.true;
        expect(occursFree("x", parseL3("(let ((y 1)) (lambda (z) x))"))).to.be.true;
        expect(occursFree("x", parseL3("(let ((x 1)) (lambda (z) x))"))).to.be.false;
        expect(occursFree("x", parseL3("(let ((y 1) (x 2)) x)"))).to.be.false;
        expect(occursFree("x", parseL3("(let ((y x) (x 2)) x)"))).to.be.true;
        expect(occursFree("x", parseL3("(let ((y x) (x 2)) z)"))).to.be.true;
    });
});

describe('referencesVars', () => {
    it('returns referenced variables for a given expression', () => {
        expect(referencedVars(parseL3("(lambda (y) (lambda (z) x))"))).to.deep.equal([makeVarRef("x")]);
        expect(referencedVars(parseL3("(+ x y)"))).to.deep.equal([makeVarRef("x"), makeVarRef("y")]);
        expect(referencedVars(parseL3("(if x 1 2)"))).to.deep.equal([makeVarRef("x")]);
        expect(referencedVars(parseL3("(plus x 1)"))).to.deep.equal([makeVarRef("plus"), makeVarRef("x")]);
    });
});
