import { expect } from 'chai';
import { map } from 'ramda';
import { makeNumExp, makeVarDecl, makeVarRef } from "../../src/L3/L3-ast";
import * as LA from "../../src/L3/lexicalAddress";

describe('parseLA', () => {
    it('parses lexical addresses', () => {
        expect(LA.parseLA("1")).to.deep.equal(makeNumExp(1));
        expect(LA.isIfExpLA(LA.parseLA("(if #t (+ 1 2) 'ok)"))).to.be.true;
        expect(LA.isProcExpLA(LA.parseLA("(lambda (x) x)"))).to.be.true;
    });
});

describe('unparseLA', () => {
    it('unparses lexical addresses', () => {
        expect(LA.unparseLA(LA.parseLA("1"))).to.equal(1);
        expect(LA.unparseLA(LA.parseLA("#t"))).to.be.true;
        expect(LA.unparseLA(LA.parseLA("(if #t (+ 1 2) 'ok)"))).to.deep.equal(["if", true, ["+", 1, 2], ["quote", "ok"]]);
        expect(LA.unparseLA(LA.parseLA("(lambda (x) x)"))).to.deep.equal(["lambda", ["x"], "x"]);
        expect(LA.unparseLA(LA.parseLA("(lambda (x) (* x x))"))).to.deep.equal(["lambda", ["x"], ["*", "x", "x"]]);
    });
});

describe('getLexicalAddress', () => {
    it('returns the closest enclosing lexical address given a variable name', () => {
        const b = makeVarRef("b");
        const las1 = [LA.makeLexicalAddress("a", 0, 0), LA.makeLexicalAddress("b", 0, 1)];
        expect(LA.getLexicalAddress(b, las1)).to.deep.equal(LA.makeLexicalAddress("b", 0, 1));

        const c = makeVarRef("c");
        const las2 = [LA.makeLexicalAddress("a", 0, 0), LA.makeLexicalAddress("b", 0, 1)];
        expect(LA.getLexicalAddress(c, las2)).to.deep.equal(LA.makeFreeVar("c"));

        const a = makeVarRef("a");
        const las3 = [LA.makeLexicalAddress("a", 0, 0), LA.makeLexicalAddress("b", 0, 1), LA.makeLexicalAddress("a", 1, 1)];
        expect(LA.getLexicalAddress(a, las3)).to.deep.equal(LA.makeLexicalAddress("a", 0, 0));
    });
});

describe('indexOfVar', () => {
    it('returns the pos of a variable in a declaration list', () => {
        const vds = map(makeVarDecl, ["a", "b"]);
        expect(LA.indexOfVar(vds[1], vds)).to.equal(1);

        expect(LA.indexOfVar(makeVarDecl("c"), vds)).to.equal(-1);
    });
});

describe('crossContour', () => {
    it('works...', () => {
        const vds = map(makeVarDecl, ["a", "b"]);
        const a00 = LA.makeLexicalAddress("a", 0, 0);
        const a10 = LA.makeLexicalAddress("a", 1, 0);
        const b01 = LA.makeLexicalAddress("b", 0, 1);
        const c01 = LA.makeLexicalAddress("c", 0, 1);
        const c11 = LA.makeLexicalAddress("c", 1, 1);

        expect(LA.crossContour(vds, [a00, c01])).to.deep.equal([a00, b01, a10, c11]);
    });
});

describe('addLexicalAddress', () => {
    it('works...', () => {
        const f = (s: string) => LA.unparseLA(LA.addLexicalAddresses(LA.parseLA(s)));
        expect(f("(lambda (x) x)")).to.deep.equal(["lambda", ["x"], ["x", ":", 0, 0]]);
        expect(f("(lambda (x) (lambda (y) (+ x y)))")).to.deep.equal(["lambda", ["x"], ["lambda", ["y"], [["+", "free"], ["x", ":", 1, 0], ["y", ":", 0, 0]]]]);
        expect(f("((lambda (x) (* x x)) ((lambda (x) (+ x x)) 2))")).to.deep.equal([["lambda", ["x"], [["*", "free"], ["x", ":", 0, 0], ["x", ":", 0, 0]]], [["lambda", ["x"], [["+", "free"], ["x", ":", 0, 0], ["x", ":", 0, 0]]], 2]]);
    });
});
