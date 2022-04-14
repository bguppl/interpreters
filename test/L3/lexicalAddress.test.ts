import { map } from 'ramda';
import { makeNumExp, makeVarDecl, makeVarRef } from "../../src/L3/L3-ast";
import * as LA from "../../src/L3/lexicalAddress";
import { Result, makeOk, bind, mapv, isOkT } from "../../src/shared/result";
import { Sexp } from "s-expression";

describe('parseLA', () => {
    it('parses lexical addresses', () => {
        expect(LA.parseLA("1")).toEqual(makeOk(makeNumExp(1)));
        expect(LA.parseLA("(if #t (+ 1 2) 'ok)")).toSatisfy(isOkT(LA.isIfExpLA));
        expect(LA.parseLA("(lambda (x) x)")).toSatisfy(isOkT(LA.isProcExpLA));
    });
});

describe('unparseLA', () => {
    it('unparses lexical addresses', () => {
        expect(bind(LA.parseLA("1"), cexpla => makeOk(LA.unparseLA(cexpla)))).toEqual(makeOk("1"));
        expect(bind(LA.parseLA("#t"), cexpla => makeOk(LA.unparseLA(cexpla)))).toEqual(makeOk("#t"));
        expect(bind(LA.parseLA("(if #t (+ 1 2) 'ok)"), cexpla => makeOk(LA.unparseLA(cexpla)))).toEqual(makeOk(["if", "#t", ["+", "1", "2"], ["quote", "ok"]]));
        expect(bind(LA.parseLA("(lambda (x) x)"), cexpla => makeOk(LA.unparseLA(cexpla)))).toEqual(makeOk(["lambda", ["x"], "x"]));
        expect(bind(LA.parseLA("(lambda (x) (* x x))"), cexpla => makeOk(LA.unparseLA(cexpla)))).toEqual(makeOk(["lambda", ["x"], ["*", "x", "x"]]));
    });
});

describe('getLexicalAddress', () => {
    it('returns the closest enclosing lexical address given a variable name', () => {
        const b = makeVarRef("b");
        const las1 = [LA.makeLexicalAddress("a", 0, 0), LA.makeLexicalAddress("b", 0, 1)];
        expect(LA.getLexicalAddress(b, las1)).toEqual(LA.makeLexicalAddress("b", 0, 1));

        const c = makeVarRef("c");
        const las2 = [LA.makeLexicalAddress("a", 0, 0), LA.makeLexicalAddress("b", 0, 1)];
        expect(LA.getLexicalAddress(c, las2)).toEqual(LA.makeFreeVar("c"));

        const a = makeVarRef("a");
        const las3 = [LA.makeLexicalAddress("a", 0, 0), LA.makeLexicalAddress("b", 0, 1), LA.makeLexicalAddress("a", 1, 1)];
        expect(LA.getLexicalAddress(a, las3)).toEqual(LA.makeLexicalAddress("a", 0, 0));
    });
});

describe('indexOfVar', () => {
    it('returns the pos of a variable in a declaration list', () => {
        const vds = map(makeVarDecl, ["a", "b"]);
        expect(LA.indexOfVar(vds[1], vds)).toBe(1);

        expect(LA.indexOfVar(makeVarDecl("c"), vds)).toBe(-1);
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

        expect(LA.crossContour(vds, [a00, c01])).toEqual([a00, b01, a10, c11]);
    });
});

describe('addLexicalAddress', () => {
    it('works...', () => {
        const f = (s: string): Result<Sexp> =>
            bind(LA.parseLA(s), cexpla => 
                 mapv(LA.addLexicalAddresses(cexpla), cexpla => 
                      LA.unparseLA(cexpla)));

        expect(f("(lambda (x) x)")).toEqual(makeOk(["lambda", ["x"], ["x", ":", "0", "0"]]));
        
        expect(f("(lambda (x) (lambda (y) (+ x y)))")).toEqual(
            makeOk(["lambda", ["x"], ["lambda", ["y"], [["+", "free"], ["x", ":", "1", "0"], ["y", ":", "0", "0"]]]])
        );
        
        expect(f("((lambda (x) (* x x)) ((lambda (x) (+ x x)) 2))")).toEqual(
            makeOk([["lambda", ["x"], [["*", "free"], ["x", ":", "0", "0"], ["x", ":", "0", "0"]]], 
                    [["lambda", ["x"], [["+", "free"], ["x", ":", "0", "0"], ["x", ":", "0", "0"]]], "2"]])
        );
    });
});
