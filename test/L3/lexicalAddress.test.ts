import * as E from "fp-ts/Either";
import { map } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { makeNumExp, makeVarDecl, makeVarRef } from "../../src/L3/L3-ast";
import * as LA from "../../src/L3/lexicalAddress";
import { Sexp } from "s-expression";
import { isRightT } from "../shared/test-helpers";

describe('parseLA', () => {
    it('parses lexical addresses', () => {
        expect(LA.parseLA("1")).toEqual(E.of(makeNumExp(1)));
        expect(LA.parseLA("(if #t (+ 1 2) 'ok)")).toSatisfy(isRightT(LA.isIfExpLA));
        expect(LA.parseLA("(lambda (x) x)")).toSatisfy(isRightT(LA.isProcExpLA));
    });
});

describe('unparseLA', () => {
    it('unparses lexical addresses', () => {
        expect(pipe(LA.parseLA("1"), E.map(LA.unparseLA))).toEqual(E.of("1"));
        expect(pipe(LA.parseLA("#t"), E.map(LA.unparseLA))).toEqual(E.of("#t"));
        expect(pipe(LA.parseLA("(if #t (+ 1 2) 'ok)"), E.map(LA.unparseLA))).toEqual(E.of(["if", "#t", ["+", "1", "2"], ["quote", "ok"]]));
        expect(pipe(LA.parseLA("(lambda (x) x)"), E.map(LA.unparseLA))).toEqual(E.of(["lambda", ["x"], "x"]));
        expect(pipe(LA.parseLA("(lambda (x) (* x x))"), E.map(LA.unparseLA))).toEqual(E.of(["lambda", ["x"], ["*", "x", "x"]]));
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
        const vds = pipe(["a", "b"], map(makeVarDecl));
        expect(LA.indexOfVar(vds[1], vds)).toBe(1);

        expect(LA.indexOfVar(makeVarDecl("c"), vds)).toBe(-1);
    });
});

describe('crossContour', () => {
    it('works...', () => {
        const vds = pipe(["a", "b"], map(makeVarDecl));
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
        const f = (s: string): E.Either<string, Sexp> =>
            pipe(
                LA.parseLA(s),
                E.chain(cexpla => pipe(
                    LA.addLexicalAddresses(cexpla),
                    E.map(LA.unparseLA)
                ))
            );
        expect(f("(lambda (x) x)")).toEqual(E.of(["lambda", ["x"], ["x", ":", "0", "0"]]));
        expect(f("(lambda (x) (lambda (y) (+ x y)))")).toEqual(
            E.of(["lambda", ["x"], ["lambda", ["y"], [["+", "free"], ["x", ":", "1", "0"], ["y", ":", "0", "0"]]]])
        );
        expect(f("((lambda (x) (* x x)) ((lambda (x) (+ x x)) 2))")).toEqual(
            E.of([["lambda", ["x"], [["*", "free"], ["x", ":", "0", "0"], ["x", ":", "0", "0"]]], [["lambda", ["x"], [["+", "free"], ["x", ":", "0", "0"], ["x", ":", "0", "0"]]], "2"]])
        );
    });
});
