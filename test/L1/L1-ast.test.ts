import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { isNumExp, parseL1Exp, isBoolExp, isVarRef, isDefineExp, isVarDecl, isAppExp, isProgram, parseL1 } from "../../src/L1/L1-ast";
import { parse as p } from "../../src/shared/parser";
import { isRightT } from "../shared/test-helpers";

describe("L1 Parsing", () => {
    it("parses a number as NumExp", () => {
        expect(pipe(p("1"), E.chain(parseL1Exp))).toSatisfy(isRightT(isNumExp));
    });

    it("parses a boolean as BoolExp", () => {
        expect(pipe(p("#t"), E.chain(parseL1Exp))).toSatisfy(isRightT(isBoolExp));
        expect(pipe(p("#f"), E.chain(parseL1Exp))).toSatisfy(isRightT(isBoolExp));
    });

    it("parses a variable as VarRef", () => {
        expect(pipe(p("x"), E.chain(parseL1Exp))).toSatisfy(isRightT(isVarRef));
    });

    it('parses "define" expressions as DefineExp', () => {
        const parsed = pipe(p("(define x 1)"), E.chain(parseL1Exp));
        expect(parsed).toSatisfy(isRightT(isDefineExp));
        if (isRightT(isDefineExp)(parsed)) {
            expect(parsed.right.var).toSatisfy(isVarDecl);
            expect(parsed.right.val).toSatisfy(isNumExp);
        } else {
            expect.fail(`${JSON.stringify(parsed)} not a "define" expression`);
        }
    });

    it("parses application expressions as AppExp", () => {
        expect(pipe(p("(> x 1)"), E.chain(parseL1Exp))).toSatisfy(isRightT(isAppExp));
        expect(pipe(p("(> (+ x x) (* x x))"), E.chain(parseL1Exp))).toSatisfy(isRightT(isAppExp));
    });
    
    it("parses a program as Program", () => {
        expect(parseL1("(L1 (define x 1) (> (+ x 1) (* x x)))")).toSatisfy(isRightT(isProgram));
    });

    describe("Failures", () => {
        it("returns a Failure when parsing a single-token program", () => {
            expect(parseL1("x")).toSatisfy(E.isLeft);
        });
    
        it("returns a Failure when parsing an empty program", () => {
            expect(parseL1("")).toSatisfy(E.isLeft);
        });
    
        it("returns a Failure if the program does not start with (L1 ...)", () => {
            expect(parseL1("(+ 1 2)")).toSatisfy(E.isLeft);
        });
    
        it("returns a Failure for a program with no Exps", () => {
            expect(parseL1("(L1)")).toSatisfy(E.isLeft);
        });
    
        it("returns a Failure if a program has an empty Exp", () => {
            expect(parseL1("(L1 ())")).toSatisfy(E.isLeft);
        });

        it('returns a Failure for an ill-formed "define"', () => {
            expect(pipe(p("(define)"), E.chain(parseL1Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(define x)"), E.chain(parseL1Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(define x y z)"), E.chain(parseL1Exp))).toSatisfy(E.isLeft);
            expect(pipe(p('(define "1" y)'), E.chain(parseL1Exp))).toSatisfy(E.isLeft);
            expect(pipe(p('(define 1 y)'), E.chain(parseL1Exp))).toSatisfy(E.isLeft);
        });

        it("returns a Failure for an empty CExp", () => {
            expect(pipe(p("(+ ())"), E.chain(parseL1Exp))).toSatisfy(E.isLeft);
        });

        it("retruns a Failure when parsing SexpStrings", () => {
            expect(pipe(p('"string"'), E.chain(parseL1Exp))).toSatisfy(E.isLeft);
        });
    })
});
