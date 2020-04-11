import p from "s-expression";
import { expect } from "chai";
import { isNumExp, parseL1Exp, isBoolExp, isVarRef, isDefineExp, isVarDecl, isAppExp, isProgram, parseL1 } from "../../src/L1/L1-ast";
import { isOk, isOkT, isFailure } from "../../src/shared/result";

describe("L1 Parsing", () => {
    it("parses a number as NumExp", () => {
        expect(parseL1Exp(p("1"))).to.satisfy(isOkT(isNumExp));
    });

    it("parses a boolean as BoolExp", () => {
        expect(parseL1Exp(p("#t"))).to.satisfy(isOkT(isBoolExp));
        expect(parseL1Exp(p("#f"))).to.satisfy(isOkT(isBoolExp));
    });

    it("parses a variable as VarRef", () => {
        expect(parseL1Exp(p("x"))).to.satisfy(isOkT(isVarRef));
    });

    it('parses "define" expressions as DefineExp', () => {
        let parsed = parseL1Exp(p("(define x 1)"));
        expect(parsed).to.satisfy(isOkT(isDefineExp));
        if (isOk(parsed) && isDefineExp(parsed.value)) {
            expect(parsed.value.var).to.satisfy(isVarDecl);
            expect(parsed.value.val).to.satisfy(isNumExp);
        } else {
            expect.fail(`${JSON.stringify(parsed)} not a "define" expression`);
        }
    });

    it("parses application expressions as AppExp", () => {
        expect(parseL1Exp(p("(> x 1)"))).to.satisfy(isOkT(isAppExp));
        expect(parseL1Exp(p("(> (+ x x) (* x x))"))).to.satisfy(isOkT(isAppExp));
    });
    
    it("parses a program as Program", () => {
        expect(parseL1("(L1 (define x 1) (> (+ x 1) (* x x)))")).to.satisfy(isOkT(isProgram));
    });

    describe("Failures", () => {
        it("returns a Failure when parsing a single-token program", () => {
            expect(parseL1("x")).to.satisfy(isFailure);
        });
    
        it("returns a Failure when parsing an empty program", () => {
            expect(parseL1("")).to.satisfy(isFailure);
        });
    
        it("returns a Failure if the program does not start with (L1 ...)", () => {
            expect(parseL1("(+ 1 2)")).to.satisfy(isFailure);
        });
    
        it("returns a Failure for a program with no Exps", () => {
            expect(parseL1("(L1)")).to.satisfy(isFailure);
        });
    
        it("returns a Failure if a program has an empty Exp", () => {
            expect(parseL1("(L1 ())")).to.satisfy(isFailure);
        });

        it('returns a Failure for an ill-formed "define"', () => {
            expect(parseL1Exp(p("(define)"))).to.satisfy(isFailure);
            expect(parseL1Exp(p("(define x)"))).to.satisfy(isFailure);
            expect(parseL1Exp(p("(define x y z)"))).to.satisfy(isFailure);
            expect(parseL1Exp(p('(define "1" y)'))).to.satisfy(isFailure);
            expect(parseL1Exp(p('(define 1 y)'))).to.satisfy(isFailure);
        });

        it("returns a Failure for an empty CExp", () => {
            expect(parseL1Exp(p("(+ ())"))).to.satisfy(isFailure);
        });

        it("retruns a Failure when parsing SexpStrings", () => {
            expect(parseL1Exp(p('"string"'))).to.satisfy(isFailure);
        });
    })
});
