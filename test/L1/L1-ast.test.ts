import { expect } from "chai";
import { isNumExp, parseL1Exp, isBoolExp, isVarRef, isDefineExp, isVarDecl, isAppExp, isProgram, parseL1 } from "../../src/L1/L1-ast";
import { isOkT, isFailure, bind } from "../../src/shared/result";
import { parse as p } from "../../src/shared/parser";

describe("L1 Parsing", () => {
    it("parses a number as NumExp", () => {
        expect(bind(p("1"), parseL1Exp)).to.satisfy(isOkT(isNumExp));
    });

    it("parses a boolean as BoolExp", () => {
        expect(bind(p("#t"), parseL1Exp)).to.satisfy(isOkT(isBoolExp));
        expect(bind(p("#f"), parseL1Exp)).to.satisfy(isOkT(isBoolExp));
    });

    it("parses a variable as VarRef", () => {
        expect(bind(p("x"), parseL1Exp)).to.satisfy(isOkT(isVarRef));
    });

    it('parses "define" expressions as DefineExp', () => {
        const parsed = bind(p("(define x 1)"), parseL1Exp);
        expect(parsed).to.satisfy(isOkT(isDefineExp));
        if (isOkT(isDefineExp)(parsed)) {
            expect(parsed.value.var).to.satisfy(isVarDecl);
            expect(parsed.value.val).to.satisfy(isNumExp);
        } else {
            expect.fail(`${JSON.stringify(parsed)} not a "define" expression`);
        }
    });

    it("parses application expressions as AppExp", () => {
        expect(bind(p("(> x 1)"), parseL1Exp)).to.satisfy(isOkT(isAppExp));
        expect(bind(p("(> (+ x x) (* x x))"), parseL1Exp)).to.satisfy(isOkT(isAppExp));
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
            expect(bind(p("(define)"), parseL1Exp)).to.satisfy(isFailure);
            expect(bind(p("(define x)"), parseL1Exp)).to.satisfy(isFailure);
            expect(bind(p("(define x y z)"), parseL1Exp)).to.satisfy(isFailure);
            expect(bind(p('(define "1" y)'), parseL1Exp)).to.satisfy(isFailure);
            expect(bind(p('(define 1 y)'), parseL1Exp)).to.satisfy(isFailure);
        });

        it("returns a Failure for an empty CExp", () => {
            expect(bind(p("(+ ())"), parseL1Exp)).to.satisfy(isFailure);
        });

        it("retruns a Failure when parsing SexpStrings", () => {
            expect(bind(p('"string"'), parseL1Exp)).to.satisfy(isFailure);
        });
    })
});
