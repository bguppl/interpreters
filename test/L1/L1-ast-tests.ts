import { expect } from "chai";
import { isNumExp, parseL1, isBoolExp, isVarRef, isDefineExp, isVarDecl, isAppExp, isProgram } from "../../src/L1/L1-ast";

describe("L1 Parsing", () => {
    it("parses a number as NumExp", () => {
        expect(parseL1("1")).to.satisfy(isNumExp);
    });

    it("parses a boolean as BoolExp", () => {
        expect(parseL1("#t")).to.satisfy(isBoolExp);
    });

    it("parses a variable as VarRef", () => {
        expect(parseL1("x")).to.satisfy(isVarRef);
    });

    it('parses "define" expressions as DefineExp', () => {
        let parsed = parseL1("(define x 1)");
        expect(parsed).to.satisfy(isDefineExp);
        if (isDefineExp(parsed)) {
            expect(parsed.var).to.satisfy(isVarDecl);
            expect(parsed.val).to.satisfy(isNumExp);
        }
    });

    it("parses application expressions as AppExp", () => {
        expect(parseL1("(> x 1)")).to.satisfy(isAppExp);
        expect(parseL1("(> (+ x x) (* x x))")).to.satisfy(isAppExp);
    });

    it("parses a program as Program", () => {
        expect(parseL1("(L1 (define x 1) (> (+ x 1) (* x x)))")).to.satisfy(isProgram);
    });
});
