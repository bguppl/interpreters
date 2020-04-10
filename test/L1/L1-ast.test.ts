import parseSexp from "s-expression";
import { expect } from "chai";
import { isNumExp, parseL1Exp, isBoolExp, isVarRef, isDefineExp, isVarDecl, isAppExp, isProgram, Exp, parseL1, Program } from "../../src/L1/L1-ast";
import { isOk, Result } from "../../src/shared/result";

const isOkT = <T>(pred: (x: T) => boolean): (r: Result<T>) => boolean =>
    (r: Result<T>) => isOk(r) && pred(r.value);

describe("L1 Parsing", () => {
    it("parses a number as NumExp", () => {
        expect(parseL1Exp(parseSexp("1"))).to.satisfy(isOkT(isNumExp));
    });

    it("parses a boolean as BoolExp", () => {
        expect(parseL1Exp(parseSexp("#t"))).to.satisfy(isOkT(isBoolExp));
    });

    it("parses a variable as VarRef", () => {
        expect(parseL1Exp(parseSexp("x"))).to.satisfy(isOkT(isVarRef));
    });

    it('parses "define" expressions as DefineExp', () => {
        let parsed = parseL1Exp(parseSexp("(define x 1)"));
        expect(parsed).to.satisfy(isOkT(isDefineExp));
        if (isOk(parsed) && isDefineExp(parsed.value)) {
            expect(parsed.value.var).to.satisfy(isVarDecl);
            expect(parsed.value.val).to.satisfy(isNumExp);
        } else {
            expect.fail(`${JSON.stringify(parsed)} not a "define" expression`);
        }
    });

    it("parses application expressions as AppExp", () => {
        expect(parseL1Exp(parseSexp("(> x 1)"))).to.satisfy(isOkT(isAppExp));
        expect(parseL1Exp(parseSexp("(> (+ x x) (* x x))"))).to.satisfy(isOkT(isAppExp));
    });

    it("parses a program as Program", () => {
        expect(parseL1("(L1 (define x 1) (> (+ x 1) (* x x)))")).to.satisfy(isOkT(isProgram));
    });
});
