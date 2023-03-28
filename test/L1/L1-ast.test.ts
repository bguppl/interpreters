import { isNumExp, parseL1Exp, isBoolExp, isVarRef, isDefineExp, isVarDecl, isAppExp, isProgram, parseL1, Exp } from "../../src/L1/L1-ast";
import { isOkT, isFailure, Result, bind } from "../../src/shared/result";
import { parse as p } from "../../src/shared/parser";

const parse = (s: string): Result<Exp> =>
    bind(p(s), parseL1Exp);

describe("L1 Parsing", () => {
    it("parses a number as NumExp", () => {
        expect(parse("1")).toSatisfy(isOkT(isNumExp));
        expect(parse("3.0")).toSatisfy(isOkT(isNumExp));
    });

    it("parses a boolean as BoolExp", () => {
        expect(parse("#t")).toSatisfy(isOkT(isBoolExp));
        expect(parse("#f")).toSatisfy(isOkT(isBoolExp));
    });

    it("parses a variable as VarRef", () => {
        expect(parse("x")).toSatisfy(isOkT(isVarRef));
    });

    it('parses "define" expressions as DefineExp', () => {
        const parsed = parse("(define x 1)");
        expect(parsed).toSatisfy(isOkT(isDefineExp));
        if (isOkT(isDefineExp)(parsed)) {
            expect(parsed.value.var).toSatisfy(isVarDecl);
            expect(parsed.value.val).toSatisfy(isNumExp);
        } else {
            expect.fail(`${JSON.stringify(parsed)} not a "define" expression`);
        }
    });

    it("parses application expressions as AppExp", () => {
        expect(parse("(> x 1)")).toSatisfy(isOkT(isAppExp));
        expect(parse("(> (+ x x) (* x x))")).toSatisfy(isOkT(isAppExp));
    });
    
    it("parses a program as Program", () => {
        expect(parseL1("(L1 (define x 1) (> (+ x 1) (* x x)))")).toSatisfy(isOkT(isProgram));
    });

    describe("Failures", () => {
        it("returns a Failure when parsing a single-token program", () => {
            expect(parseL1("x")).toSatisfy(isFailure);
        });
    
        it("returns a Failure when parsing an empty program", () => {
            expect(parseL1("")).toSatisfy(isFailure);
        });
    
        it("returns a Failure if the program does not start with (L1 ...)", () => {
            expect(parseL1("(+ 1 2)")).toSatisfy(isFailure);
        });
    
        it("returns a Failure for a program with no Exps", () => {
            expect(parseL1("(L1)")).toSatisfy(isFailure);
        });
    
        it("returns a Failure if a program has an empty Exp", () => {
            expect(parseL1("(L1 ())")).toSatisfy(isFailure);
        });

        it('returns a Failure for an ill-formed "define"', () => {
            expect(bind(p("(define)"), parseL1Exp)).toSatisfy(isFailure);
            expect(bind(p("(define x)"), parseL1Exp)).toSatisfy(isFailure);
            expect(bind(p("(define x y z)"), parseL1Exp)).toSatisfy(isFailure);
            expect(bind(p('(define "1" y)'), parseL1Exp)).toSatisfy(isFailure);
            expect(bind(p('(define 1 y)'), parseL1Exp)).toSatisfy(isFailure);
        });

        it("returns a Failure for an empty CExp", () => {
            expect(bind(p("(+ ())"), parseL1Exp)).toSatisfy(isFailure);
        });

        it("retruns a Failure when parsing SexpStrings", () => {
            expect(bind(p('"string"'), parseL1Exp)).toSatisfy(isFailure);
        });
    })
});
