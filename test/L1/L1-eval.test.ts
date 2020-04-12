import { expect } from 'chai';
import { parseL1, parseL1Exp, Exp } from "../../src/L1/L1-ast";
import { evalL1program, makeEnv, makeEmptyEnv, evalSequence } from '../../src/L1/L1-eval';
import { bind, makeOk, isFailure } from '../../src/shared/result';
import p from "../../src/shared/parser";

describe('L1 Eval', () => {
    it('Evaluates a program without an explicit environment', () => {
        expect(bind(parseL1(`(L1 (define x 3) + #t (+ (* x x) (+ x x)))`), evalL1program)).to.deep.equal(makeOk(15));
    });

    it('Evaluates a program with an explicit environment', () => {
        const env1 = makeEnv("x", 1, makeEmptyEnv());
        const result1 = bind(bind(p("(+ x 2)"), parseL1Exp),
                             (exp: Exp) => evalSequence([exp], env1));
        expect(result1).to.deep.equal(makeOk(3));
        
        const env2 = makeEnv("x", 1, makeEnv("y", 2, makeEmptyEnv()));
        const result2 = bind(bind(p("(+ y 2)"), parseL1Exp),
                             (exp: Exp) => evalSequence([exp], env2));
        expect(result2).to.deep.equal(makeOk(4));
    });

    describe("Failures", () => {
        it("returns a Failure when accessing a variable in an empty env", () => {
            const env = makeEmptyEnv();
            const result = bind(bind(p("(+ y 2)"), parseL1Exp),
                                (exp: Exp) => evalSequence([exp], env));
            expect(result).to.satisfy(isFailure);
        });

        it("returns a Failure when accessing a variable not present in the env", () => {
            const env = makeEnv("x", 1, makeEmptyEnv());
            const result = bind(bind(p("(+ y 2)"), parseL1Exp),
                                (exp: Exp) => evalSequence([exp], env));
            expect(result).to.satisfy(isFailure);
        });

        it("returns a Failure when evaluating an empty sequence of Exps", () => {
            expect(evalL1program({ tag: "Program", exps: []})).to.satisfy(isFailure);
        });

        it("returns a Failure for an unknown primitive op", () => {
            expect(bind(bind(p("(eq? 1 1)"), parseL1Exp), exp => evalSequence([exp], makeEmptyEnv()))).to.satisfy(isFailure);
        })
    });
});
