import parseSexp from "s-expression";
import { expect } from 'chai';
import { parseL1, parseL1Exp, Exp } from "../../src/L1/L1-ast";
import { evalL1program, makeEnv, makeEmptyEnv, evalExps } from '../../src/L1/L1-eval';
import { bind, makeOk } from '../../src/shared/result';

describe('L1 Eval', () => {
    it('Evaluates a program without an explicit environment', () => {
        const result = bind(parseL1("(L1 (define x 3) (+ (* x x) (+ x x)))"), evalL1program);
        expect(result).to.deep.equal(makeOk(15));
    });

    it('Evaluates a program with an explicit environment', () => {
        const env = makeEnv("x", 1, makeEmptyEnv());
        const result = bind(parseL1Exp(parseSexp("(+ x 2)")),
                            (exp: Exp) => evalExps([exp], env));
        expect(result).to.deep.equal(makeOk(3));
    });
});
