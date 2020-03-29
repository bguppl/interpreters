import { expect } from 'chai';
import { parseL1, isProgram, isCExp } from "../../src/L1/L1-ast";
import { evalL1program, makeEnv, makeEmptyEnv, isEnv, evalL1Exps } from '../../src/L1/L1-eval';

describe('L1 Eval', () => {
    it('Evaluates a program without an explicit environment', () => {
        const program = parseL1("(L1 (define x 3) (+ (* x x) (+ x x)))");
        if (isProgram(program)) {
            expect(evalL1program(program)).to.be.equal(15);
        }
    });

    it('Evaluates a program with an explicit environment', () => {
        const env = makeEnv("x", 1, makeEmptyEnv());
        const exp = parseL1("(+ x 2)");
        if (isCExp(exp) && isEnv(env)) {
            expect(evalL1Exps([exp], env)).to.be.equal(3);
        }
    });
});
