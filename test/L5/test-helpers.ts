import { expect } from 'chai';
import { zipWith, map } from 'ramda';
import { parse, isProgram } from '../../src/L5/L5-ast';
import { inferType } from '../../src/L5/L5-type-equations';
import { unparseTExp, TExp, parseTE, makeTVar, equivalentTEs, isTVar } from '../../src/L5/TExp';
import { hasNoError, isError, safeU2, safeU } from '../../src/shared/error';
import { typeofExp } from '../../src/L5/L5-typeinference';
import { makeEmptyTEnv } from '../../src/L5/TEnv';
import { makeSub, Sub, makeEmptySub } from '../../src/L5/L5-substitution-adt';

// Safe parser
export const p = (t: string): TExp => {
    const te = parseTE(t);
    return isError(te) ? makeTVar("Error") : te;
};
// Sub constructor from concrete syntax
export const sub = (vars: string[], tes: string[]): Sub => {
    const res = makeSub(map(makeTVar, vars), map(p, tes));
    return (isError(res)) ? makeEmptySub() : res;
}

const subToStr = (sub: Sub): string =>
    `{${zipWith((v, t) => `${v.var}:${unparseTExp(t)}`, sub.vars, sub.tes).sort().join(", ")}}`;

// Compare 2 subs encoded as VarTe (set equality)
const eqSub = (sub1: Sub, sub2: Sub): boolean =>
    hasNoError([sub1, sub2]) && subToStr(sub1) == subToStr(sub2);

export const assertEqSub = (sub: Sub | Error, expected: Sub): void => {
    if (!isError(sub)) {
        expect(eqSub(sub, expected), `${subToStr(sub)} instead of\n${subToStr(expected)}`).to.be.true;
    } else {
        expect.fail(sub.toString());
    }
};

export const verifyTeOfExprWithEquations: (exp: string, texp: string) => void = (exp, texp) => {
    const e = parse(exp);
    if (isProgram(e)) {
        expect.fail("Program exps not yet supported");
        return;
    }
    if (isError(e)) {
        expect.fail(`Bad expression ${exp} - ${e}`)
        return;
    }
    const expectedType = parseTE(texp);
    if (isError(expectedType)) {
        expect.fail(`Bad expression ${texp} - ${expectedType}`)
        return;
    }
    const computedType = inferType(e);
    const ok = safeU2(equivalentTEs)(computedType, expectedType);
    expect(ok, `Expected type ${unparseTExp(expectedType)}, Computed type: ${safeU(unparseTExp)(computedType)}`).to.be.true;
};

export const verifyTeOfExprWithInference: (exp: string, texp: string) => void = (exp, texp) => {
    const e = parse(exp);
    if (isProgram(e)) {
        expect.fail("Program exps not yet supported");
        return;
    }
    if (isError(e)) {
        expect.fail(`Bad expression ${exp} - ${e}`)
        return;
    }
    const expectedType = parseTE(texp);
    if (isError(expectedType)) {
        expect.fail(`Bad expression ${texp} - ${expectedType}`)
        return;
    }
    const computedType = typeofExp(e, makeEmptyTEnv());
    if (isError(computedType)) {
        expect(expectedType).to.satisfy(isTVar);
        if (isTVar(expectedType)) {
            expect(computedType.name, `Type inference failed - expected ${texp} - ${computedType.message}`).to.equal(expectedType.var);
        }
        return;
    }
    const ok = equivalentTEs(computedType, expectedType);
    expect(ok, `Expected type ${unparseTExp(expectedType)}, Computed type: ${unparseTExp(computedType)}`).to.be.true;
};
