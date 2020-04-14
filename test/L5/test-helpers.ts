import { expect } from 'chai';
import { zipWith, map } from 'ramda';
import { parseL5Exp, isProgram, Exp } from '../../src/L5/L5-ast';
import { inferType } from '../../src/L5/L5-type-equations';
import { unparseTExp, TExp, parseTE, makeTVar, equivalentTEs, isTVar } from '../../src/L5/TExp';
import { typeofExp } from '../../src/L5/L5-typeinference';
import { makeEmptyTEnv } from '../../src/L5/TEnv';
import { makeSub, Sub, makeEmptySub } from '../../src/L5/L5-substitution-adt';
import { Result, bind as bindResult, mapResult, zipWithResult, makeOk, resultToOptional, safe2 } from '../../src/shared/result';
import { bind as bindOptional, optionalToResult } from "../../src/shared/optional";
import { parse as p } from "../../src/shared/parser";

// Sub constructor from concrete syntax
export const sub = (vars: string[], tes: string[]): Result<Sub> =>
    bindResult(mapResult(parseTE, tes),
               (texps: TExp[]) => makeSub(map(makeTVar, vars), texps));

export const subToStr = (sub: Sub): Result<string> =>
    bindResult(zipWithResult((v, t) => bindResult(unparseTExp(t), up => makeOk(`${v.var}:${up}`)), sub.vars, sub.tes),
               (vts: string[]) => makeOk(vts.sort().join(", ")));

export const verifyTeOfExprWithEquations = (exp: string, texp: string): Result<boolean> => {
    const e = bindResult(p(exp), parseL5Exp);
    const expectedType = parseTE(texp);
    const computedType = bindResult(e, (exp: Exp) => optionalToResult(inferType(exp), "Could not infer type"));
    const ok = safe2((ct: TExp, et: TExp) => makeOk(equivalentTEs(ct, et)))(computedType, expectedType);
    return ok;
};

// export const verifyTeOfExprWithInference: (exp: string, texp: string) => void = (exp, texp) => {
//     const e = parse(exp);
//     if (isProgram(e)) {
//         expect.fail("Program exps not yet supported");
//         return;
//     }
//     if (isError(e)) {
//         expect.fail(`Bad expression ${exp} - ${e}`)
//         return;
//     }
//     const expectedType = parseTE(texp);
//     if (isError(expectedType)) {
//         expect.fail(`Bad expression ${texp} - ${expectedType}`)
//         return;
//     }
//     const computedType = typeofExp(e, makeEmptyTEnv());
//     if (isError(computedType)) {
//         expect(expectedType).to.satisfy(isTVar);
//         if (isTVar(expectedType)) {
//             expect(computedType.name, `Type inference failed - expected ${texp} - ${computedType.message}`).to.equal(expectedType.var);
//         }
//         return;
//     }
//     const ok = equivalentTEs(computedType, expectedType);
//     expect(ok, `Expected type ${unparseTExp(expectedType)}, Computed type: ${unparseTExp(computedType)}`).to.be.true;
// };
