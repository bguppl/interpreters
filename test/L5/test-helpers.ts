import { map } from 'ramda';
import { parseL5Exp, Exp } from '../../src/L5/L5-ast';
import { inferType } from '../../src/L5/L5-type-equations';
import { unparseTExp, TExp, parseTE, makeTVar, equivalentTEs } from '../../src/L5/TExp';
import { typeofExp } from '../../src/L5/L5-typeinference';
import { makeEmptyTEnv } from '../../src/L5/TEnv';
import { makeSub, Sub} from '../../src/L5/L5-substitution-adt';
import { Result, bind as bindResult, mapResult, zipWithResult, makeOk, safe2 } from '../../src/shared/result';
import { optionalToResult } from "../../src/shared/optional";
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
    return safe2((ct: TExp, et: TExp) => makeOk(equivalentTEs(ct, et)))(computedType, expectedType);
};

export const verifyTeOfExprWithInference = (exp: string, texp: string): Result<boolean> => {
    const e = bindResult(p(exp), parseL5Exp);
    const expectedType = parseTE(texp);
    const computedType = bindResult(e, (exp: Exp) => typeofExp(exp, makeEmptyTEnv()));
    return safe2((ct: TExp, et: TExp) => makeOk(equivalentTEs(ct, et)))(computedType, expectedType);
};
