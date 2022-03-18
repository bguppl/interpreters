import * as E from "fp-ts/Either";
import { map, sort, zipWith } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import * as S from "fp-ts/string";
import { parseL5Exp } from '../../src/L5/L5-ast';
import { inferType } from '../../src/L5/L5-type-equations';
import { unparseTExp, parseTE, makeTVar, equivalentTEs } from '../../src/L5/TExp';
import { typeofExp } from '../../src/L5/L5-typeinference';
import { makeEmptyTEnv } from '../../src/L5/TEnv';
import { makeSub, Sub} from '../../src/L5/L5-substitution-adt';
import { parse as p } from "../../src/shared/parser";

export const isRightT = <T>(p: (x: any) => x is T) => (e: any): e is E.Right<T> =>
    E.isRight(e) && p(e.right);

// Sub constructor from concrete syntax
export const sub = (vars: readonly string[], tes: readonly string[]): E.Either<string, Sub> =>
    pipe(
        tes,
        E.traverseArray(parseTE),
        E.chain(texps => pipe(vars, map(makeTVar), tvars => makeSub(tvars, texps)))
    );

export const subToStr = (sub: Sub): E.Either<string, string> =>
    pipe(
        zipWith(sub.vars, sub.tes, (v, t) => pipe(
            unparseTExp(t),
            E.map(up => `${v.var}:${up}`)
        )),
        E.sequenceArray,
        E.map(vts => pipe(vts, sort(S.Ord)).join(", "))
    );

export const verifyTeOfExprWithEquations = (exp: string, texp: string): E.Either<string, boolean> => {
    const e = pipe(p(exp), E.chain(parseL5Exp));    
    const expectedType = parseTE(texp);
    const computedType = pipe(
        e,
        E.chain(exp => pipe(
            inferType(exp),
            E.fromOption(() => "Could not infer type"),
        ))
    );
    return pipe(
        computedType,
        E.chain(ct => pipe(
            expectedType,
            E.map(et => equivalentTEs(ct, et))
        ))
    );
};

export const verifyTeOfExprWithInference = (exp: string, texp: string): E.Either<string, boolean> => {
    const e = pipe(p(exp), E.chain(parseL5Exp));    
    const expectedType = parseTE(texp);
    const computedType = pipe(e, E.chain(exp => typeofExp(exp, makeEmptyTEnv())));
    return pipe(
        computedType,
        E.chain(ct => pipe(
            expectedType,
            E.map(et => equivalentTEs(ct, et))
        ))
    );
};
