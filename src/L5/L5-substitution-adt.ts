import * as E from "fp-ts/Either";
import { elem, map, zipWith } from "fp-ts/ReadonlyArray";
import * as B from "fp-ts/boolean";
import { pipe } from "fp-ts/function";
import * as S from "fp-ts/string";
import { eqTVar, isAtomicTExp, isProcTExp, isTVar, makeProcTExp, unparseTExp, TExp, TVar } from "./TExp";
import { cons, isEmpty, first, rest } from "../shared/list";

// Implementation of the Substitution ADT
// ========================================================
// A substitution is represented as a 2 element list of equal length
// lists of variables and type expression.
// The empty substitution is [[], []]

export interface Sub { tag: "Sub"; vars: readonly TVar[]; tes: readonly TExp[]; }
export const isSub = (x: any): x is Sub => x.tag === "Sub";

// Constructors:
// Signature: makeSub(vars, tes)
// Purpose: Create a substitution in which the i-th element of 'variables'
//          is mapped to the i-th element of 'tes'.
// Example: makeSub(
//             map(parseTE, ["x", "y", "z"]),
//             map(parseTE, ["number", "boolean", "(number -> number)"])
//          => {tag: "Sub", vars: [x y z], [numTexp, boolTexp, ProcTexp([NumTexp, NumTexp])]}
//          makeSub(map(parseTE, ["x", "y", "z"]),
//                  map(parseTE, ["number", "boolean", "(z -> number)"]))
//          => error makeSub: circular substitution
// Pre-condition: (length variables) = (length tes)
//                variables has no repetitions (set)
export const makeSub = (vars: readonly TVar[], tes: readonly TExp[]): E.Either<string, Sub> =>
    pipe(
        zipWith(vars, tes, checkNoOccurrence),
        E.sequenceArray,
        E.map(_ => ({ tag: "Sub", vars: vars, tes: tes }))
    );

export const makeEmptySub = (): Sub => ({ tag: "Sub", vars: [], tes: [] });

// Purpose: when attempting to bind tvar to te in a sub - check whether tvar occurs in te.
// Return error if a circular reference is found.
export const checkNoOccurrence = (tvar: TVar, te: TExp): E.Either<string, true> => {
    const check = (e: TExp): E.Either<string, true> =>
        isTVar(e) ?((e.var === tvar.var) ? pipe(unparseTExp(te), E.chain(up => E.left(`Occur check error - circular sub ${tvar.var} in ${up}`))) : E.of(true)) :
        isAtomicTExp(e) ? E.of(true) :
        isProcTExp(e) ? pipe(e.paramTEs, E.traverseArray(check), E.chain(_ => check(e.returnTE))) :
        E.left(`Bad type expression ${e} in ${te}`);
    return check(te);
};

export const isEmptySub = (sub: any): boolean => isSub(sub) && isEmpty(sub.vars) && isEmpty(sub.tes);

// Purpose: If v is in sub.vars - return corresponding te, else v unchanged.
export const subGet = (sub: Sub, v: TVar): TExp => {
    const lookup = (vars: readonly TVar[], tes: readonly TExp[]): TExp =>
        isEmpty(vars) ? v :
        eqTVar(first(vars), v) ? first(tes) :
        lookup(rest(vars), rest(tes));
    return lookup(sub.vars, sub.tes);
};

// ============================================================
// Purpose: apply a sub to a TExp
// Example:
// unparseTexp(applySub(makeSub(map(parseTE, ["T1", "T2"]), map(parseTE, ["number", "boolean"])),
//                      parseTE("(T1 * T2 -> T1)")) =>
// "(number * boolean -> number)"
export const applySub = (sub: Sub) => (te: TExp): TExp =>
    isEmptySub(sub) ? te :
    isAtomicTExp(te) ? te :
    isTVar(te) ? subGet(sub, te) :
    isProcTExp(te) ? pipe(te.paramTEs, map(applySub(sub)), params => makeProcTExp(params, applySub(sub)(te.returnTE))) :
    te;

// ============================================================
// Purpose: Returns the composition of substitutions s.t.:
//  applySub(result, te) === applySub(sub2, applySub(sub1, te))
export const combineSub = (sub1: Sub, sub2: Sub): E.Either<string, Sub> =>
    isEmptySub(sub1) ? E.of(sub2) :
    isEmptySub(sub2) ? E.of(sub1) :
    combine(sub1, sub2.vars, sub2.tes);

const combine = (sub: Sub, vars: readonly TVar[], tes: readonly TExp[]): E.Either<string, Sub> =>
    isEmpty(vars) ? E.of(sub) :
    pipe(
        extendSub(sub, first(vars), first(tes)),
        E.chain(extSub => combine(extSub, rest(vars), rest(tes)))
    );

// Purpose: extend a substitution with one pair (tv, te)
// Calls to makeSub to do the occur-check
export const extendSub = (sub: Sub, v: TVar, te: TExp): E.Either<string, Sub> =>
    pipe(
        makeSub([v], [te]),
        E.chain(sub2 => pipe(
            sub.tes,
            map(applySub(sub2)),
            updatedTEs => pipe(
                sub.vars,
                map(v => v.var),
                elem(S.Eq)(v.var),
                B.match(
                    () => makeSub(cons(v, sub.vars), cons(te, updatedTEs)),
                    () => makeSub(sub.vars, updatedTEs)
                )
            )
        ))
    );
