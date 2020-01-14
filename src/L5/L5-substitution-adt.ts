import { curry, map, prop, zipWith } from 'ramda';
import { isEmpty } from "./L5-ast";
import { eqTVar, isAtomicTExp, isProcTExp, isTVar, makeProcTExp, unparseTExp, TExp, TVar } from "./TExp";
import { getErrorMessages, hasNoError, isError } from "../shared/error";
import { first, rest } from "../shared/list";

// Implementation of the Substitution ADT
// ========================================================
// A substitution is represented as a 2 element list of equal length
// lists of variables and type expression.
// The empty substitution is [[], []]

export interface Sub {tag: "Sub"; vars: TVar[]; tes: TExp[]; };
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
export const makeSub = (vars: TVar[], tes: TExp[]): Error | Sub => {
    // console.log(`makeSub ${map(prop('var'), vars)} with ${map(unparseTExp, tes)}`);
    const noOccurrences = zipWith(checkNoOccurrence, vars, tes);
    if (hasNoError(noOccurrences))
        return {tag: "Sub", vars: vars, tes: tes};
    else
        return Error(getErrorMessages(noOccurrences));
};

export const makeEmptySub = (): Sub => ({tag: "Sub", vars: [], tes: []});

// Purpose: when attempting to bind tvar to te in a sub - check whether tvar occurs in te.
// Return error if a circular reference is found.
export const checkNoOccurrence = (tvar: TVar, te: TExp): true | Error => {
    const check = (e: TExp): true | Error =>
        isTVar(e) ? ((e.var === tvar.var) ? Error(`Occur check error - circular sub ${tvar.var} in ${unparseTExp(te)}`) : true) :
        isAtomicTExp(e) ? true :
        isProcTExp(e) ? (hasNoError(map(check, e.paramTEs)) ?
                          check(e.returnTE) :
                          Error(getErrorMessages(map(check, e.paramTEs)))) :
        Error(`Bad type expression ${e} in ${te}`);
    // console.log(`checkNoOcc ${tvar.var} in ${unparseTExp(te)}`);
    return check(te);
};

export const isEmptySub = (sub: any): boolean => isSub(sub) && isEmpty(sub.vars) && isEmpty(sub.tes);

// Purpose: If v is in sub.vars - return corresponding te, else v unchanged.
export const subGet = (sub: Sub, v: TVar): TExp => {
    const lookup = (vars: TVar[], tes: TExp[]): TExp =>
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
export const applySub = (sub: Sub, te: TExp): TExp =>
    isEmptySub(sub) ? te :
    isAtomicTExp(te) ? te :
    isTVar(te) ? subGet(sub, te) :
    isProcTExp(te) ? makeProcTExp(map(curry(applySub)(sub), te.paramTEs),
                                  applySub(sub, te.returnTE)) :
    te;

// ============================================================
// Purpose: Returns the composition of substitutions s.t.:
//  applySub(result, te) === applySub(sub2, applySub(sub1, te))
export const combineSub = (sub1: Sub, sub2: Sub): Sub | Error =>
    isEmptySub(sub1) ? sub2 :
    isEmptySub(sub2) ? sub1 :
    combine(sub1, sub2.vars, sub2.tes);

const combine = (sub: Sub | Error, vars: TVar[], tes: TExp[]): Sub | Error =>
    isEmpty(vars) ? sub :
    isError(sub) ? sub :
    combine(extendSub(sub, first(vars), first(tes)),
            rest(vars), rest(tes));

// Purpose: extend a substitution with one pair (tv, te)
// Calls to makeSub to do the occur-check
export const extendSub = (sub: Sub, v: TVar, te: TExp): Sub | Error => {
    const sub2 = makeSub([v], [te]);
    if (isError(sub2)) return sub2;
    if (isEmptySub(sub))
        return sub2;
    const updatedTes = map(curry(applySub)(sub2), sub.tes);
    if (map(prop('var'), sub.vars).includes(v.var))
        return makeSub(sub.vars, updatedTes);
    else
        return makeSub([v].concat(sub.vars), [te].concat(updatedTes));
};
