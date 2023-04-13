import { map, prop, includes } from 'ramda';
import { eqTVar, isAtomicTExp, isProcTExp, isTVar, makeProcTExp, unparseTExp, TExp, TVar } from "./TExp";
import { cons, isEmpty, first, rest, isNonEmptyList } from "../shared/list";
import { Result, makeOk, makeFailure, mapResult, bind, zipWithResult, mapv } from '../shared/result';
import { format } from '../shared/format';

// Implementation of the Substitution ADT
// ========================================================
// A substitution is represented as a 2 element list of equal length
// lists of variables and type expression.
// The empty substitution is [[], []]

export type Sub = {tag: "Sub"; vars: TVar[]; tes: TExp[]; }
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
export const makeSub = (vars: TVar[], tes: TExp[]): Result<Sub> =>
    mapv(zipWithResult(checkNoOccurrence, vars, tes), _ => ({ tag: "Sub", vars: vars, tes: tes }));

export const makeEmptySub = (): Sub => ({tag: "Sub", vars: [], tes: []});

// Purpose: when attempting to bind tvar to te in a sub - check whether tvar occurs in te.
// Return error if a circular reference is found.
export const checkNoOccurrence = (tvar: TVar, te: TExp): Result<true> => {
    const check = (e: TExp): Result<true> =>
        isTVar(e) ? ((e.var === tvar.var) ? bind(unparseTExp(te), up => makeFailure(`Occur check error - circular sub ${tvar.var} in ${format(up)}`)) : 
                                            makeOk(true)) :
        isAtomicTExp(e) ? makeOk(true) :
        isProcTExp(e) ? bind(mapResult(check, e.paramTEs), _ => check(e.returnTE)) :
        makeFailure(`Bad type expression ${e} in ${format(te)}`);
    return check(te);
};

export const isEmptySub = (sub: any): boolean => isSub(sub) && isEmpty(sub.vars) && isEmpty(sub.tes);

// Purpose: If v is in sub.vars - return corresponding te, else v unchanged.
export const subGet = (sub: Sub, v: TVar): TExp => {
    const lookup = (vars: TVar[], tes: TExp[]): TExp =>
        isNonEmptyList<TVar>(vars) && isNonEmptyList<TExp>(tes) ? 
            eqTVar(first(vars), v) ? first(tes) :
            lookup(rest(vars), rest(tes)) :
        v;

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
    isProcTExp(te) ? makeProcTExp(map((te) => applySub(sub, te), te.paramTEs), applySub(sub, te.returnTE)) :
    te;

// ============================================================
// Purpose: Returns the composition of substitutions s.t.:
//  applySub(result, te) === applySub(sub2, applySub(sub1, te))
export const combineSub = (sub1: Sub, sub2: Sub): Result<Sub> =>
    isEmptySub(sub1) ? makeOk(sub2) :
    isEmptySub(sub2) ? makeOk(sub1) :
    combine(sub1, sub2.vars, sub2.tes);

const combine = (sub: Sub, vars: TVar[], tes: TExp[]): Result<Sub> =>
    isNonEmptyList<TVar>(vars) && isNonEmptyList<TExp>(tes) ? 
        bind(extendSub(sub, first(vars), first(tes)), 
             (extSub: Sub) => combine(extSub, rest(vars), rest(tes))) :
    makeOk(sub);


// Purpose: extend a substitution with one pair (tv, te)
// Calls to makeSub to do the occur-check
export const extendSub = (sub: Sub, v: TVar, te: TExp): Result<Sub> =>
    bind(makeSub([v], [te]), (sub2: Sub) => {
        const updatedTEs = map((te) => applySub(sub2, te), sub.tes);
        return includes(v.var, map(prop('var'), sub.vars)) ? makeSub(sub.vars, updatedTEs) :
               makeSub(cons(v, sub.vars), cons(te, updatedTEs));
    });
