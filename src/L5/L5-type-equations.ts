// L5-type-equations
import * as R from "ramda";
import * as A from "./L5-ast";
import * as S from "./L5-substitution-adt";
import * as TC from "./L5-typecheck";
import * as T from "./TExp";
import { allDefined, isError, safeF, safeU, safeU2, trust, isDefined } from '../shared/error';
import { isEmpty, first, rest } from "../shared/list";
import { Result, bind as bindResult, makeOk, makeFailure, resultToOptional } from "../shared/result";
import { Optional, bind as bindOptional, maybe, makeSome, makeNone, isSome, mapOptional, safe3, safe2, optionalToResult } from "../shared/optional";

// ============================================================n
// Pool ADT
// A pool represents a map from Exp to TExp
// It is implemented as a list of pairs (Exp TExp).
// When a new Exp is added to a pool, a fresh Tvar
// is allocated for it.
export interface PoolItem {e: A.Exp, te: T.TExp};
export type Pool = PoolItem[];

export const makeEmptyPool = () => [];
export const isEmptyPool = (x: any): boolean => x.length === 0;

// Purpose: construct a pool with one additional pair
//          (exp fresh-tvar)
// @Pre: exp is not already in pool.
export const extendPool = (exp: A.Exp, pool: Pool): Pool =>
    R.prepend({e: exp, te: T.makeFreshTVar()}, pool);

// Purpose: construct a pool with one additional pair
//          ((VarRef var) texp)
//          from a (VarDecl var texp) declaration.
// @Pre: var is not already in pool - which means
// that all bound variables have been renamed with distinct names.
const extendPoolVarDecl = (vd: A.VarDecl, pool: Pool): Pool =>
    R.prepend({e: A.makeVarRef(vd.var), te: vd.texp}, pool);

export const inPool = (pool: Pool, e: A.Exp): Optional<T.TExp> => {
    const exp = R.find(R.propEq('e', e), pool);
    return exp ? makeSome(R.prop('te')(exp)) : makeNone();
}

// Purpose: verify that a set of expressions are found in a pool
//          useful to verify preconditions if allInPool(pool, [e1, e2]) then 
//          inPool(pool ei) will not be undefined and will be a TExp
export const allInPool = (pool: Pool, es: A.Exp[]): boolean =>
    R.all(isSome, R.map((e) => inPool(pool, e), es))

// Map a function over a list of expressions to accumulate
// matching sub-expressions into a pool.
// fun should construct a new pool given a new expression from exp-list
// that has not yet been seen before.
const mapPool = (fun: (e: A.Exp, pool: Pool) => Pool, exps: A.Exp[], result: Pool): Pool =>
    isEmpty(exps) ? result :
    mapPool(fun, rest(exps), maybe(_ => result,
                                   () => fun(first(exps), result),
                                   inPool(result, first(exps))));

const mapPoolVarDecls = (fun: (e: A.VarDecl, pool: Pool) => Pool, vds: A.VarDecl[], result: Pool): Pool =>
    isEmpty(vds) ? result :
    mapPoolVarDecls(fun, rest(vds), maybe(_ => result,
                                          () => fun(first(vds), result),
                                          inPool(result, A.makeVarRef(first(vds).var))));

// Purpose: Traverse the abstract syntax tree L5-exp
//          and collect all sub-expressions into a Pool of fresh type variables.
// Example:
// (ExpToPool parse('(+ x 1)')) =>
// '(((AppExp PrimOp(+) [VarRef(x), NumExp(1)]) TVar(T252722))
//   (NumExp(1) TVar(T252721))
//   (VarRef(x) TVar(T252720))
//   (PrimOp(+) TVar(T252719)))
export const expToPool = (exp: A.Exp): Pool => {
    const findVars = (e: A.Exp, pool: Pool): Pool =>
        A.isAtomicExp(e) ? extendPool(e, pool) :
        A.isProcExp(e) ? extendPool(e, mapPool(findVars, e.body, mapPoolVarDecls(extendPoolVarDecl, e.args, pool))) :
        A.isCompoundExp(e) ? extendPool(e, mapPool(findVars, A.expComponents(e), pool)) :
        makeEmptyPool();
    return findVars(exp, makeEmptyPool());
};

// ========================================================
// Equations ADT
export interface Equation {left: T.TExp, right: T.TExp};
export const makeEquation = (l: T.TExp, r: T.TExp): Equation => ({left: l, right: r});

// Safe makeEquation: make equation that absorbs undefined params into undefined
export const safeMakeEquation = safeU2(makeEquation);
export const safeEquations = (eqs: (Equation | undefined)[]): Equation[] => 
    allDefined(eqs) ? eqs : [];
export const safeList = <T1>(l: (T1 | undefined)[]): T1[] | undefined =>
    allDefined(l) ? l : undefined;
export const safeLast = <T extends any>(list: readonly T[]): Optional<T> => {
    const last = R.last(list);
    return last ? makeSome(last) : makeNone();
}

// Constructor for equations for a Scheme expression:
// this constructor implements the second step of the type-inference-equations
// algorithm -- derive equations for all composite sub expressions of a
// given L5 expression. Its input is a pool of pairs (L5-exp Tvar).
// A Scheme expression is mapped to a pool with L5-exp->pool

// Signature: poolToEquations(pool)
// Purpose: Return a set of equations for a given Exp encoded as a pool
// Type: [Pool -> List(Equation)]
// @Pre: pool is the result of expTopool(exp)
export const poolToEquations = (pool: Pool): Optional<Equation[]> => {
    // VarRef generate no equations beyond that of var-decl - remove them.
    const poolWithoutVars: Pool = R.filter(R.propSatisfies(R.complement(A.isVarRef), 'e'), pool);
    return bindOptional(mapOptional((e: A.Exp) => makeEquationFromExp(e, pool), R.pluck('e', poolWithoutVars)),
                        (eqns: Equation[][]) => makeSome(R.chain(R.identity, eqns)));
};

// Signature: make-equation-from-exp(exp, pool)
// Purpose: Return a single equation
// @Pre: exp is a member of pool
export const makeEquationFromExp = (exp: A.Exp, pool: Pool): Optional<Equation[]> =>
    // An application must respect the type of its operator
    // Type(Operator) = [T1 * .. * Tn -> Te]
    // Type(Application) = Te
    A.isAppExp(exp) ? safe3((rator: T.TExp, rands: T.TExp[], e: T.TExp) => makeSome([makeEquation(rator, T.makeProcTExp(rands, e))]))
                        (inPool(pool, exp.rator), mapOptional((e) => inPool(pool, e), exp.rands), inPool(pool, exp)) :
    // The type of procedure is (T1 * ... * Tn -> Te)
    // where Te is the type of the last exp in the body of the proc.
    // and   Ti is the type of each of the parameters.
    // No need to traverse the other body expressions - they will be
    // traversed by the overall loop of pool->equations
    A.isProcExp(exp) ? safe2((left: T.TExp, ret: T.TExp) => makeSome([makeEquation(left, T.makeProcTExp(R.map((vd) => vd. texp, exp.args), ret))]))
                        (inPool(pool, exp), bindOptional(safeLast(exp.body), (last: A.CExp) => inPool(pool, last))) :
    // The type of a number is Number
    A.isNumExp(exp) ? bindOptional(inPool(pool, exp), (left: T.TExp) => makeSome([makeEquation(left, T.makeNumTExp())])) :
    // The type of a boolean is Boolean
    A.isBoolExp(exp) ? bindOptional(inPool(pool, exp), (left: T.TExp) => makeSome([makeEquation(left, T.makeBoolTExp())])) :
    // The type of a primitive procedure is given by the primitive.
    A.isPrimOp(exp) ? safe2((left: T.TExp, right: T.TExp) => makeSome([makeEquation(left, right)]))
                        (inPool(pool, exp), resultToOptional(TC.typeofPrim(exp))) :
    makeSome([]); // Error(`makeEquationFromExp: Unsupported exp ${exp}`)


// ========================================================
// Signature: inferType(exp)
// Purpose: Infer the type of an expression using the equations method
// Example: unparseTExp(inferType(parse('(lambda (f x) (f (f x)))')))
//          ==> '((T_1 -> T_1) * T_1 -> T_1)'
export const inferType = (exp: A.Exp): Optional<T.TExp> => {
    // console.log(`Infer ${A.unparse(exp)}`)
    const pool = expToPool(exp);
    // console.log(`Pool ${JSON.stringify(pool)}`);
    const equations = poolToEquations(pool);
    // console.log(`Equations ${JSON.stringify(equations)}`);
    const sub = bindOptional(equations, (eqns: Equation[]) => resultToOptional(solveEquations(eqns)))
    // console.log(`Sub ${JSON.stringify(sub)}`);
    const texp = inPool(pool, exp);
    // console.log(`TExp = ${JSON.stringify(bindResult(optionalToResult(texp, "TExp is None"), T.unparseTExp))}`);
    return safe2((sub: S.Sub, texp: T.TExp) => makeSome(T.isTVar(texp) ? S.subGet(sub, texp) : texp))(sub, texp);
};

// Type: [Concrete-Exp -> Concrete-TExp]
export const infer = (exp: string): Result<string> =>
    bindResult(A.parseL5Exp(exp),
               (exp: A.Exp) => maybe((te: T.TExp) => T.unparseTExp(te),
                                     () => makeFailure("Infer type failed"),
                                     inferType(exp)));

// ========================================================
// type equation solving

// Signature: solveEquations(equations)
// Purpose: Solve the type equations and return the resulting substitution
//          or error, if not solvable
// Type: [List(Equation) -> Sub | Error]
// Example: solveEquations(
//            poolToEquations(
//              expToPool(
//                parse('((lambda (x) (x 11)) (lambda (y) y))')))) => sub
export const solveEquations = (equations: Equation[]): Result<S.Sub> =>
    solve(equations, S.makeEmptySub());

// Signature: solve(equations, substitution)
// Purpose: Solve the equations, starting from a given substitution.
//          Returns the resulting substitution, or error, if not solvable
const solve = (equations: Equation[], sub: S.Sub): Result<S.Sub> => {
    const solveVarEq = (tvar: T.TVar, texp: T.TExp): Result<S.Sub> =>
        bindResult(S.extendSub(sub, tvar, texp), sub2 => solve(rest(equations), sub2));

    const bothSidesAtomic = (eq: Equation): boolean =>
        T.isAtomicTExp(eq.left) && T.isAtomicTExp(eq.right);

    const handleBothSidesAtomic = (eq: Equation): Result<S.Sub> =>
        T.isAtomicTExp(eq.left) && T.isAtomicTExp(eq.right) && T.eqAtomicTExp(eq.left, eq.right)
        ? solve(rest(equations), sub)
        : makeFailure(`Equation with non-equal atomic type ${eq}`);

    if (isEmpty(equations)) {
        return makeOk(sub);
    }

    const eq = makeEquation(S.applySub(sub, first(equations).left),
                            S.applySub(sub, first(equations).right));

    return T.isTVar(eq.left) ? solveVarEq(eq.left, eq.right) :
           T.isTVar(eq.right) ? solveVarEq(eq.right, eq.left) :
           bothSidesAtomic(eq) ? handleBothSidesAtomic(eq) :
           T.isCompoundTExp(eq.left) && T.isCompoundTExp(eq.right) && canUnify(eq) ?
                solve(R.concat(rest(equations), splitEquation(eq)), sub) :
           makeFailure(`Equation contains incompatible types ${eq}`);
};

// Signature: canUnify(equation)
// Purpose: Compare the structure of the type expressions of the equation
const canUnify = (eq: Equation): boolean =>
    T.isProcTExp(eq.left) && T.isProcTExp(eq.right) &&
    (eq.left.paramTEs.length === eq.right.paramTEs.length);

// Signature: splitEquation(equation)
// Purpose: For an equation with unifyable type expressions,
//          create equations for corresponding components.
// Type: [Equation -> List(Equation)]
// Example: splitEquation(
//            makeEquation(parseTExp('(T1 -> T2)'),
//                         parseTExp('(T3 -> (T4 -> T4))')) =>
//            [ {left:T2, right: (T4 -> T4)},
//              {left:T3, right: T1)} ]
// @Pre: isCompoundExp(eq.left) && isCompoundExp(eq.right) && canUnify(eq)
const splitEquation = (eq: Equation): Equation[] =>
    (T.isProcTExp(eq.left) && T.isProcTExp(eq.right)) ?
        R.zipWith(makeEquation,
                  R.prepend(eq.left.returnTE, eq.left.paramTEs),
                  R.prepend(eq.right.returnTE, eq.right.paramTEs)) :
    [];
