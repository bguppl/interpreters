// L5-type-equations
import * as R from "ramda";
import * as A from "./L5-ast";
import * as S from "./L5-substitution-adt";
import * as TC from "./L5-typecheck";
import * as T from "./TExp";
import * as Res from "../shared/result";
import * as Opt from "../shared/optional";
import { isEmpty, first, rest, cons, isNonEmptyList } from "../shared/list";
import { parse as p } from "../shared/parser";
import { format } from "../shared/format";

// ============================================================n
// Pool ADT
// A pool represents a map from Exp to TExp
// It is implemented as a list of pairs (Exp TExp).
// When a new Exp is added to a pool, a fresh Tvar
// is allocated for it.
export type PoolItem = {e: A.Exp, te: T.TExp}
export type Pool = PoolItem[];

export const makeEmptyPool = () => [];

// Purpose: construct a pool with one additional pair
//          [exp, fresh-tvar]
// @Pre: exp is not already in pool.
export const extendPool = (exp: A.Exp, pool: Pool): Pool =>
    cons({e: exp, te: T.makeFreshTVar()}, pool);

// Purpose: construct a pool with one additional pair
//          [VarRef(var), texp]
//          from a VarDecl(var, texp) declaration.
// @Pre: var is not already in pool - which means
// that all bound variables have been renamed with distinct names.
const extendPoolVarDecl = (vd: A.VarDecl, pool: Pool): Pool =>
    cons({e: A.makeVarRef(vd.var), te: vd.texp}, pool);

export const inPool = (pool: Pool, e: A.Exp): Opt.Optional<T.TExp> => {
    const exp = R.find(R.propEq('e', e), pool);
    return exp ? Opt.makeSome(R.prop('te')(exp)) : Opt.makeNone();
}

// Map a function over a list of expressions to accumulate
// matching sub-expressions into a pool.
// fun should construct a new pool given a new expression from exp-list
// that has not yet been seen before.
const reducePool = (fun: (e: A.Exp, pool: Pool) => Pool, exps: A.Exp[], result: Pool): Pool =>
    isNonEmptyList<A.Exp>(exps) ?
        Opt.maybe(inPool(result, first(exps)),
                  _ => reducePool(fun, rest(exps), result),
                  () => reducePool(fun, rest(exps), fun(first(exps), result))) :
    result;

const reducePoolVarDecls = (fun: (e: A.VarDecl, pool: Pool) => Pool, vds: A.VarDecl[], result: Pool): Pool =>
    isNonEmptyList<A.VarDecl>(vds) ?
        Opt.maybe(inPool(result, A.makeVarRef(first(vds).var)),
                  _ => reducePoolVarDecls(fun, rest(vds), result),
                  () => reducePoolVarDecls(fun, rest(vds), fun(first(vds), result))) :
    result;

// Purpose: Traverse the abstract syntax tree L5-exp
//          and collect all sub-expressions into a Pool of fresh type variables.
// Example:
// bind(p('(+ x 1)'), (s) => mapv(parseL5Exp(s), (e) => TE.expToPool(e))) =>
// Ok([[AppExp(PrimOp(+), [VarRef(x), NumExp(1)]), TVar(16)],
//     [NumExp(1), TVar(15)],
//     [VarRef(x), TVar(14)],
//     [PrimOp(+), TVar(13)]])
export const expToPool = (exp: A.Exp): Pool => {
    const findVars = (e: A.Exp, pool: Pool): Pool =>
        A.isAtomicExp(e) ? extendPool(e, pool) :
        A.isProcExp(e) ? extendPool(e, reducePool(findVars, e.body, reducePoolVarDecls(extendPoolVarDecl, e.args, pool))) :
        A.isCompoundExp(e) ? extendPool(e, reducePool(findVars, A.expComponents(e), pool)) :
        makeEmptyPool();
    return findVars(exp, makeEmptyPool());
};

// ========================================================
// Equations ADT
export type Equation = {left: T.TExp, right: T.TExp}
export const makeEquation = (l: T.TExp, r: T.TExp): Equation => ({left: l, right: r});

export const safeLast = <T extends any>(list: readonly T[]): Opt.Optional<T> => {
    const last = R.last(list);
    return last ? Opt.makeSome(last) : Opt.makeNone();
}

// Purpose: flatten a list of lists into a flat list of items
// Example:
// flatten([[1,2], [], [3]]) => [1,2,3]
// flatten([]) => []
// flatten([[]]) => []
export const flatten = <T>(listOfLists: readonly T[][]): T[] => R.chain(R.identity, listOfLists);

// Constructor for equations for a Scheme expression:
// this constructor implements the second step of the type-inference-equations
// algorithm -- derive equations for all composite sub expressions of a
// given L5 expression. Its input is a pool of pairs (L5-exp Tvar).
// A Scheme expression is mapped to a pool with L5-exp->pool

// Purpose: Return a set of equations for a given Exp encoded as a pool
// @Pre: pool is the result of expToPool(exp)
export const poolToEquations = (pool: Pool): Opt.Optional<Equation[]> => {
    // VarRef generate no equations beyond that of var-decl - remove them.
    const poolWithoutVars = R.filter(R.propSatisfies(R.complement(A.isVarRef), 'e'), pool);
    return Opt.mapv(Opt.mapOptional((e: A.Exp) => makeEquationsFromExp(e, pool), R.map(R.prop('e'), poolWithoutVars)), 
                    (eqns: Equation[][]) => flatten(eqns));
};

// Signature: make-equation-from-exp(exp, pool)
// Purpose: Return a single equation
// @Pre: exp is a member of pool
export const makeEquationsFromExp = (exp: A.Exp, pool: Pool): Opt.Optional<Equation[]> =>
    // An application must respect the type of its operator
    // Type(Operator) = [T1 * .. * Tn -> Te]
    // Type(Application) = Te
    A.isAppExp(exp) ? Opt.bind(inPool(pool, exp.rator), (rator: T.TExp) =>
                        Opt.bind(Opt.mapOptional((e) => inPool(pool, e), exp.rands), (rands: T.TExp[]) =>
                            Opt.mapv(inPool(pool, exp), (e: T.TExp) => 
                                [makeEquation(rator, T.makeProcTExp(rands, e))]))) :
    // The type of procedure is (T1 * ... * Tn -> Te)
    // where Te is the type of the last exp in the body of the proc.
    // and   Ti is the type of each of the parameters.
    // No need to traverse the other body expressions - they will be
    // traversed by the overall loop of pool->equations
    A.isProcExp(exp) ? Opt.bind(inPool(pool, exp), (left: T.TExp) =>
                            Opt.mapv(Opt.bind(safeLast(exp.body), (last: A.CExp) => inPool(pool, last)), (ret: T.TExp) =>
                                [makeEquation(left, T.makeProcTExp(R.map((vd) => vd. texp, exp.args), ret))])) :
    // The type of a number is Number
    A.isNumExp(exp) ? Opt.mapv(inPool(pool, exp), (left: T.TExp) => [makeEquation(left, T.makeNumTExp())]) :
    // The type of a boolean is Boolean
    A.isBoolExp(exp) ? Opt.mapv(inPool(pool, exp), (left: T.TExp) => [makeEquation(left, T.makeBoolTExp())]) :
    // The type of a string is String
    A.isStrExp(exp) ? Opt.mapv(inPool(pool, exp), (left: T.TExp) => [makeEquation(left, T.makeStrTExp())]) :
    // The type of a primitive procedure is given by the primitive.
    A.isPrimOp(exp) ? Opt.bind(inPool(pool, exp), (left: T.TExp) =>
                            Opt.mapv(Res.resultToOptional(TC.typeofPrim(exp)), (right: T.TExp) =>
                                [makeEquation(left, right)])) :
    // Todo: define, let, letrec, set 
    Opt.makeNone();


// ========================================================
// Signature: inferType(exp)
// Purpose: Infer the type of an expression using the equations method
// Example: unparseTExp(inferType(parse('(lambda (f x) (f (f x)))')))
//          ==> '((T_1 -> T_1) * T_1 -> T_1)'
export const inferType = (exp: A.Exp): Opt.Optional<T.TExp> => {
    // console.log(`Infer ${A.unparse(exp)}`)
    const pool = expToPool(exp);
    // console.log(`Pool ${format(pool)}`);
    const equations = poolToEquations(pool);
    // console.log(`Equations ${format(equations)}`);
    const sub = Opt.bind(equations, (eqns: Equation[]) => Res.resultToOptional(solveEquations(eqns)));
    // console.log(`Sub ${format(sub)}`);
    // Extract the computed type of the root expression from the pool
    const texp = inPool(pool, exp);
    // console.log(`TExp = ${format(bindResult(optionalToResult(texp, "TExp is None"), T.unparseTExp))}`);
    // Replace all TVars in the computed type by their type expression
    return Opt.bind(sub, (sub: S.Sub) =>
                Opt.mapv(texp, (texp: T.TExp) =>
                    T.isTVar(texp) ? S.subGet(sub, texp) : texp))
};

// Type: [Concrete-Exp -> Concrete-TExp]
// End to end processing: parse, infer type, unparse.
export const infer = (exp: string): Res.Result<string> =>
    Res.bind(p(exp), (x) =>
        Res.bind(A.parseL5Exp(x), (exp: A.Exp) => 
            Opt.maybe(inferType(exp),
                (te: T.TExp) => T.unparseTExp(te),
                () => Res.makeFailure("Infer type failed"))));

// ========================================================
// type equation solving

// Purpose: Solve the type equations and return the resulting substitution
//          or error, if not solvable
// Type: [List(Equation) -> Sub | Error]
// Example: solveEquations(
//            poolToEquations(
//              expToPool(
//                parse('((lambda (x) (x 11)) (lambda (y) y))')))) => sub
export const solveEquations = (equations: Equation[]): Res.Result<S.Sub> =>
    solve(equations, S.makeEmptySub());

// Purpose: Solve the equations, starting from a given substitution.
//          Returns the resulting substitution, or error, if not solvable
const solve = (equations: Equation[], sub: S.Sub): Res.Result<S.Sub> => {
    
    if (!isNonEmptyList<Equation>(equations)) {
        return Res.makeOk(sub);
    }

    const solveVarEq = (tvar: T.TVar, texp: T.TExp): Res.Result<S.Sub> =>
        Res.bind(S.extendSub(sub, tvar, texp), sub2 => solve(rest(equations), sub2));

    const bothSidesAtomic = (eq: Equation): boolean =>
        T.isAtomicTExp(eq.left) && T.isAtomicTExp(eq.right);

    const handleBothSidesAtomic = (eq: Equation): Res.Result<S.Sub> =>
        T.isAtomicTExp(eq.left) && T.isAtomicTExp(eq.right) && T.eqAtomicTExp(eq.left, eq.right)
        ? solve(rest(equations), sub)
        : Res.makeFailure(`Equation with non-equal atomic type ${format(eq)}`);

    const eq = makeEquation(S.applySub(sub, first(equations).left),
                            S.applySub(sub, first(equations).right));

    return T.isTVar(eq.left) ? solveVarEq(eq.left, eq.right) :
           T.isTVar(eq.right) ? solveVarEq(eq.right, eq.left) :
           bothSidesAtomic(eq) ? handleBothSidesAtomic(eq) :
           T.isCompoundTExp(eq.left) && T.isCompoundTExp(eq.right) && canUnify(eq) ?
                solve(R.concat(rest(equations), splitEquation(eq)), sub) :
           Res.makeFailure(`Equation contains incompatible types ${format(eq)}`);
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
                  cons(eq.left.returnTE, eq.left.paramTEs),
                  cons(eq.right.returnTE, eq.right.paramTEs)) :
    [];
