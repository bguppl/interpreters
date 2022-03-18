// L5-type-equations
import _ from "lodash";
import * as E from "fp-ts/Either";
import * as O from "fp-ts/Option";
import * as RA from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import * as A from "./L5-ast";
import * as S from "./L5-substitution-adt";
import * as TC from "./L5-typecheck";
import * as T from "./TExp";
import { isEmpty, first, rest, cons } from "../shared/list";
import { parse as p } from "../shared/parser";

// ============================================================n
// Pool ADT
// A pool represents a map from Exp to TExp
// It is implemented as a list of pairs (Exp TExp).
// When a new Exp is added to a pool, a fresh Tvar
// is allocated for it.
export interface PoolItem {
    e: A.Exp;
    te: T.TExp;
}
export type Pool = readonly PoolItem[];

export const makeEmptyPool = () => [];

// Purpose: construct a pool with one additional pair
//          [exp, fresh-tvar]
// @Pre: exp is not already in pool.
export const extendPool = (exp: A.Exp, pool: Pool): Pool =>
    cons({ e: exp, te: T.makeFreshTVar() }, pool);

// Purpose: construct a pool with one additional pair
//          [VarRef(var), texp]
//          from a VarDecl(var, texp) declaration.
// @Pre: var is not already in pool - which means
// that all bound variables have been renamed with distinct names.
const extendPoolVarDecl = (vd: A.VarDecl, pool: Pool): Pool =>
    cons({ e: A.makeVarRef(vd.var), te: vd.texp }, pool);

export const inPool = (pool: Pool, e: A.Exp): O.Option<T.TExp> =>
    pipe(
        pool,
        RA.findFirst(item => _.isEqual(item.e, e)),
        O.map(item => item.te)
    );

// Map a function over a list of expressions to accumulate
// matching sub-expressions into a pool.
// fun should construct a new pool given a new expression from exp-list
// that has not yet been seen before.
const reducePool = (fun: (e: A.Exp, pool: Pool) => Pool, exps: readonly A.Exp[], result: Pool): Pool =>
    isEmpty(exps) ? result :
    pipe(
        inPool(result, first(exps)),
        O.match(
            () => reducePool(fun, rest(exps), fun(first(exps), result)),
            _ => reducePool(fun, rest(exps), result)
        )
    );

const reducePoolVarDecls = (fun: (e: A.VarDecl, pool: Pool) => Pool, vds: readonly A.VarDecl[], result: Pool): Pool =>
    isEmpty(vds) ? result :
    pipe(
        inPool(result, A.makeVarRef(first(vds).var)),
        O.match(
            () => reducePoolVarDecls(fun, rest(vds), fun(first(vds), result)),
            _ => reducePoolVarDecls(fun, rest(vds), result)
        )
    );

// Purpose: Traverse the abstract syntax tree L5-exp
//          and collect all sub-expressions into a Pool of fresh type variables.
// Example:
// bind(bind(p('(+ x 1)'), parseL5Exp), e => makeOk(TE.expToPool(e))) =>
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
export interface Equation { left: T.TExp, right: T.TExp }
export const makeEquation = (l: T.TExp, r: T.TExp): Equation => ({ left: l, right: r });

// Constructor for equations for a Scheme expression:
// this constructor implements the second step of the type-inference-equations
// algorithm -- derive equations for all composite sub expressions of a
// given L5 expression. Its input is a pool of pairs (L5-exp Tvar).
// A Scheme expression is mapped to a pool with L5-exp->pool

// Purpose: Return a set of equations for a given Exp encoded as a pool
// @Pre: pool is the result of expToPool(exp)
export const poolToEquations = (pool: Pool): O.Option<readonly Equation[]> => {
    // VarRef generate no equations beyond that of var-decl - remove them.
    const poolWithoutVars = pipe(pool, RA.filter(item => !A.isVarRef(item.e)));
    return pipe(
        poolWithoutVars,
        RA.map(item => item.e),
        O.traverseArray(e => makeEquationsFromExp(e, pool)),
        O.map(RA.flatten)
    );
};

// Signature: make-equation-from-exp(exp, pool)
// Purpose: Return a single equation
// @Pre: exp is a member of pool
export const makeEquationsFromExp = (exp: A.Exp, pool: Pool): O.Option<Equation[]> =>
    // An application must respect the type of its operator
    // Type(Operator) = [T1 * .. * Tn -> Te]
    // Type(Application) = Te
    A.isAppExp(exp) ? pipe(
        inPool(pool, exp.rator),
        O.chain(rator => pipe(
            exp.rands,
            O.traverseArray(e => inPool(pool, e)),
            O.chain(rands => pipe(
                inPool(pool, exp),
                O.map(e => [makeEquation(rator, T.makeProcTExp(rands, e))])
            ))
        ))
    ) :
    // The type of procedure is (T1 * ... * Tn -> Te)
    // where Te is the type of the last exp in the body of the proc.
    // and   Ti is the type of each of the parameters.
    // No need to traverse the other body expressions - they will be
    // traversed by the overall loop of pool->equations
    A.isProcExp(exp) ? pipe(
        inPool(pool, exp),
        O.chain(left => pipe(
            exp.body,
            RA.last,
            O.chain(last => inPool(pool, last)),
            O.map(ret => [makeEquation(left, T.makeProcTExp(pipe(exp.args, RA.map(vd => vd.texp)), ret))])
        ))
    ) :
    // The type of a number is Number
    A.isNumExp(exp) ? pipe(inPool(pool, exp), O.map(left => [makeEquation(left, T.makeNumTExp())])) :
    // The type of a boolean is Boolean
    A.isBoolExp(exp) ? pipe(inPool(pool, exp), O.map(left => [makeEquation(left, T.makeBoolTExp())])) :
    // The type of a string is String
    A.isStrExp(exp) ? pipe(inPool(pool, exp), O.map(left => [makeEquation(left, T.makeStrTExp())])) :
    // The type of a primitive procedure is given by the primitive.
    A.isPrimOp(exp) ? pipe(
        inPool(pool, exp),
        O.chain(left => pipe(
            TC.typeofPrim(exp),
            O.fromEither,
            O.map(right => [makeEquation(left, right)])
        ))
    ) :
    // Todo: define, let, letrec, set 
    O.none;


// ========================================================
// Signature: inferType(exp)
// Purpose: Infer the type of an expression using the equations method
// Example: unparseTExp(inferType(parse('(lambda (f x) (f (f x)))')))
//          ==> '((T_1 -> T_1) * T_1 -> T_1)'
export const inferType = (exp: A.Exp): O.Option<T.TExp> => {
    // console.log(`Infer ${A.unparse(exp)}`)
    const pool = expToPool(exp);
    // console.log(`Pool ${JSON.stringify(pool)}`);
    const equations = poolToEquations(pool);
    // console.log(`Equations ${JSON.stringify(equations)}`);
    const sub = pipe(equations, O.chain(eqns => pipe(eqns, solveEquations, O.fromEither)));
    // console.log(`Sub ${JSON.stringify(sub)}`);
    // Extract the computed type of the root expression from the pool
    const texp = inPool(pool, exp);
    // console.log(`TExp = ${JSON.stringify(bindResult(optionalToResult(texp, "TExp is None"), T.unparseTExp))}`);
    // Replace all TVars in the computed type by their type expression
    return pipe(
        sub,
        O.chain(sub => pipe(
            texp,
            O.map(texp => T.isTVar(texp) ? S.subGet(sub, texp) : texp)
        ))
    );
};

// Type: [Concrete-Exp -> Concrete-TExp]
// End to end processing: parse, infer type, unparse.
export const infer = (exp: string): E.Either<string, string> =>
    pipe(
        p(exp),
        E.chain(A.parseL5Exp),
        E.chain(exp => pipe(
            inferType(exp),
            O.match(
                () => E.left("Infer type failed"),
                T.unparseTExp
            )
        ))
    );

// ========================================================
// type equation solving

// Purpose: Solve the type equations and return the resulting substitution
//          or error, if not solvable
// Type: [List(Equation) -> Sub | Error]
// Example: solveEquations(
//            poolToEquations(
//              expToPool(
//                parse('((lambda (x) (x 11)) (lambda (y) y))')))) => sub
export const solveEquations = (equations: readonly Equation[]): E.Either<string, S.Sub> =>
    solve(equations, S.makeEmptySub());

// Purpose: Solve the equations, starting from a given substitution.
//          Returns the resulting substitution, or error, if not solvable
const solve = (equations: readonly Equation[], sub: S.Sub): E.Either<string, S.Sub> => {
    const solveVarEq = (tvar: T.TVar, texp: T.TExp): E.Either<string, S.Sub> =>
        pipe(S.extendSub(sub, tvar, texp), E.chain(sub2 => solve(rest(equations), sub2)));

    const bothSidesAtomic = (eq: Equation): boolean =>
        T.isAtomicTExp(eq.left) && T.isAtomicTExp(eq.right);

    const handleBothSidesAtomic = (eq: Equation): E.Either<string, S.Sub> =>
        T.isAtomicTExp(eq.left) && T.isAtomicTExp(eq.right) && T.eqAtomicTExp(eq.left, eq.right)
        ? solve(rest(equations), sub)
        : E.left(`Equation with non-equal atomic type ${eq}`);

    if (isEmpty(equations)) {
        return E.of(sub);
    }

    const eq = makeEquation(S.applySub(sub)(first(equations).left),
                            S.applySub(sub)(first(equations).right));

    return T.isTVar(eq.left) ? solveVarEq(eq.left, eq.right) :
           T.isTVar(eq.right) ? solveVarEq(eq.right, eq.left) :
           bothSidesAtomic(eq) ? handleBothSidesAtomic(eq) :
           T.isCompoundTExp(eq.left) && T.isCompoundTExp(eq.right) && canUnify(eq) ?
                solve(RA.concat(splitEquation(eq))(rest(equations)), sub) :
           E.left(`Equation contains incompatible types ${eq}`);
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
const splitEquation = (eq: Equation): readonly Equation[] =>
    (T.isProcTExp(eq.left) && T.isProcTExp(eq.right)) ?
        RA.zipWith(cons(eq.left.returnTE, eq.left.paramTEs),
                   cons(eq.right.returnTE, eq.right.paramTEs),
                   makeEquation) :
    [];
