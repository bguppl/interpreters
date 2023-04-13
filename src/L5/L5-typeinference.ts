// L5-typeinference

import * as R from "ramda";
import * as A from "./L5-ast";
import * as TC from "./L5-typecheck";
import * as E from "./TEnv";
import * as T from "./TExp";
import { allT, first, rest, isEmpty, isNonEmptyList } from "../shared/list";
import { Result, makeFailure, makeOk, bind, zipWithResult, mapResult, mapv } from "../shared/result";
import { parse as p } from "../shared/parser";
import { format } from "../shared/format";

// Purpose: Make type expressions equivalent by deriving a unifier
// Return an error if the types are not unifiable.
// Exp is only passed for documentation purposes.
// te1 can be undefined when it is retrieved from a type variable which is not yet bound.
const checkEqualType = (te1: T.TExp | undefined, te2: T.TExp, exp: A.Exp): Result<true> =>
    te1 === undefined ? bind(T.unparseTExp(te2), (texp: string) => makeFailure(`Incompatible types: undefined - ${format(texp)}`)) :
    T.isTVar(te1) && T.isTVar(te2) ? ((T.eqTVar(te1, te2) ? makeOk(true) : checkTVarEqualTypes(te1, te2, exp))) :
    T.isTVar(te1) ? checkTVarEqualTypes(te1, te2, exp) :
    T.isTVar(te2) ? checkTVarEqualTypes(te2, te1, exp) :
    T.isAtomicTExp(te1) && T.isAtomicTExp(te2) ?
        T.eqAtomicTExp(te1, te2) ? makeOk(true) : bind(T.unparseTExp(te1), (te1: string) =>
                                                    bind(T.unparseTExp(te2), (te2: string) =>
                                                        makeFailure<true>(`Incompatible atomic types ${te1} - ${te2}`))) :
    T.isProcTExp(te1) && T.isProcTExp(te2) ? checkProcEqualTypes(te1, te2, exp) :
    bind(T.unparseTExp(te1), (te1: string) =>
        bind(T.unparseTExp(te2), (te2: string) =>
            makeFailure<true>(`Incompatible types structure: ${te1} - ${te2}`)));

// Purpose: make two lists of equal length of type expressions equal
// Return an error if one of the pair of TExps are not compatible - true otherwise.
// Exp is only passed for documentation purposes.
const checkEqualTypes = (tes1: T.TExp[], tes2: T.TExp[], exp: A.Exp): Result<true> => {
    const checks = zipWithResult((te1, te2) => checkEqualType(te1, te2, exp), tes1, tes2);
    return bind(checks, _ => makeOk(true));
}

const checkProcEqualTypes = (te1: T.ProcTExp, te2: T.ProcTExp, exp: A.Exp): Result<true> =>
    te1.paramTEs.length !== te2.paramTEs.length ? bind(T.unparseTExp(te1), (te1: string) =>
                                                    bind(T.unparseTExp(te2), (te2: string) =>
                                                        makeFailure<true>(`Wrong number of args ${te1} - ${te2}`))) :
    checkEqualTypes(T.procTExpComponents(te1), T.procTExpComponents(te2), exp);

// Purpose: check that a type variable matches a type expression
// Updates the var is needed to refer to te.
// Exp is only passed for documentation purposes.
const checkTVarEqualTypes = (tvar: T.TVar, te: T.TExp, exp: A.Exp): Result<true> =>
    T.tvarIsNonEmpty(tvar) ? checkEqualType(T.tvarContents(tvar), te, exp) :
    mapv(checkNoOccurrence(tvar, te, exp), _ => { T.tvarSetContents(tvar, te); return true; });

// Purpose: when attempting to bind tvar to te - check whether tvar occurs in te.
// Throws error if a circular reference is found.
// Exp is only passed for documentation purposes.
// Pre-conditions: Tvar is not bound
const checkNoOccurrence = (tvar: T.TVar, te: T.TExp, exp: A.Exp): Result<true> => {
    const checkList = (tes: T.TExp[]): Result<true> =>
        mapv(mapResult(loop, tes), _ => true);

    const loop = (te1: T.TExp): Result<true> =>
        T.isAtomicTExp(te1) ? makeOk(true) :
        T.isProcTExp(te1) ? checkList(T.procTExpComponents(te1)) :
        T.isTVar(te1) ? 
            (T.eqTVar(te1, tvar) ? bind(A.unparse(exp), (exp: string) => makeFailure(`Occur check error - ${te1.var} - ${tvar.var} in ${format(exp)}`)) : 
             makeOk(true)) :
        bind(A.unparse(exp), (exp: string) => makeFailure(`Bad type expression - ${format(te1)} in ${format(exp)}`));

    return loop(te);
}

// Compute the type of Typed-AST exps to TE
// ========================================
// Compute a Typed-AST exp to a Texp on the basis of its structure and the annotations it contains.

// Purpose: Compute the type of a concrete fully-typed expression
export const inferTypeOf = (concreteExp: string): Result<string> =>
    bind(p(concreteExp), (x) =>
        bind(A.parseL5Exp(x), (exp: A.Exp) =>
            bind(typeofExp(exp, E.makeEmptyTEnv()), T.unparseTExp)));

// Purpose: Compute the type of an expression
// Traverse the AST and check the type according to the exp type.
export const typeofExp = (exp: A.Parsed, tenv: E.TEnv): Result<T.TExp> =>
    A.isNumExp(exp) ? makeOk(T.makeNumTExp()) :
    A.isBoolExp(exp) ? makeOk(T.makeBoolTExp()) :
    A.isStrExp(exp) ? makeOk(T.makeStrTExp()) :
    A.isPrimOp(exp) ? TC.typeofPrim(exp) :
    A.isVarRef(exp) ? E.applyTEnv(tenv, exp.var) :
    A.isIfExp(exp) ? typeofIf(exp, tenv) :
    A.isProcExp(exp) ? typeofProc(exp, tenv) :
    A.isAppExp(exp) ? typeofApp(exp, tenv) :
    A.isLetExp(exp) ? typeofLet(exp, tenv) :
    A.isLetrecExp(exp) ? typeofLetrec(exp, tenv) :
    A.isDefineExp(exp) ? typeofDefine(exp, tenv) :
    A.isProgram(exp) ? typeofProgram(exp, tenv) :
    // TODO: isSetExp(exp) isLitExp(exp)
    makeFailure(`Unknown type: ${format(exp)}`);

// Purpose: Compute the type of a sequence of expressions
// Signature: typeof-exps(exps, tenv)
// Type: [List(Cexp) * Tenv -> Texp]
// Check all the exps in a sequence - return type of last.
// Pre-conditions: exps is not empty.
const typeofExps = (exps: A.Exp[], tenv: E.TEnv): Result<T.TExp> =>
    isNonEmptyList<A.Exp>(exps) ?
        isEmpty(rest(exps)) ? typeofExp(first(exps), tenv) :
        bind(typeofExp(first(exps), tenv), _ => typeofExps(rest(exps), tenv)) :
    makeFailure(`Unexpected empty sequence of exps`);

// Purpose: compute the type of an if-exp
// Typing rule:
//   if type<test>(tenv) = boolean
//      type<then>(tenv) = t1
//      type<else>(tenv) = t1
// then type<(if test then else)>(tenv) = t1
const typeofIf = (ifExp: A.IfExp, tenv: E.TEnv): Result<T.TExp> => {
    const testTE = typeofExp(ifExp.test, tenv);
    const thenTE = typeofExp(ifExp.then, tenv);
    const altTE = typeofExp(ifExp.alt, tenv);
    const constraint1 = bind(testTE, (testTE: T.TExp) => checkEqualType(testTE, T.makeBoolTExp(), ifExp));
    const constraint2 = bind(thenTE, (thenTE: T.TExp) => 
                            bind(altTE, (altTE: T.TExp) => 
                                checkEqualType(thenTE, altTE, ifExp)));
    return bind(constraint1, (_c1: true) =>
                bind(constraint2, (_c2: true) => thenTE));
};

// Purpose: compute the type of a proc-exp
// Typing rule:
// If   type<body>(extend-tenv(x1=t1,...,xn=tn; tenv)) = t
// then type<lambda (x1:t1,...,xn:tn) : t exp)>(tenv) = (t1 * ... * tn -> t)
export const typeofProc = (proc: A.ProcExp, tenv: E.TEnv): Result<T.TExp> => {
    const argsTEs = R.map((vd) => vd.texp, proc.args);
    const extTEnv = E.makeExtendTEnv(R.map((vd) => vd.var, proc.args), argsTEs, tenv);
    const constraint1 = bind(typeofExps(proc.body, extTEnv), (bodyTE: T.TExp) => checkEqualType(bodyTE, proc.returnTE, proc));
    return mapv(constraint1, _ => T.makeProcTExp(argsTEs, proc.returnTE));
};


// Purpose: compute the type of an app-exp
// Typing rule:
// If   type<rator>(tenv) = (t1*..*tn -> t)
//      type<rand1>(tenv) = t1
//      ...
//      type<randn>(tenv) = tn
// then type<(rator rand1...randn)>(tenv) = t
// NOTE: This procedure is different from the one in L5-typecheck
export const typeofApp = (app: A.AppExp, tenv: E.TEnv): Result<T.TExp> => {
    const ratorTE = typeofExp(app.rator, tenv);
    const randsTE = mapResult((rand) => typeofExp(rand, tenv), app.rands);
    const returnTE = T.makeFreshTVar();
    const constraint = bind(ratorTE, (ratorTE: T.TExp) =>
                            bind(randsTE, (randsTE: T.TExp[]) =>
                                checkEqualType(ratorTE, T.makeProcTExp(randsTE, returnTE), app)));
    return bind(constraint, _ => makeOk(returnTE));
};

// Purpose: compute the type of a let-exp
// Typing rule:
// If   type<val1>(tenv) = t1
//      ...
//      type<valn>(tenv) = tn
//      type<body>(extend-tenv(var1=t1,..,varn=tn; tenv)) = t
// then type<let ((var1 val1) .. (varn valn)) body>(tenv) = t
export const typeofLet = (exp: A.LetExp, tenv: E.TEnv): Result<T.TExp> => {
    const vars = R.map((b) => b.var.var, exp.bindings);
    const vals = R.map((b) => b.val, exp.bindings);
    const varTEs = R.map((b) => b.var.texp, exp.bindings);
    const constraints = zipWithResult((varTE, val) => bind(typeofExp(val, tenv), (valTE: T.TExp) =>
                                                            checkEqualType(varTE, valTE, exp)),
                                      varTEs, vals);
    return bind(constraints, _ => typeofExps(exp.body, E.makeExtendTEnv(vars, varTEs, tenv)));
};

// Purpose: compute the type of a letrec-exp
// We make the same assumption as in L4 that letrec only binds proc values.
// Typing rule:
//   (letrec((p1 (lambda (x11 ... x1n1) body1)) ...) body)
//   tenv-body = extend-tenv(p1=(t11*..*t1n1->t1)....; tenv)
//   tenvi = extend-tenv(xi1=ti1,..,xini=tini; tenv-body)
// If   type<body1>(tenv1) = t1
//      ...
//      type<bodyn>(tenvn) = tn
//      type<body>(tenv-body) = t
// then type<(letrec((p1 (lambda (x11 ... x1n1) body1)) ...) body)>(tenv-body) = t
export const typeofLetrec = (exp: A.LetrecExp, tenv: E.TEnv): Result<T.TExp> => {
    const ps = R.map((b) => b.var.var, exp.bindings);
    const procs = R.map((b) => b.val, exp.bindings);
    if (! allT(A.isProcExp, procs)) {
        return bind(A.unparse(exp), (exp: string) =>
                    makeFailure(`letrec - only support binding of procedures - ${format(exp)}`));
    }
    const paramss = R.map((p) => p.args, procs);
    const bodies = R.map((p) => p.body, procs);
    const tijs = R.map((params) => R.map((p) => p.texp, params), paramss);
    const tis = R.map((proc) => proc.returnTE, procs);
    const tenvBody = E.makeExtendTEnv(ps, R.zipWith((tij, ti) => T.makeProcTExp(tij, ti), tijs, tis), tenv);
    const tenvIs = R.zipWith((params, tij) => E.makeExtendTEnv(R.map((p) => p.var, params), tij, tenvBody),
                             paramss, tijs);
    // Unfortunately ramda.zipWith does not work with 3 params
    const types = zipWithResult((bodyI, tenvI) => typeofExps(bodyI, tenvI), bodies, tenvIs)
    const constraints = bind(types, (types: T.TExp[]) => zipWithResult((typeI, ti) => checkEqualType(typeI, ti, exp), types, tis))
    return bind(constraints, _ => typeofExps(exp.body, tenvBody));
};


// Purpose: compute the type of a define
// Typing rule:
//   (define (var : texp) val)
// TODO - write the true definition
export const typeofDefine = (exp: A.DefineExp, tenv: E.TEnv): Result<T.VoidTExp> => {
    // return Error("TODO");
    return makeOk(T.makeVoidTExp());
};

// Purpose: compute the type of a program
// Typing rule:
// TODO - write the true definition
export const typeofProgram = (exp: A.Program, tenv: E.TEnv): Result<T.TExp> => {
    return makeFailure("TODO");
};

