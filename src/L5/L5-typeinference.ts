// L5-typeinference
import * as E from "fp-ts/Either";
import * as RA from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import * as A from "./L5-ast";
import * as TC from "./L5-typecheck";
import { TEnv, makeExtendTEnv, makeEmptyTEnv, applyTEnv } from "./TEnv";
import * as T from "./TExp";
import { allT, first, rest, isEmpty } from "../shared/list";
import { parse as p } from "../shared/parser";

// Purpose: Make type expressions equivalent by deriving a unifier
// Return an error if the types are not unifiable.
// Exp is only passed for documentation purposes.
// te1 can be undefined when it is retrieved from a type variable which is not yet bound.
const checkEqualType = (te1: T.TExp | undefined, te2: T.TExp, exp: A.Exp): E.Either<string, true> =>
    te1 === undefined ? pipe(T.unparseTExp(te2), E.chain(texp => E.left(`Incompatible types: undefined - ${texp}`))) :
    T.isTVar(te1) && T.isTVar(te2) ? ((T.eqTVar(te1, te2) ? E.of(true) : checkTVarEqualTypes(te1, te2, exp))) :
    T.isTVar(te1) ? checkTVarEqualTypes(te1, te2, exp) :
    T.isTVar(te2) ? checkTVarEqualTypes(te2, te1, exp) :
    T.isAtomicTExp(te1) && T.isAtomicTExp(te2) ?
        T.eqAtomicTExp(te1, te2) ? E.of(true) : pipe(
            T.unparseTExp(te1),
            E.chain(te1 => pipe(
                T.unparseTExp(te2),
                E.chain(te2 => E.left(`Incompatible atomic types ${te1} - ${te2}`))
            ))
        ) :
    T.isProcTExp(te1) && T.isProcTExp(te2) ? checkProcEqualTypes(te1, te2, exp) :
    pipe(
        T.unparseTExp(te1),
        E.chain(te1 => pipe(
            T.unparseTExp(te2),
            E.chain(te2 => E.left(`Incompatible types structure: ${te1} - ${te2}`))
        ))
    );

// Purpose: make two lists of equal length of type expressions equal
// Return an error if one of the pair of TExps are not compatible - true otherwise.
// Exp is only passed for documentation purposes.
const checkEqualTypes = (tes1: readonly T.TExp[], tes2: readonly T.TExp[], exp: A.Exp): E.Either<string, true> =>
    pipe(
        RA.zipWith(tes1, tes2, (te1, te2) => checkEqualType(te1, te2, exp)),
        E.sequenceArray,
        E.map(_ => true)
    )

const checkProcEqualTypes = (te1: T.ProcTExp, te2: T.ProcTExp, exp: A.Exp): E.Either<string, true> =>
    te1.paramTEs.length !== te2.paramTEs.length ? pipe(
        T.unparseTExp(te1),
        E.chain(te1 => pipe(
            T.unparseTExp(te2),
            E.chain(te2 => E.left(`Wrong number of args ${te1} - ${te2}`))
        ))
    ) :
    checkEqualTypes(T.procTExpComponents(te1), T.procTExpComponents(te2), exp);

// Purpose: check that a type variable matches a type expression
// Updates the var is needed to refer to te.
// Exp is only passed for documentation purposes.
const checkTVarEqualTypes = (tvar: T.TVar, te: T.TExp, exp: A.Exp): E.Either<string, true> =>
    T.tvarIsNonEmpty(tvar) ? checkEqualType(T.tvarContents(tvar), te, exp) :
    pipe(checkNoOccurrence(tvar, te, exp), E.map(_ => {
        T.tvarSetContents(tvar, te);
        return true;
    }));

// Purpose: when attempting to bind tvar to te - check whether tvar occurs in te.
// Throws error if a circular reference is found.
// Exp is only passed for documentation purposes.
// Pre-conditions: Tvar is not bound
const checkNoOccurrence = (tvar: T.TVar, te: T.TExp, exp: A.Exp): E.Either<string, true> => {
    const checkList = (tes: readonly T.TExp[]): E.Either<string, true> =>
        pipe(tes, E.traverseArray(loop), E.map(_ => true));

    const loop = (te1: T.TExp): E.Either<string, true> =>
        T.isAtomicTExp(te1) ? E.of(true) :
        T.isProcTExp(te1) ? checkList(T.procTExpComponents(te1)) :
        T.isTVar(te1) ? (T.eqTVar(te1, tvar) ? pipe(A.unparse(exp), E.chain(exp => E.left(`Occur check error - ${te1.var} - ${tvar.var} in ${exp}`))) : E.of(true)) :
        pipe(A.unparse(exp), E.chain(exp => E.left(`Bad type expression - ${JSON.stringify(te1)} in ${exp}`)));

    return loop(te);
}

// Compute the type of Typed-AST exps to TE
// ========================================
// Compute a Typed-AST exp to a Texp on the basis of its structure and the annotations it contains.

// Purpose: Compute the type of a concrete fully-typed expression
export const inferTypeOf = (conceteExp: string): E.Either<string, string> =>
    pipe(
        p(conceteExp),
        E.chain(A.parseL5Exp),
        E.chain(exp => typeofExp(exp, makeEmptyTEnv())),
        E.chain(T.unparseTExp)
    );

// Purpose: Compute the type of an expression
// Traverse the AST and check the type according to the exp type.
export const typeofExp = (exp: A.Parsed, tenv: TEnv): E.Either<string, T.TExp> =>
    A.isNumExp(exp) ? E.of(T.makeNumTExp()) :
    A.isBoolExp(exp) ? E.of(T.makeBoolTExp()) :
    A.isStrExp(exp) ? E.of(T.makeStrTExp()) :
    A.isPrimOp(exp) ? TC.typeofPrim(exp) :
    A.isVarRef(exp) ? applyTEnv(tenv, exp.var) :
    A.isIfExp(exp) ? typeofIf(exp, tenv) :
    A.isProcExp(exp) ? typeofProc(exp, tenv) :
    A.isAppExp(exp) ? typeofApp(exp, tenv) :
    A.isLetExp(exp) ? typeofLet(exp, tenv) :
    A.isLetrecExp(exp) ? typeofLetrec(exp, tenv) :
    A.isDefineExp(exp) ? typeofDefine(exp, tenv) :
    A.isProgram(exp) ? typeofProgram(exp, tenv) :
    // TODO: isSetExp(exp) isLitExp(exp)
    E.left("Unknown type");

// Purpose: Compute the type of a sequence of expressions
// Signature: typeof-exps(exps, tenv)
// Type: [List(Cexp) * Tenv -> Texp]
// Check all the exps in a sequence - return type of last.
// Pre-conditions: exps is not empty.
const typeofExps = (exps: readonly A.Exp[], tenv: TEnv): E.Either<string, T.TExp> =>
    isEmpty(rest(exps)) ? typeofExp(first(exps), tenv) :
    pipe(typeofExp(first(exps), tenv), E.chain(_ => typeofExps(rest(exps), tenv)));

// Purpose: compute the type of an if-exp
// Typing rule:
//   if type<test>(tenv) = boolean
//      type<then>(tenv) = t1
//      type<else>(tenv) = t1
// then type<(if test then else)>(tenv) = t1
const typeofIf = (ifExp: A.IfExp, tenv: TEnv): E.Either<string, T.TExp> => {
    const testTE = typeofExp(ifExp.test, tenv);
    const thenTE = typeofExp(ifExp.then, tenv);
    const altTE = typeofExp(ifExp.alt, tenv);
    const constraint1 = pipe(testTE, E.chain(testTE => checkEqualType(testTE, T.makeBoolTExp(), ifExp)));
    const constraint2 = pipe(
        thenTE,
        E.chain(thenTE => pipe(
            altTE,
            E.chain(altTE => checkEqualType(thenTE, altTE, ifExp))
        ))
    );
    return pipe(
        constraint1,
        E.chain(_ => pipe(
            constraint2,
            E.chain(_ => thenTE)
        ))
    );
};

// Purpose: compute the type of a proc-exp
// Typing rule:
// If   type<body>(extend-tenv(x1=t1,...,xn=tn; tenv)) = t
// then type<lambda (x1:t1,...,xn:tn) : t exp)>(tenv) = (t1 * ... * tn -> t)
export const typeofProc = (proc: A.ProcExp, tenv: TEnv): E.Either<string, T.TExp> => {
    const argsTEs = pipe(proc.args, RA.map(vd => vd.texp));
    const extTEnv = makeExtendTEnv(pipe(proc.args, RA.map(vd => vd.var)), argsTEs, tenv);
    return pipe(
        typeofExps(proc.body, extTEnv),
        E.chain(bodyTE => checkEqualType(bodyTE, proc.returnTE, proc)),
        E.map(_ => T.makeProcTExp(argsTEs, proc.returnTE))
    );
};


// Purpose: compute the type of an app-exp
// Typing rule:
// If   type<rator>(tenv) = (t1*..*tn -> t)
//      type<rand1>(tenv) = t1
//      ...
//      type<randn>(tenv) = tn
// then type<(rator rand1...randn)>(tenv) = t
// NOTE: This procedure is different from the one in L5-typecheck
export const typeofApp = (app: A.AppExp, tenv: TEnv): E.Either<string, T.TExp> => {
    const ratorTE = typeofExp(app.rator, tenv);
    const randsTE = pipe(app.rands, E.traverseArray(rand => typeofExp(rand, tenv)));
    const returnTE = T.makeFreshTVar();
    return pipe(
        ratorTE,
        E.chain(ratorTE => pipe(
            randsTE,
            E.chain(randsTE => checkEqualType(ratorTE, T.makeProcTExp(randsTE, returnTE), app)),
            E.map(_ => returnTE)
        ))
    );
};

// Purpose: compute the type of a let-exp
// Typing rule:
// If   type<val1>(tenv) = t1
//      ...
//      type<valn>(tenv) = tn
//      type<body>(extend-tenv(var1=t1,..,varn=tn; tenv)) = t
// then type<let ((var1 val1) .. (varn valn)) body>(tenv) = t
export const typeofLet = (exp: A.LetExp, tenv: TEnv): E.Either<string, T.TExp> => {
    const vars = pipe(exp.bindings, RA.map(b => b.var.var));
    const vals = pipe(exp.bindings, RA.map(b => b.val));
    const varTEs = pipe(exp.bindings, RA.map(b => b.var.texp));
    return pipe(
        RA.zipWith(varTEs, vals, (varTE, val) => pipe(
            typeofExp(val, tenv),
            E.chain(valTE => checkEqualType(varTE, valTE, exp))
        )),
        E.sequenceArray,
        E.chain(_ => typeofExps(exp.body, makeExtendTEnv(vars, varTEs, tenv)))
    );
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
export const typeofLetrec = (exp: A.LetrecExp, tenv: TEnv): E.Either<string, T.TExp> => {
    const ps = pipe(exp.bindings, RA.map(b => b.var.var));
    const procs = pipe(exp.bindings, RA.map(b => b.val));
    if (! allT(A.isProcExp, procs)) {
        return pipe(A.unparse(exp), E.chain(exp => E.left(`letrec - only support binding of procedures - ${exp}`)));
    }
    const paramss = pipe(procs, RA.map(p => p.args));
    const bodies = pipe(procs, RA.map(p => p.body));
    const tijs = pipe(paramss, RA.map(params => pipe(params, RA.map(p => p.texp))));
    const tis = pipe(procs, RA.map(proc => proc.returnTE));
    const tenvBody = makeExtendTEnv(ps, RA.zipWith(tijs, tis, (tij, ti) => T.makeProcTExp(tij, ti)), tenv);    
    const tenvIs = RA.zipWith(paramss, tijs, (params, tij) => makeExtendTEnv(pipe(params, RA.map(p => p.var)), tij, tenvBody));
    return pipe(
        RA.zipWith(bodies, tenvIs, typeofExps),
        E.sequenceArray,
        E.chain(types => pipe(
            RA.zipWith(types, tis, (typeI, ti) => checkEqualType(typeI, ti, exp)),
            E.sequenceArray
        )),
        E.chain(_ => typeofExps(exp.body, tenvBody))
    );
};


// Purpose: compute the type of a define
// Typing rule:
//   (define (var : texp) val)
// TODO - write the true definition
export const typeofDefine = (exp: A.DefineExp, tenv: TEnv): E.Either<string, T.VoidTExp> => {
    // return Error("TODO");
    return E.of(T.makeVoidTExp());
};

// Purpose: compute the type of a program
// Typing rule:
// TODO - write the true definition
export const typeofProgram = (exp: A.Program, tenv: TEnv): E.Either<string, T.TExp> => {
    return E.left("TODO");
};

