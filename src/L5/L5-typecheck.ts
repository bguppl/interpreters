// L5-typecheck
// ========================================================
import _ from "lodash";
import * as E from "fp-ts/Either";
import { map, zipWith } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { isAppExp, isBoolExp, isDefineExp, isIfExp, isLetrecExp, isLetExp, isNumExp,
         isPrimOp, isProcExp, isProgram, isStrExp, isVarRef, parseL5Exp, unparse,
         AppExp, BoolExp, DefineExp, Exp, IfExp, LetrecExp, LetExp, NumExp,
         Parsed, PrimOp, ProcExp, Program, StrExp } from "./L5-ast";
import { applyTEnv, makeEmptyTEnv, makeExtendTEnv, TEnv } from "./TEnv";
import { isProcTExp, makeBoolTExp, makeNumTExp, makeProcTExp, makeStrTExp, makeVoidTExp,
         parseTE, unparseTExp,
         BoolTExp, NumTExp, StrTExp, TExp, VoidTExp } from "./TExp";
import { isEmpty, allT, first, rest } from '../shared/list';
import { parse as p } from "../shared/parser";

// Purpose: Check that type expressions are equivalent
// as part of a fully-annotated type check process of exp.
// Return an error if the types are different - true otherwise.
// Exp is only passed for documentation purposes.
const checkEqualType = (te1: TExp, te2: TExp, exp: Exp): E.Either<string, true> => 
    _.isEqual(te1, te2) ? E.of(true) :
    pipe(
        unparseTExp(te1),
        E.chain(
            te1 => pipe(
                unparseTExp(te2),
                E.chain(te2 => pipe(
                    unparse(exp),
                    E.chain(exp => E.left(`Incompatible types: ${te1} and ${te2} in ${exp}`))
                ))
            )
        )
    );

// Compute the type of L5 AST exps to TE
// ===============================================
// Compute a Typed-L5 AST exp to a Texp on the basis
// of its structure and the annotations it contains.

// Purpose: Compute the type of a concrete fully-typed expression
export const L5typeof = (concreteExp: string): E.Either<string, string> =>
    pipe(
        p(concreteExp),
        E.chain(parseL5Exp),
        E.chain(e => pipe(
            typeofExp(e, makeEmptyTEnv()),
            E.chain(unparseTExp)
        ))
    );

// Purpose: Compute the type of an expression
// Traverse the AST and check the type according to the exp type.
// We assume that all variables and procedures have been explicitly typed in the program.
export const typeofExp = (exp: Parsed, tenv: TEnv): E.Either<string, TExp> =>
    isNumExp(exp) ? E.of(typeofNum(exp)) :
    isBoolExp(exp) ? E.of(typeofBool(exp)) :
    isStrExp(exp) ? E.of(typeofStr(exp)) :
    isPrimOp(exp) ? typeofPrim(exp) :
    isVarRef(exp) ? applyTEnv(tenv, exp.var) :
    isIfExp(exp) ? typeofIf(exp, tenv) :
    isProcExp(exp) ? typeofProc(exp, tenv) :
    isAppExp(exp) ? typeofApp(exp, tenv) :
    isLetExp(exp) ? typeofLet(exp, tenv) :
    isLetrecExp(exp) ? typeofLetrec(exp, tenv) :
    isDefineExp(exp) ? typeofDefine(exp, tenv) :
    isProgram(exp) ? typeofProgram(exp, tenv) :
    // TODO: isSetExp(exp) isLitExp(exp)
    E.left("Unknown type");

// Purpose: Compute the type of a sequence of expressions
// Check all the exps in a sequence - return type of last.
// Pre-conditions: exps is not empty.
export const typeofExps = (exps: readonly Exp[], tenv: TEnv): E.Either<string, TExp> =>
    isEmpty(rest(exps)) ? typeofExp(first(exps), tenv) :
    pipe(typeofExp(first(exps), tenv), E.chain(_ => typeofExps(rest(exps), tenv)));

// a number literal has type num-te
export const typeofNum = (n: NumExp): NumTExp => makeNumTExp();

// a boolean literal has type bool-te
export const typeofBool = (b: BoolExp): BoolTExp => makeBoolTExp();

// a string literal has type str-te
const typeofStr = (s: StrExp): StrTExp => makeStrTExp();

// primitive ops have known proc-te types
const numOpTExp = parseTE('(number * number -> number)');
const numCompTExp = parseTE('(number * number -> boolean)');
const boolOpTExp = parseTE('(boolean * boolean -> boolean)');

// Todo: cons, car, cdr, list
export const typeofPrim = (p: PrimOp): E.Either<string, TExp> =>
    (p.op === '+') ? numOpTExp :
    (p.op === '-') ? numOpTExp :
    (p.op === '*') ? numOpTExp :
    (p.op === '/') ? numOpTExp :
    (p.op === 'and') ? boolOpTExp :
    (p.op === 'or') ? boolOpTExp :
    (p.op === '>') ? numCompTExp :
    (p.op === '<') ? numCompTExp :
    (p.op === '=') ? numCompTExp :
    // Important to use a different signature for each op with a TVar to avoid capture
    (p.op === 'number?') ? parseTE('(T -> boolean)') :
    (p.op === 'boolean?') ? parseTE('(T -> boolean)') :
    (p.op === 'string?') ? parseTE('(T -> boolean)') :
    (p.op === 'list?') ? parseTE('(T -> boolean)') :
    (p.op === 'pair?') ? parseTE('(T -> boolean)') :
    (p.op === 'symbol?') ? parseTE('(T -> boolean)') :
    (p.op === 'not') ? parseTE('(boolean -> boolean)') :
    (p.op === 'eq?') ? parseTE('(T1 * T2 -> boolean)') :
    (p.op === 'string=?') ? parseTE('(T1 * T2 -> boolean)') :
    (p.op === 'display') ? parseTE('(T -> void)') :
    (p.op === 'newline') ? parseTE('(Empty -> void)') :
    E.left(`Primitive not yet implemented: ${p.op}`);

// Purpose: compute the type of an if-exp
// Typing rule:
//   if type<test>(tenv) = boolean
//      type<then>(tenv) = t1
//      type<else>(tenv) = t1
// then type<(if test then else)>(tenv) = t1
export const typeofIf = (ifExp: IfExp, tenv: TEnv): E.Either<string, TExp> => {
    const testTE = typeofExp(ifExp.test, tenv);
    const thenTE = typeofExp(ifExp.then, tenv);
    const altTE = typeofExp(ifExp.alt, tenv);
    const constraint1 = pipe(testTE, E.chain(testTE => checkEqualType(testTE, makeBoolTExp(), ifExp)));    
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
export const typeofProc = (proc: ProcExp, tenv: TEnv): E.Either<string, TExp> => {
    const argsTEs = pipe(proc.args, map(vd => vd.texp));
    const extTEnv = makeExtendTEnv(pipe(proc.args, map(vd => vd.var)), argsTEs, tenv);
    return pipe(
        typeofExps(proc.body, extTEnv),
        E.chain(body => checkEqualType(body, proc.returnTE, proc)),
        E.map(_ => makeProcTExp(argsTEs, proc.returnTE))
    );
};

// Purpose: compute the type of an app-exp
// Typing rule:
// If   type<rator>(tenv) = (t1*..*tn -> t)
//      type<rand1>(tenv) = t1
//      ...
//      type<randn>(tenv) = tn
// then type<(rator rand1...randn)>(tenv) = t
// We also check the correct number of arguments is passed.
export const typeofApp = (app: AppExp, tenv: TEnv): E.Either<string, TExp> =>
    pipe(
        typeofExp(app.rator, tenv),
        E.chain(ratorTE => {
            if (!isProcTExp(ratorTE)) {
                return pipe(
                    unparseTExp(ratorTE),
                    E.chain(rator => pipe(
                        unparse(app),
                        E.chain(exp => E.left(`Application of non-procedure: ${rator} in ${exp}`))
                    ))
                )
            }
            if (app.rands.length !== ratorTE.paramTEs.length) {
                return pipe(unparse(app), E.chain(exp => E.left(`Wrong parameter numbers passed to proc: ${exp}`)))
            }
            return pipe(
                zipWith(app.rands, ratorTE.paramTEs, (rand, trand) => pipe(
                    typeofExp(rand, tenv),
                    E.chain(typeOfRand => checkEqualType(typeOfRand, trand, app))
                )),
                E.sequenceArray,
                E.map(_ => ratorTE.returnTE)
            )
        })
    );

// Purpose: compute the type of a let-exp
// Typing rule:
// If   type<val1>(tenv) = t1
//      ...
//      type<valn>(tenv) = tn
//      type<body>(extend-tenv(var1=t1,..,varn=tn; tenv)) = t
// then type<let ((var1 val1) .. (varn valn)) body>(tenv) = t
export const typeofLet = (exp: LetExp, tenv: TEnv): E.Either<string, TExp> => {
    const vars = pipe(exp.bindings, map(b => b.var.var));
    const vals = pipe(exp.bindings, map(b => b.val));
    const varTEs = pipe(exp.bindings, map(b => b.var.texp));
    return pipe(
        zipWith(varTEs, vals, (varTE, val) => pipe(
            typeofExp(val, tenv),
            E.chain(typeOfVal => checkEqualType(varTE, typeOfVal, exp))
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
export const typeofLetrec = (exp: LetrecExp, tenv: TEnv): E.Either<string, TExp> => {
    const ps = pipe(exp.bindings, map(b => b.var.var));
    const procs = pipe(exp.bindings, map(b => b.val));
    if (!allT(isProcExp, procs)) {
        return E.left(`letrec - only support binding of procedures - ${exp}`)
    }
    const paramss = pipe(procs, map(p => p.args));
    const bodies = pipe(procs, map(p => p.body));
    const tijs = pipe(paramss, map(params => pipe(params, map(p => p.texp))));
    const tis = pipe(procs, map(p => p.returnTE));
    const tenvBody = makeExtendTEnv(ps, zipWith(tijs, tis, (tij, ti) => makeProcTExp(tij, ti)), tenv);
    const tenvIs = zipWith(paramss, tijs, (params, tij) => makeExtendTEnv(pipe(params, map(p => p.var)), tij, tenvBody));
    return pipe(
        zipWith(bodies, tenvIs, typeofExps),
        E.sequenceArray,
        E.chain(types => pipe(
            zipWith(types, tis, (typeI, ti) => checkEqualType(typeI, ti, exp)),
            E.sequenceArray
        )),
        E.chain(_ => typeofExps(exp.body, tenvBody))
    );
};

// Typecheck a full program
// TODO: Thread the TEnv (as in L1)

// Purpose: compute the type of a define
// Typing rule:
//   (define (var : texp) val)
// TODO - write the true definition
export const typeofDefine = (exp: DefineExp, tenv: TEnv): E.Either<string, VoidTExp> => {
    // return Error("TODO");
    return E.of(makeVoidTExp());
};

// Purpose: compute the type of a program
// Typing rule:
// TODO - write the true definition
export const typeofProgram = (exp: Program, tenv: TEnv): E.Either<string, TExp> =>
    E.left("TODO");
