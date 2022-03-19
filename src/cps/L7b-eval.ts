// L7-eval: CPS version of L5 with concrete data-structure continuations

import * as E from "fp-ts/Either";
import { map, replicate, zipWith } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         ProcExp, Program, SetExp } from '../L5/L5-ast';
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { parseL5Exp, unparse } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, ExtEnv } from "../L5/L5-env";
import { isClosure, makeClosure, Closure, Value, valueToString } from "../L5/L5-value";
import { isEmpty, first, rest } from '../shared/list';
import { applyPrimitive } from "../L5/evalPrimitive";
import { parse as p } from "../shared/parser";

// ========================================================
// Concrete Continuation datatype
// type Cont = (res: E.Either<string, Value>) => E.Either<string, Value>;
// type ContArray = (results: E.Either<string, readonly Value[]>) => E.Either<string, Value>;
export type Cont = IfCont | FirstCont | SetCont | AppCont1 | ExpsCont1 | DefCont | TopCont;
export type ContArray = LetCont | LetrecCont | AppCont2 | ExpsCont2;

export const isCont = (x: any): x is Cont =>
    isIfCont(x) || isFirstCont(x) || isSetCont(x) ||
    isAppCont1(x) || isExpsCont1(x) || isDefCont(x) || isTopCont(x);
export const isContArray = (x: any): x is ContArray =>
    isLetCont(x) || isLetrecCont(x) || isAppCont2(x) || isExpsCont2(x);

export interface TopCont {tag: "TopCont"}
export const makeTopCont = (): TopCont => ({tag: "TopCont"});
export const isTopCont = (x: any): x is TopCont => x.tag === "TopCont";

export interface IfCont {tag: "IfCont", exp: IfExp, env: Env, cont: Cont}
export const makeIfCont = (exp: IfExp, env: Env, cont: Cont): IfCont => ({tag: "IfCont", env: env, exp: exp, cont: cont});
export const isIfCont = (x: any): x is IfCont => x.tag === "IfCont";

export interface FirstCont {tag: "FirstCont", exps: readonly Exp[], env: Env, cont: Cont}
export const makeFirstCont = (exps: readonly Exp[], env: Env, cont: Cont): FirstCont => ({tag: "FirstCont", env: env, exps: exps, cont: cont});
export const isFirstCont = (x: any): x is FirstCont => x.tag === "FirstCont";

export interface SetCont {tag: "SetCont", exp: SetExp, env: Env, cont: Cont}
export const makeSetCont = (exp: SetExp, env: Env, cont: Cont): SetCont => ({tag: "SetCont", env: env, exp: exp, cont: cont});
export const isSetCont = (x: any): x is SetCont => x.tag === "SetCont";

export interface AppCont1 {tag: "AppCont1", exp: AppExp, env: Env, cont: Cont}
export const makeAppCont1 = (exp: AppExp, env: Env, cont: Cont): AppCont1 => ({tag: "AppCont1", env: env, exp: exp, cont: cont});
export const isAppCont1 = (x: any): x is AppCont1 => x.tag === "AppCont1";

export interface ExpsCont1 {tag: "ExpsCont1", exps: readonly Exp[], env: Env, cont: ContArray}
export const makeExpsCont1 = (exps: readonly Exp[], env: Env, cont: ContArray): ExpsCont1 => ({tag: "ExpsCont1", env: env, exps: exps, cont: cont});
export const isExpsCont1 = (x: any): x is ExpsCont1 => x.tag === "ExpsCont1";

export interface LetCont {tag: "LetCont", exp: LetExp, env: Env, cont: Cont}
export const makeLetCont = (exp: LetExp, env: Env, cont: Cont): LetCont => ({tag: "LetCont", env: env, exp: exp, cont: cont});
export const isLetCont = (x: any): x is LetCont => x.tag === "LetCont";

export interface LetrecCont {tag: "LetrecCont", exp: LetrecExp, env: ExtEnv, cont: Cont}
export const makeLetrecCont = (exp: LetrecExp, env: ExtEnv, cont: Cont): LetrecCont => ({tag: "LetrecCont", env: env, exp: exp, cont: cont});
export const isLetrecCont = (x: any): x is LetrecCont => x.tag === "LetrecCont";

export interface AppCont2 {tag: "AppCont2", proc: E.Either<string, Value>, env: Env, cont: Cont}
export const makeAppCont2 = (proc: E.Either<string, Value>, env: Env, cont: Cont): AppCont2 => ({tag: "AppCont2", proc: proc, env: env, cont: cont});
export const isAppCont2 = (x: any): x is AppCont2 => x.tag === "AppCont2";

export interface ExpsCont2 {tag: "ExpsCont2", firstVal: E.Either<string, Value>, cont: ContArray}
export const makeExpsCont2 = (firstVal: E.Either<string, Value>, cont: ContArray): ExpsCont2 => ({tag: "ExpsCont2", firstVal: firstVal, cont: cont});
export const isExpsCont2 = (x: any): x is ExpsCont2 => x.tag === "ExpsCont2";

export interface DefCont {tag: "DefCont", exp: DefineExp, exps: readonly Exp[], cont: Cont}
export const makeDefCont = (exp: DefineExp, exps: readonly Exp[], cont: Cont): DefCont => ({tag: "DefCont", exp: exp, exps: exps, cont: cont});
export const isDefCont = (x: any): x is DefCont => x.tag === "DefCont";

const applyCont = (cont: Cont, val: E.Either<string, Value>): E.Either<string, Value> => 
    isIfCont(cont) ? applyIfCont(cont, val) :
    isFirstCont(cont) ? applyFirstCont(cont, val) :
    isSetCont(cont) ? applySetCont(cont, val) :
    isAppCont1(cont) ? applyAppCont1(cont, val) :
    isExpsCont1(cont) ? applyExpsCont1(cont, val) :
    isDefCont(cont) ? applyDefCont(cont, val) :
    isTopCont(cont) ? applyTopCont(cont, val) :
    cont;

const applyContArray = (cont: ContArray, val: E.Either<string, readonly Value[]>): E.Either<string, Value> =>
    isLetCont(cont) ? applyLetCont(cont, val) :
    isLetrecCont(cont) ? applyLetrecCont(cont, val) :
    isAppCont2(cont) ? applyAppCont2(cont, val) :
    isExpsCont2(cont) ? applyExpsCont2(cont, val) :
    cont;

const applyTopCont = (cont: TopCont, val: E.Either<string, Value>): E.Either<string, Value> => {
    pipe(val, E.match(console.error, v => console.log(valueToString(v))));
    return val;
}

const applyIfCont = (cont: IfCont, testVal: E.Either<string, Value>): E.Either<string, Value> =>
    pipe(
        testVal,
        E.chain(test => isTrueValue(test) ?
                        evalCont(cont.exp.then, cont.env, cont.cont) :
                        evalCont(cont.exp.alt, cont.env, cont.cont))
    );

const applyLetCont = (cont: LetCont, vals: E.Either<string, readonly Value[]>): E.Either<string, Value> =>
    pipe(vals, E.chain(vals => evalSequence(cont.exp.body, makeExtEnv(letVars(cont.exp), vals, cont.env), cont.cont)));

export const applyFirstCont = (cont: FirstCont, firstVal: E.Either<string, Value>): E.Either<string, Value> =>
    pipe(firstVal, E.chain(_ => evalSequence(cont.exps, cont.env, cont.cont)));

export const applyLetrecCont = (cont: LetrecCont, cvals: E.Either<string, readonly Value[]>): E.Either<string, Value> =>
    pipe(
        cvals,
        E.chain(cvals => {
            zipWith(cont.env.frame.fbindings, cvals, setFBinding);
            return evalSequence(cont.exp.body, cont.env, cont.cont);
        })
    );

export const applySetCont = (cont: SetCont, rhsVal: E.Either<string, Value>): E.Either<string, Value> =>
    pipe(
        rhsVal,
        E.chain(rhs => pipe(
            applyEnvBdg(cont.env, cont.exp.var.var),
            E.chain(bdg => {
                setFBinding(bdg, rhs);
                return applyCont(cont.cont, E.of(undefined));
            })
        ))
    );

export const applyAppCont1 = (cont: AppCont1, proc: E.Either<string, Value>): E.Either<string, Value> =>
    evalExps(cont.exp.rands, cont.env, makeAppCont2(proc, cont.env, cont.cont));

export const applyAppCont2 = (cont: AppCont2, args: E.Either<string, readonly Value[]>): E.Either<string, Value> =>
    pipe(
        cont.proc,
        E.chain(proc => pipe(
            args,
            E.chain(args => applyProcedure(proc, args, cont.cont))
        ))
    );

export const applyExpsCont1 = (cont: ExpsCont1, firstVal: E.Either<string, Value>): E.Either<string, Value> =>
    evalExps(cont.exps, cont.env, makeExpsCont2(firstVal, cont.cont));

export const applyExpsCont2 = (cont: ExpsCont2, restVals: E.Either<string, readonly Value[]>): E.Either<string, Value> =>
    pipe(
        cont.firstVal,
        E.chain(first => pipe(
            restVals,
            E.chain(rest => applyContArray(cont.cont, E.of([first, ...rest])))
        ))
    );

export const applyDefCont = (cont: DefCont, rhsVal: E.Either<string, Value>): E.Either<string, Value> =>
    pipe(
        rhsVal,
        E.chain(rhs => {
            globalEnvAddBinding(cont.exp.var.var, rhs);
            return evalSequence(cont.exps, theGlobalEnv, cont.cont);
        })
    );

// ========================================================
// Eval functions

export const evalCont = (exp: CExp, env: Env, cont: Cont): E.Either<string, Value> =>
    isNumExp(exp) ? applyCont(cont, E.of(exp.val)) :
    isBoolExp(exp) ? applyCont(cont, E.of(exp.val)) :
    isStrExp(exp) ? applyCont(cont, E.of(exp.val)) :
    isPrimOp(exp) ? applyCont(cont, E.of(exp)) :
    isVarRef(exp) ? applyCont(cont, applyEnv(env, exp.var)) :
    isLitExp(exp) ? applyCont(cont, E.of(exp.val)) :
    isIfExp(exp) ? evalIf(exp, env, cont) :
    isProcExp(exp) ? evalProc(exp, env, cont) :
    isLetExp(exp) ? evalLet(exp, env, cont) :
    isLetrecExp(exp) ? evalLetrec(exp, env, cont) :
    isSetExp(exp) ? evalSet(exp, env, cont) :
    isAppExp(exp) ? evalApp(exp, env, cont) :
    exp;

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env, cont: Cont): E.Either<string, Value> =>
    evalCont(exp.test, env, makeIfCont(exp, env, cont));

const evalProc = (exp: ProcExp, env: Env, cont: Cont): E.Either<string, Value> =>
    applyCont(cont, E.of(makeClosure(exp.args, exp.body, env)));

// Return the vals (rhs) of the bindings of a let expression
const letVals = (exp: LetExp | LetrecExp): readonly CExp[] =>
    pipe(exp.bindings, map(b => b.val));

// Return the vars (lhs) of the bindings of a let expression
const letVars = (exp: LetExp | LetrecExp): readonly string[] =>
    pipe(exp.bindings, map(b => b.var.var));

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env, cont: Cont): E.Either<string, Value> =>
    evalExps(letVals(exp), env, makeLetCont(exp, env, cont));

// LETREC: Direct evaluation rule without syntax expansion
// 1. extend the env with vars initialized to void (temporary value)
// 2. compute the vals in the new extended env
// 3. update the bindings of the vars to the computed vals
// 4. compute body in extended env
export const evalLetrec = (exp: LetrecExp, env: Env, cont: Cont): E.Either<string, Value> => {
    const vars = letVars(exp);
    const vals = letVals(exp);
    const extEnv = makeExtEnv(vars, replicate(vars.length, undefined), env);
    // Compute the vals in the extended env
    return evalExps(vals, extEnv, makeLetrecCont(exp, extEnv, cont));
}

// L4-eval-box: Handling of mutation with set!
export const evalSet = (exp: SetExp, env: Env, cont: Cont): E.Either<string, Value> =>
    evalCont(exp.val, env, makeSetCont(exp, env, cont));

// L4 evalApp
export const evalApp = (exp: AppExp, env: Env, cont: Cont): E.Either<string, Value> =>
    evalCont(exp.rator, env, makeAppCont1(exp, env, cont));

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
export const applyProcedure = (proc: Value, args: readonly Value[], cont: Cont): E.Either<string, Value> =>
    isPrimOp(proc) ? applyCont(cont, applyPrimitive(proc, args)) :
    isClosure(proc) ? applyClosure(proc, args, cont) :
    applyCont(cont, E.left(`Bad procedure ${JSON.stringify(proc)}`));

export const applyClosure = (proc: Closure, args: readonly Value[], cont: Cont): E.Either<string, Value> => {
    const vars = pipe(proc.params, map(v => v.var));
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env), cont);
}

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: readonly Exp[], env: Env, cont: Cont): E.Either<string, Value> =>
    isEmpty(exps) ? applyCont(cont, E.left("Empty Sequence")) :
    evalSequenceFR(first(exps), rest(exps), env, cont);

const evalSequenceFR = (exp: Exp, exps: readonly Exp[], env: Env, cont: Cont): E.Either<string, Value> =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, makeFirstCont(exps, env, cont));

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
const evalDefineExps = (exp: DefineExp, exps: readonly Exp[], cont: Cont): E.Either<string, Value> =>
    evalCont(exp.val, theGlobalEnv, makeDefCont(exp, exps, cont));

// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: readonly Exp[], env: Env, cont: ContArray): E.Either<string, Value> =>
    isEmpty(exps) ? applyContArray(cont, E.of([])) :
    evalExpsFR(first(exps), rest(exps), env, cont);

const evalExpsFR = (exp: Exp, exps: readonly Exp[], env: Env, cont: ContArray): E.Either<string, Value> =>
    isDefineExp(exp) ? applyContArray(cont, pipe(unparse(exp),E.chain(e => E.left(`Unexpected define: ${e}`)))) :
    evalCont(exp, env, makeExpsCont1(exps, env, cont));

// Evaluate a program
// Main program
export const evalProgram = (program: Program): E.Either<string, Value> =>
    evalSequence(program.exps, theGlobalEnv, makeTopCont());

export const evalParse = (s: string): E.Either<string, Value> =>
    pipe(
        p(s),
        E.chain(parseL5Exp),
        E.chain(exp => evalSequence([exp], theGlobalEnv, makeTopCont()))
    );
