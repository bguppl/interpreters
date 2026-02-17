// L7-eval: CPS version of L5 with concrete data-structure continuations

import { map, reduce, repeat, zip, zipWith } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         Parsed, PrimOp, ProcExp, Program, SetExp, VarDecl } from '../L5/L5-ast';
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { parseL5Exp, unparse } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isProgram, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, ExtEnv, FBinding } from "../L5/L5-env";
import { isClosure, isCompoundSExp, makeClosure, makeCompoundSExp,
         Closure, CompoundSExp, Value, valueToString } from "../L5/L5-value";
import { isEmpty, allT, first, rest, isNonEmptyList } from '../shared/list';
import { Result, makeOk, makeFailure, bind, either } from "../shared/result";
import { applyPrimitive } from "../L5/evalPrimitive";
import { parse as p } from "../shared/parser";
import { format } from "../shared/format";

// ========================================================
// Concrete Continuation datatype
// type Cont = (res: Result<Value>) => Result<Value>;
// type ContArray = (results: Result<Value[]>) => Result<Value>;
export type Cont = IfCont | FirstCont | SetCont | AppCont1 | ExpsCont1 | DefCont | TopCont;
export type ContArray = LetCont | LetrecCont | AppCont2 | ExpsCont2;

export const isCont = (x: any): x is Cont =>
    isIfCont(x) || isFirstCont(x) || isSetCont(x) ||
    isAppCont1(x) || isExpsCont1(x) || isDefCont(x) || isTopCont(x);
export const isContArray = (x: any): x is ContArray =>
    isLetCont(x) || isLetrecCont(x) || isAppCont2(x) || isExpsCont2(x);

export type TopCont = {tag: "TopCont"}
export const makeTopCont = (): TopCont => ({tag: "TopCont"});
export const isTopCont = (x: any): x is TopCont => x.tag === "TopCont";

export type IfCont = {tag: "IfCont", exp: IfExp, env: Env, cont: Cont}
export const makeIfCont = (exp: IfExp, env: Env, cont: Cont): IfCont => ({tag: "IfCont", env: env, exp: exp, cont: cont});
export const isIfCont = (x: any): x is IfCont => x.tag === "IfCont";

export type FirstCont = {tag: "FirstCont", exps: Exp[], env: Env, cont: Cont}
export const makeFirstCont = (exps: Exp[], env: Env, cont: Cont): FirstCont => ({tag: "FirstCont", env: env, exps: exps, cont: cont});
export const isFirstCont = (x: any): x is FirstCont => x.tag === "FirstCont";

export type SetCont = {tag: "SetCont", exp: SetExp, env: Env, cont: Cont}
export const makeSetCont = (exp: SetExp, env: Env, cont: Cont): SetCont => ({tag: "SetCont", env: env, exp: exp, cont: cont});
export const isSetCont = (x: any): x is SetCont => x.tag === "SetCont";

export type AppCont1 = {tag: "AppCont1", exp: AppExp, env: Env, cont: Cont}
export const makeAppCont1 = (exp: AppExp, env: Env, cont: Cont): AppCont1 => ({tag: "AppCont1", env: env, exp: exp, cont: cont});
export const isAppCont1 = (x: any): x is AppCont1 => x.tag === "AppCont1";

export type ExpsCont1 = {tag: "ExpsCont1", exps: Exp[], env: Env, cont: ContArray}
export const makeExpsCont1 = (exps: Exp[], env: Env, cont: ContArray): ExpsCont1 => ({tag: "ExpsCont1", env: env, exps: exps, cont: cont});
export const isExpsCont1 = (x: any): x is ExpsCont1 => x.tag === "ExpsCont1";

export type LetCont = {tag: "LetCont", exp: LetExp, env: Env, cont: Cont}
export const makeLetCont = (exp: LetExp, env: Env, cont: Cont): LetCont => ({tag: "LetCont", env: env, exp: exp, cont: cont});
export const isLetCont = (x: any): x is LetCont => x.tag === "LetCont";

export type LetrecCont = {tag: "LetrecCont", exp: LetrecExp, env: ExtEnv, cont: Cont}
export const makeLetrecCont = (exp: LetrecExp, env: ExtEnv, cont: Cont): LetrecCont => ({tag: "LetrecCont", env: env, exp: exp, cont: cont});
export const isLetrecCont = (x: any): x is LetrecCont => x.tag === "LetrecCont";

export type AppCont2 = {tag: "AppCont2", proc: Result<Value>, env: Env, cont: Cont}
export const makeAppCont2 = (proc: Result<Value>, env: Env, cont: Cont): AppCont2 => ({tag: "AppCont2", proc: proc, env: env, cont: cont});
export const isAppCont2 = (x: any): x is AppCont2 => x.tag === "AppCont2";

export type ExpsCont2 = {tag: "ExpsCont2", firstVal: Result<Value>, cont: ContArray}
export const makeExpsCont2 = (firstVal: Result<Value>, cont: ContArray): ExpsCont2 => ({tag: "ExpsCont2", firstVal: firstVal, cont: cont});
export const isExpsCont2 = (x: any): x is ExpsCont2 => x.tag === "ExpsCont2";

export type DefCont = {tag: "DefCont", exp: DefineExp, exps: Exp[], cont: Cont}
export const makeDefCont = (exp: DefineExp, exps: Exp[], cont: Cont): DefCont => ({tag: "DefCont", exp: exp, exps: exps, cont: cont});
export const isDefCont = (x: any): x is DefCont => x.tag === "DefCont";

const applyCont = (cont: Cont, val: Result<Value>): Result<Value> => 
    isIfCont(cont) ? applyIfCont(cont, val) :
    isFirstCont(cont) ? applyFirstCont(cont, val) :
    isSetCont(cont) ? applySetCont(cont, val) :
    isAppCont1(cont) ? applyAppCont1(cont, val) :
    isExpsCont1(cont) ? applyExpsCont1(cont, val) :
    isDefCont(cont) ? applyDefCont(cont, val) :
    isTopCont(cont) ? applyTopCont(cont, val) :
    cont;

const applyContArray = (cont: ContArray, val: Result<Value[]>): Result<Value> =>
    isLetCont(cont) ? applyLetCont(cont, val) :
    isLetrecCont(cont) ? applyLetrecCont(cont, val) :
    isAppCont2(cont) ? applyAppCont2(cont, val) :
    isExpsCont2(cont) ? applyExpsCont2(cont, val) :
    cont;

const applyTopCont = (cont: TopCont, val: Result<Value>): Result<Value> => {
    either(val, (v: Value) => console.log(valueToString(v)), console.error);
    return val;
}

const applyIfCont = (cont: IfCont, testVal: Result<Value>): Result<Value> =>
    bind(testVal, (test: Value) => isTrueValue(test) ? evalCont(cont.exp.then, cont.env, cont.cont) :
                                   evalCont(cont.exp.alt, cont.env, cont.cont));

const applyLetCont = (cont: LetCont, vals: Result<Value[]>): Result<Value> =>
    bind(vals, (vals: Value[]) => evalSequence(cont.exp.body, makeExtEnv(letVars(cont.exp), vals, cont.env), cont.cont));

export const applyFirstCont = (cont: FirstCont, firstVal: Result<Value>): Result<Value> =>
    bind(firstVal, _ => evalSequence(cont.exps, cont.env, cont.cont));

export const applyLetrecCont = (cont: LetrecCont, cvals: Result<Value[]>): Result<Value> =>
    bind(cvals, (cvals: Value[]) => { zipWith((bdg, cval) => setFBinding(bdg, cval), cont.env.frame.fbindings, cvals); 
                                      return evalSequence(cont.exp.body, cont.env, cont.cont); });

export const applySetCont = (cont: SetCont, rhsVal: Result<Value>): Result<Value> =>
    bind(rhsVal,
         (rhs: Value) => bind(applyEnvBdg(cont.env, cont.exp.var.var),
                              (bdg: FBinding) => { setFBinding(bdg, rhs);
                                                   return applyCont(cont.cont, makeOk(undefined)); }));

export const applyAppCont1 = (cont: AppCont1, proc: Result<Value>): Result<Value> =>
    evalExps(cont.exp.rands, cont.env, makeAppCont2(proc, cont.env, cont.cont));

export const applyAppCont2 = (cont: AppCont2, args: Result<Value[]>): Result<Value> =>
    bind(cont.proc, (proc: Value) =>
        bind(args, (args: Value[]) =>
            applyProcedure(proc, args, cont.cont)));

export const applyExpsCont1 = (cont: ExpsCont1, firstVal: Result<Value>): Result<Value> =>
    evalExps(cont.exps, cont.env, makeExpsCont2(firstVal, cont.cont));

export const applyExpsCont2 = (cont: ExpsCont2, restVals: Result<Value[]>): Result<Value> =>
    bind(cont.firstVal, (first: Value) =>
        bind(restVals, (rest: Value[]) =>
            applyContArray(cont.cont, makeOk([first, ...rest]))));

export const applyDefCont = (cont: DefCont, rhsVal: Result<Value>): Result<Value> =>
    bind(rhsVal, (rhs: Value) => { globalEnvAddBinding(cont.exp.var.var, rhs);
                                   return evalSequence(cont.exps, theGlobalEnv, cont.cont); });

// ========================================================
// Eval functions

export const evalCont = (exp: CExp, env: Env, cont: Cont): Result<Value> =>
    isNumExp(exp) ? applyCont(cont, makeOk(exp.val)) :
    isBoolExp(exp) ? applyCont(cont, makeOk(exp.val)) :
    isStrExp(exp) ? applyCont(cont, makeOk(exp.val)) :
    isPrimOp(exp) ? applyCont(cont, makeOk(exp)) :
    isVarRef(exp) ? applyCont(cont, applyEnv(env, exp.var)) :
    isLitExp(exp) ? applyCont(cont, makeOk(exp.val)) :
    isIfExp(exp) ? evalIf(exp, env, cont) :
    isProcExp(exp) ? evalProc(exp, env, cont) :
    isLetExp(exp) ? evalLet(exp, env, cont) :
    isLetrecExp(exp) ? evalLetrec(exp, env, cont) :
    isSetExp(exp) ? evalSet(exp, env, cont) :
    isAppExp(exp) ? evalApp(exp, env, cont) :
    exp;

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.test, env, makeIfCont(exp, env, cont));

const evalProc = (exp: ProcExp, env: Env, cont: Cont): Result<Value> =>
    applyCont(cont, makeOk(makeClosure(exp.args, exp.body, env)));

// Return the vals (rhs) of the bindings of a let expression
const letVals = (exp: LetExp | LetrecExp): CExp[] =>
    map((b) => b.val, exp.bindings);

// Return the vars (lhs) of the bindings of a let expression
const letVars = (exp: LetExp | LetrecExp): string[] =>
    map((b) => b.var.var, exp.bindings);

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env, cont: Cont): Result<Value> =>
    evalExps(letVals(exp), env, makeLetCont(exp, env, cont));

// LETREC: Direct evaluation rule without syntax expansion
// 1. extend the env with vars initialized to void (temporary value)
// 2. compute the vals in the new extended env
// 3. update the bindings of the vars to the computed vals
// 4. compute body in extended env
export const evalLetrec = (exp: LetrecExp, env: Env, cont: Cont): Result<Value> => {
    const vars = letVars(exp);
    const vals = letVals(exp);
    const extEnv = makeExtEnv(vars, repeat(undefined, vars.length), env);
    // Compute the vals in the extended env
    return evalExps(vals, extEnv, makeLetrecCont(exp, extEnv, cont));
}

// L4-eval-box: Handling of mutation with set!
export const evalSet = (exp: SetExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.val, env, makeSetCont(exp, env, cont));

// L4 evalApp
export const evalApp = (exp: AppExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.rator, env, makeAppCont1(exp, env, cont));

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
export const applyProcedure = (proc: Value, args: Value[], cont: Cont): Result<Value> =>
    isPrimOp(proc) ? applyCont(cont, applyPrimitive(proc, args)) :
    isClosure(proc) ? applyClosure(proc, args, cont) :
    applyCont(cont, makeFailure(`Bad procedure ${format(proc)}`));

export const applyClosure = (proc: Closure, args: Value[], cont: Cont): Result<Value> => {
    const vars = map((v: VarDecl) => v.var, proc.params);
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env), cont);
}

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Result<Value> =>
    isNonEmptyList<Exp>(exps) ? evalSequenceFR(first(exps), rest(exps), env, cont) :
    applyCont(cont, makeFailure("Empty Sequence"));

const evalSequenceFR = (exp: Exp, exps: Exp[], env: Env, cont: Cont): Result<Value> =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, makeFirstCont(exps, env, cont));

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
const evalDefineExps = (exp: DefineExp, exps: Exp[], cont: Cont): Result<Value> =>
    evalCont(exp.val, theGlobalEnv, makeDefCont(exp, exps, cont));

// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Result<Value> =>
    isNonEmptyList<Exp>(exps) ? evalExpsFR(first(exps), rest(exps), env, cont) :
    applyContArray(cont, makeOk([]));

const evalExpsFR = (exp: Exp, exps: Exp[], env: Env, cont: ContArray): Result<Value> =>
    isDefineExp(exp) ? applyContArray(cont, bind(unparse(exp), e => makeFailure(`Unexpected define: ${format(e)}`))) :
    evalCont(exp, env, makeExpsCont1(exps, env, cont));

// Evaluate a program
// Main program
export const evalProgram = (program: Program): Result<Value> =>
    evalSequence(program.exps, theGlobalEnv, makeTopCont());

export const evalParse = (s: string): Result<Value> =>
    bind(bind(p(s), parseL5Exp), (exp: Exp) => evalSequence([exp], theGlobalEnv, makeTopCont()));
