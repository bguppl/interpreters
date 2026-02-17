// L7-eval: CPS version of L5 with concrete data-structure continuations

import { map, repeat, zipWith } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         ProcExp, Program, SetExp, parseL5Exp } from '../L5/L5-ast';
import { VarDecl } from "../L5/L5-ast";
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { unparse } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, ExtEnv, FBinding } from "../L5/L5-env";
import { isClosure, makeClosure, Closure, Value, valueToString } from "../L5/L5-value";
import { isEmpty, first, rest, isNonEmptyList } from '../shared/list';
import { Result, makeFailure, makeOk, bind, either } from "../shared/result";
import { applyPrimitive } from "../L5/evalPrimitive";
import { parse as p } from "../shared/parser";
import { format } from "../shared/format";

// ========================================================
// Continuation datatype
type Cont = (res: Result<Value>) => Result<Value>;
type ContArray = (results: Result<Value[]>) => Result<Value>;

const applyCont = (cont: Cont, val: Result<Value>) => cont(val);
const applyContArray = (cont: ContArray, val: Result<Value[]>) => cont(val);

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

const makeIfCont = (exp: IfExp, env: Env, cont: Cont) =>
    (testVal: Result<Value>) =>
        bind(testVal,
             (test: Value) => isTrueValue(test) ? evalCont(exp.then, env, cont) :
                              evalCont(exp.alt, env, cont));

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

const makeLetCont = (exp: LetExp, env: Env, cont: Cont): ContArray =>
    (vals: Result<Value[]>) =>
        bind(vals,
             (values: Value[]) => evalSequence(exp.body, makeExtEnv(letVars(exp), values, env), cont));

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Result<Value> =>
    isNonEmptyList<Exp>(exps) ? evalSequenceFR(first(exps), rest(exps), env, cont) :
    applyCont(cont, makeFailure("Empty Sequence"));

const evalSequenceFR = (exp: Exp, exps: Exp[], env: Env, cont: Cont): Result<Value> =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, makeEvalSequenceCont(exps, env, cont));
        
const makeEvalSequenceCont = (exps: Exp[], env: Env, cont: Cont): Cont =>
    (firstVal: Result<Value>) =>
        bind(firstVal, _ => evalSequence(exps, env, cont));

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
const evalDefineExps = (exp: DefineExp, exps: Exp[], cont: Cont): Result<Value> =>
    evalCont(exp.val, theGlobalEnv, makeEvalDefineExpsCont(exp, exps, cont));

const makeEvalDefineExpsCont = (exp: DefineExp, exps: Exp[], cont: Cont): Cont =>
    (rhsVal: Result<Value>) =>
        bind(rhsVal, (rhs: Value) => { globalEnvAddBinding(exp.var.var, rhs);
                                       return evalSequence(exps, theGlobalEnv, cont); });


// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Result<Value> =>
    isNonEmptyList<Exp>(exps) ? evalExpsFR(first(exps), rest(exps), env, cont) :
    applyContArray(cont, makeOk([]));

const evalExpsFR = (exp: Exp, exps: Exp[], env: Env, cont: ContArray): Result<Value> =>
    isDefineExp(exp) ? applyContArray(cont, bind(unparse(exp), e => makeFailure(`Unexpected define: ${format(e)}`))) :
    evalCont(exp, env, makeExpsCont1(exps, env, cont));


export const makeExpsCont1 = (exps: Exp[], env: Env, cont: ContArray): Cont =>
    (firstVal: Result<Value>) => 
        bind(firstVal, (first: Value) => evalExps(exps, env, makeExpsCont2(first, cont)));

export const makeExpsCont2 = (firstVal: Value, cont: ContArray): ContArray =>
    (restVals: Result<Value[]>) =>
        bind(restVals, (rest: Value[]) => applyContArray(cont, makeOk([firstVal, ...rest])));


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

export const makeLetrecCont = (exp: LetrecExp, extEnv: ExtEnv, cont: Cont): ContArray =>
    (cvals: Result<Value[]>) =>
        bind(cvals, (vals: Value[]) => { zipWith((bdg, cval) => setFBinding(bdg, cval), extEnv.frame.fbindings, vals);
                                         return evalSequence(exp.body, extEnv, cont); });

// L4-eval-box: Handling of mutation with set!
export const evalSet = (exp: SetExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.val, env, makeSetCont(exp, env, cont));

export const makeSetCont = (exp: SetExp, env: Env, cont: Cont): Cont =>
    (rhsVal: Result<Value>) =>
        bind(rhsVal,
             (rhs: Value) => bind(applyEnvBdg(env, exp.var.var),
                                  (bdg: FBinding) => { setFBinding(bdg, rhs);
                                                       return applyCont(cont, makeOk(undefined)); }));

// L4 evalApp
export const evalApp = (exp: AppExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.rator, env, makeAppCont1(exp, env, cont));

export const makeAppCont1 = (exp: AppExp, env: Env, cont: Cont): Cont =>
    (proc: Result<Value>) => 
        bind(proc, (proc: Value) => evalExps(exp.rands, env, makeAppCont2(proc, env, cont)));

export const makeAppCont2 = (proc: Value, env: Env, cont: Cont): ContArray =>
    (args: Result<Value[]>) => bind(args, (args: Value[]) => applyProcedure(proc, args, cont));

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

// Final continuation
export const topCont: Cont = (val: Result<Value>) => { 
    either(val, (v: Value) => console.log(valueToString(v)), console.error);
    return val;
}

// Evaluate a program
// Main program
export const evalProgram = (program: Program): Result<Value> =>
    evalSequence(program.exps, theGlobalEnv, topCont);

export const evalParse = (s: string): Result<Value> =>
    bind(bind(p(s), parseL5Exp), (exp: Exp) => evalSequence([exp], theGlobalEnv, topCont));
