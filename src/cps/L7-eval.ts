// L7-eval: CPS version of L5 with concrete data-structure continuations

import { map, repeat, zipWith } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         ProcExp, Program, SetExp, VarDecl } from '../L5/L5-ast';
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { parseL5Exp, unparse } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, ExtEnv } from "../L5/L5-env";
import { isClosure, makeClosure, Closure, Value, valueToString } from "../L5/L5-value";
import { isEmpty, first, rest } from '../shared/list';
import { applyPrimitive } from "../L5/evalPrimitive";
import { Result, makeOk, makeFailure, bind, either } from "../shared/result";
import { parse as p } from "../shared/parser";

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
             test => isTrueValue(test) ? evalCont(exp.then, env, cont) : evalCont(exp.alt, env, cont));


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

const makeLetCont = (exp: LetExp, env: Env, cont: Cont) =>
    (vals: Result<Value[]>) =>
        bind(vals,
             (values: Value[]) => evalSequence(exp.body, makeExtEnv(letVars(exp), values, env), cont));

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Result<Value> =>
    isEmpty(exps) ? applyCont(cont, makeFailure("Empty Sequence")) :
    evalSequenceFR(first(exps), rest(exps), env, cont);

const evalSequenceFR = (exp: Exp, exps: Exp[], env: Env, cont: Cont): Result<Value> =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, makeFirstCont(exps, env, cont));

export const makeFirstCont = (exps: Exp[], env: Env, cont: Cont) =>
    (firstVal: Result<Value>) => bind(firstVal, _ => evalSequence(exps, env, cont));

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
export const evalDefineExps = (exp: DefineExp, exps: Exp[], cont: Cont): Result<Value> =>
    evalCont(exp.val, theGlobalEnv, makeDefCont(exp, exps, cont));

export const makeDefCont = (exp: DefineExp, exps: Exp[], cont: Cont) =>
    (rhsVal: Result<Value>) =>
        bind(rhsVal, rhs => { globalEnvAddBinding(exp.var.var, rhs);
                              return evalSequence(exps, theGlobalEnv, cont); });

// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Result<Value> =>
    isEmpty(exps) ? applyContArray(cont, makeOk([])) :
    evalExpsFR(first(exps), rest(exps), env, cont)

const evalExpsFR = (exp: Exp, exps: Exp[], env: Env, cont: ContArray): Result<Value> =>
    isDefineExp(exp) ? applyContArray(cont, bind(unparse(exp), e => makeFailure(`Unexpected define: ${JSON.stringify(e, null, 2)}`))) :
    evalCont(exp, env, makeExpsCont1(exps, env, cont));

const makeExpsCont1 = (exps: Exp[], env: Env, cont: ContArray): Cont =>
    (firstVal: Result<Value>) => bind(firstVal, first => evalExps(exps, env, makeExpsCont2(first, cont)));

const makeExpsCont2 = (firstVal: Value, cont: ContArray): ContArray =>
    (restVals: Result<Value[]>) => bind(restVals, rest => applyContArray(cont, makeOk([firstVal, ...rest])));

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
        bind(cvals, vals => { zipWith((bdg, val) => setFBinding(bdg, val), extEnv.frame.fbindings, vals);
                              return evalSequence(exp.body, extEnv, cont); });

// L4-eval-box: Handling of mutation with set!
export const evalSet = (exp: SetExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.val, env, makeSetCont(exp, env, cont));

export const makeSetCont = (exp: SetExp, env: Env, cont: Cont) =>
    (rhsVal: Result<Value>) =>
        bind(rhsVal,
             rhs => bind(applyEnvBdg(env, exp.var.var),
                         bdg => { setFBinding(bdg, rhs);
                                  return applyCont(cont, makeOk(undefined))}));

const evalApp = (exp: AppExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.rator, env,
        (proc) => evalExps(exp.rands, env,
                    (args) => applyProcedure(proc, args, cont)));

const applyProcedure = (proc: Result<Value>, args: Result<Value[]>, cont: Cont): Result<Value> =>
    bind(proc, (proc: Value) =>
        bind(args, (args: Value[]) =>
            isPrimOp(proc) ? applyCont(cont, applyPrimitive(proc, args)) :
            isClosure(proc) ? applyClosure(proc, args, cont) :
            applyCont(cont, makeFailure(`Bad procedure: ${JSON.stringify(proc, null, 2)}`))));

const applyClosure = (proc: Closure, args: Value[], cont: Cont): Result<Value> => {
    const vars = map((v: VarDecl) => v.var, proc.params);
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env), cont);
}

// Final continuation
export const topCont: Cont = (val) => {
    either(val, v => console.log(valueToString(v)), console.error);
    return val;
}

// Evaluate a program
// Main program
export const evalProgram = (program: Program): Result<Value> =>
    evalSequence(program.exps, theGlobalEnv, topCont);

export const evalParse = (s: string): Result<Value> =>
    bind(bind(p(s), parseL5Exp), exp => evalSequence([exp], theGlobalEnv, topCont));
