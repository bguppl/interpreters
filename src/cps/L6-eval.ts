// L6-eval: CPS version of L5

import { map, repeat, zipWith, identity } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         ProcExp, Program, SetExp, VarDecl } from '../L5/L5-ast';
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { parseL5Exp, unparse } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, FBinding } from "../L5/L5-env";
import { isClosure, makeClosure, Closure, Value } from "../L5/L5-value";
import { applyPrimitive } from "../L5/evalPrimitive";
import { isEmpty, first, rest, isNonEmptyList } from '../shared/list';
import { Result, makeOk, makeFailure, bind } from "../shared/result";
import { parse as p } from "../shared/parser";
import { format } from "../shared/format";

// ========================================================
// Continuation datatype
type Cont = (res: Result<Value>) => Result<Value>;
type ContArray = (results: Result<Value[]>) => Result<Value>;

// ========================================================
// Eval functions

export const evalCont = (exp: CExp, env: Env, cont: Cont): Result<Value> =>
    isNumExp(exp) ? cont(makeOk(exp.val)) :
    isBoolExp(exp) ? cont(makeOk(exp.val)) :
    isStrExp(exp) ? cont(makeOk(exp.val)) :
    isPrimOp(exp) ? cont(makeOk(exp)) :
    isVarRef(exp) ? cont(applyEnv(env, exp.var)) :
    isLitExp(exp) ? cont(makeOk(exp.val)) :
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
    evalCont(exp.test, env,
             (testVal: Result<Value>) => bind(testVal,
                                              (test: Value) => isTrueValue(test) ? evalCont(exp.then, env, cont) :
                                                               evalCont(exp.alt, env, cont)));


const evalProc = (exp: ProcExp, env: Env, cont: Cont): Result<Value> =>
    cont(makeOk(makeClosure(exp.args, exp.body, env)));

// Return the vals (rhs) of the bindings of a let expression
const letVals = (exp: LetExp | LetrecExp): CExp[] =>
    map((b) => b.val, exp.bindings);

// Return the vars (lhs) of the bindings of a let expression
const letVars = (exp: LetExp | LetrecExp): string[] =>
    map((b) => b.var.var, exp.bindings);

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env, cont: Cont): Result<Value> =>
    evalExps(letVals(exp), env,
             (vals: Result<Value[]>) => 
                bind(vals,
                     (values: Value[]) => 
                         evalSequence(exp.body, makeExtEnv(letVars(exp), values, env), 
                                      cont)));

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Result<Value> =>
    isNonEmptyList<Exp>(exps) ? evalSequenceFR(first(exps), rest(exps), env, cont) :
    cont(makeFailure("Empty Sequence"));

const evalSequenceFR = (exp: Exp, exps: Exp[], env: Env, cont: Cont): Result<Value> =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, (firstVal: Result<Value>) => bind(firstVal, _ => evalSequence(exps, env, cont)));

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
const evalDefineExps = (exp: DefineExp, exps: Exp[], cont: Cont): Result<Value> =>
    evalCont(exp.val, theGlobalEnv,
             (rhsVal: Result<Value>) => bind(rhsVal, (rhs: Value) => { globalEnvAddBinding(exp.var.var, rhs);
                                                                       return evalSequence(exps, theGlobalEnv, cont); }));

// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Result<Value> =>
    isNonEmptyList<Exp>(exps) ? evalExpsFR(first(exps), rest(exps), env, cont) :
    cont(makeOk([]));

const evalExpsFR = (exp: Exp, exps: Exp[], env: Env, cont: ContArray): Result<Value> =>
    isDefineExp(exp) ? cont(bind(unparse(exp), up => makeFailure(`Unexpected define: ${format(up)}`))) :
    evalCont(exp, env,
             (firstVal: Result<Value>) =>
                bind(firstVal,
                     (first: Value) => evalExps(exps, env,
                                                (restVals: Result<Value[]>) => bind(restVals,
                                                                                    (rest: Value[]) => cont(makeOk([first, ...rest]))))));

// LETREC: Direct evaluation rule without syntax expansion
// 1. extend the env with vars initialized to void (temporary value)
// 2. compute the vals in the new extended env
// 3. update the bindings of the vars to the computed vals
// 4. compute body in extended env
const evalLetrec = (exp: LetrecExp, env: Env, cont: Cont): Result<Value> => {
    const vars = letVars(exp);
    const vals = letVals(exp);
    const extEnv = makeExtEnv(vars, repeat(undefined, vars.length), env);
    // Compute the vals in the extended env
    return evalExps(vals, extEnv,
                    (cvals: Result<Value[]>) => bind(cvals, (values: Value[]) => { zipWith((bdg, cval) => setFBinding(bdg, cval), extEnv.frame.fbindings, values);
                                                                                   return evalSequence(exp.body, extEnv, cont); }));
}

// Handling of mutation with set!
const evalSet = (exp: SetExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.val, env,
             (rhsVal: Result<Value>) => bind(rhsVal,
                                             (rhs: Value) => bind(applyEnvBdg(env, exp.var.var),
                                                                  (bdg: FBinding) => cont(makeOk(setFBinding(bdg, rhs))))));

const evalApp = (exp: AppExp, env: Env, cont: Cont): Result<Value> =>
    evalCont(exp.rator, env,
             (proc: Result<Value>) => bind(proc,
                                           (p: Value) => evalExps(exp.rands, env,
                                                                  (args: Result<Value[]>) => bind(args,
                                                                                                  (as: Value[]) => applyProcedure(p, as, cont)))));

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
const applyProcedure = (proc: Value, args: Value[], cont: Cont): Result<Value> =>
    isPrimOp(proc) ? cont(applyPrimitive(proc, args)) :
    isClosure(proc) ? applyClosure(proc, args, cont) :
    cont(makeFailure(`Bad procedure ${format(proc)}`));

const applyClosure = (proc: Closure, args: Value[], cont: Cont): Result<Value> => {
    const vars = map((v: VarDecl) => v.var, proc.params);
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env), cont);
}

// Final continuation
// export const topCont: Cont = (val) => { console.log(parsedToString(val)); return val; }
export const topCont: Cont = identity;

// Evaluate a program
// Main program
export const evalProgram = (program: Program): Result<Value> =>
    evalSequence(program.exps, theGlobalEnv, topCont);

export const evalParse = (s: string): Result<Value> =>
    bind(bind(p(s), parseL5Exp), (exp: Exp) => evalSequence([exp], theGlobalEnv, topCont));
