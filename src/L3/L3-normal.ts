// ========================================================
// L3 normal eval
import { Sexp } from "s-expression";
import { map } from "ramda";
import { CExp, Exp, IfExp, Program, parseL3Exp } from "./L3-ast";
import { isAppExp, isBoolExp, isCExp, isDefineExp, isIfExp, isLitExp, isNumExp,
         isPrimOp, isProcExp, isStrExp, isVarRef } from "./L3-ast";
import { applyEnv, makeEmptyEnv, makeEnv, Env } from './L3-env';
import { isTrueValue } from "./L3-eval";
import { applyPrimitive } from "./evalPrimitive";
import { renameExps, substitute } from "./substitute";
import { isClosure, makeClosure, Value } from "./L3-value";
import { first, rest, isEmpty } from '../shared/list';
import { Result, makeOk, makeFailure, bind, mapResult } from "../shared/result";
import { parse as p } from "../shared/parser";

/*
Purpose: Evaluate an L3 expression with normal-eval algorithm
Signature: L3-normal-eval(exp,env)
Type: CExp * Env => Value
*/
export const L3normalEval = (exp: CExp, env: Env): Result<Value> =>
    isBoolExp(exp) ? makeOk(exp.val) :
    isNumExp(exp) ? makeOk(exp.val) :
    isStrExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isLitExp(exp) ? makeOk(exp.val) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? makeOk(makeClosure(exp.args, exp.body)) :
    // This is the difference between applicative-eval and normal-eval
    // Substitute the arguments into the body without evaluating them first.
    isAppExp(exp) ? bind(L3normalEval(exp.rator, env), proc => L3normalApplyProc(proc, exp.rands, env)) :
    makeFailure(`Bad ast: ${exp}`);

const evalIf = (exp: IfExp, env: Env): Result<Value> =>
    bind(L3normalEval(exp.test, env),
         test => isTrueValue(test) ? L3normalEval(exp.then, env) : L3normalEval(exp.alt, env));

/*
===========================================================
Normal Order Application handling

Purpose: Apply a procedure to NON evaluated arguments.
Signature: L3-normalApplyProcedure(proc, args)
Pre-conditions: proc must be a prim-op or a closure value
*/
const L3normalApplyProc = (proc: Value, args: CExp[], env: Env): Result<Value> => {
    if (isPrimOp(proc)) {
        const argVals: Result<Value[]> = mapResult((arg) => L3normalEval(arg, env), args);
        return bind(argVals, (args: Value[]) => applyPrimitive(proc, args));
    } else if (isClosure(proc)) {
        // Substitute non-evaluated args into the body of the closure
        const vars = map((p) => p.var, proc.params);
        const body = renameExps(proc.body);
        return L3normalEvalSeq(substitute(body, vars, args), env);
    } else {
        return makeFailure(`Bad proc applied ${proc}`);
    }
};

/*
Purpose: Evaluate a sequence of expressions
Signature: L3-normal-eval-sequence(exps, env)
Type: [List(CExp) * Env -> Value]
Pre-conditions: exps is not empty
*/
const L3normalEvalSeq = (exps: CExp[], env: Env): Result<Value> => {
    if (isEmpty(rest(exps)))
        return L3normalEval(first(exps), env);
    else {
        L3normalEval(first(exps), env);
        return L3normalEvalSeq(rest(exps), env);
    }
};

/*
Purpose: evaluate a program made up of a sequence of expressions. (Same as in L1)
When def-exp expressions are executed, thread an updated env to the continuation.
For other expressions (that have no side-effect), execute the expressions sequentially.
Signature: L3normalEvalProgram(program)
Type: [Program -> Value]
*/
export const L3normalEvalProgram = (program: Program): Result<Value> =>
    evalExps(program.exps, makeEmptyEnv());

// Evaluate a sequence of expressions (in a program)
export const evalExps = (exps: Exp[], env: Env): Result<Value> =>
    isEmpty(exps) ? makeFailure("Empty program") :
    isDefineExp(first(exps)) ? evalDefineExps(first(exps), rest(exps), env) :
    evalCExps(first(exps), rest(exps), env);
    
const evalCExps = (exp1: Exp, exps: Exp[], env: Env): Result<Value> =>
    isCExp(exp1) && isEmpty(exps) ? L3normalEval(exp1, env) :
    isCExp(exp1) ? bind(L3normalEval(exp1, env), _ => evalExps(exps, env)) :
    makeFailure("Never");
    
// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: Exp, exps: Exp[], env: Env): Result<Value> =>
    isDefineExp(def) ? bind(L3normalEval(def.val, env),
                            (rhs: Value) => evalExps(exps, makeEnv(def.var.var, rhs, env))) :
    makeFailure("Unexpected " + def);

export const evalNormalProgram = (program: Program): Result<Value> =>
    evalExps(program.exps, makeEmptyEnv());

export const evalNormalParse = (s: string): Result<Value> =>
    bind(p(s),
         (parsed: Sexp) => bind(parseL3Exp(parsed),
                                (exp: Exp) => evalExps([exp], makeEmptyEnv())));
