// ========================================================
// L3 normal eval

import { map } from "ramda";
import { CExp, Exp, IfExp, Parsed, Program } from "./L3-ast";
import { isAppExp, isBoolExp, isCExp, isDefineExp, isExp, isIfExp, isLitExp, isNumExp,
         isPrimOp, isProcExp, isProgram, isStrExp, isVarRef } from "./L3-ast";
import { parseL3 } from "./L3-ast";
import { applyEnv, makeEmptyEnv, makeEnv, Env } from './L3-env';
import { applyPrimitive, isTrueValue, renameExps, substitute } from "./L3-eval";
import { isClosure, makeClosure, Value } from "./L3-value";
import { getErrorMessages, hasNoError, isError }  from "../shared/error";
import { first, rest, isEmpty } from '../shared/list';

/*
Purpose: Evaluate an L3 expression with normal-eval algorithm
Signature: L3-normal-eval(exp,env)
Type: CExp * Env => Value
*/
export const L3normalEval = (exp: CExp | Error, env: Env): Value | Error =>
    isError(exp) ? exp :
    isBoolExp(exp) ? exp.val :
    isNumExp(exp) ? exp.val :
    isStrExp(exp) ? exp.val :
    isPrimOp(exp) ? exp :
    isLitExp(exp) ? exp.val :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? makeClosure(exp.args, exp.body) :
    // This is the difference between applicative-eval and normal-eval
    // Substitute the arguments into the body without evaluating them first.
    isAppExp(exp) ? L3normalApplyProc(L3normalEval(exp.rator, env), exp.rands, env) :
    Error(`Bad ast: ${exp}`);

const evalIf = (exp: IfExp, env: Env): Value | Error => {
    const test = L3normalEval(exp.test, env);
    return isError(test) ? test :
        isTrueValue(test) ? L3normalEval(exp.then, env) :
        L3normalEval(exp.alt, env);
};

/*
===========================================================
Normal Order Application handling

Purpose: Apply a procedure to NON evaluated arguments.
Signature: L3-normalApplyProcedure(proc, args)
Pre-conditions: proc must be a prim-op or a closure value
*/
const L3normalApplyProc = (proc: Value | Error, args: CExp[], env: Env): Value | Error => {
    if (isError(proc)) {
        return proc;
    } else if (isPrimOp(proc)) {
        const argVals: Array<Value | Error> = map((arg) => L3normalEval(arg, env), args);
        if (hasNoError(argVals))
            return applyPrimitive(proc, argVals);
        else
            return Error(getErrorMessages(argVals));
    } else if (isClosure(proc)) {
        // Substitute non-evaluated args into the body of the closure
        const vars = map((p) => p.var, proc.params);
        const body = renameExps(proc.body);
        return L3normalEvalSeq(substitute(body, vars, args), env);
    } else {
        return Error(`Bad proc applied ${proc}`);
    }
};

/*
Purpose: Evaluate a sequence of expressions
Signature: L3-normal-eval-sequence(exps, env)
Type: [List(CExp) * Env -> Value]
Pre-conditions: exps is not empty
*/
const L3normalEvalSeq = (exps: CExp[], env: Env): Value | Error => {
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
export const L3normalEvalProgram = (program: Program): Value | Error =>
    evalExps(program.exps, makeEmptyEnv());

// Evaluate a sequence of expressions (in a program)
export const evalExps = (exps: Exp[], env: Env): Value | Error =>
    isEmpty(exps) ? Error("Empty program") :
    isDefineExp(first(exps)) ? evalDefineExps(first(exps), rest(exps), env) :
    evalCExps(first(exps), rest(exps), env);
    
const evalCExps = (exp1: Exp, exps: Exp[], env: Env): Value | Error =>
    isCExp(exp1) && isEmpty(exps) ? L3normalEval(exp1, env) :
    isCExp(exp1) ? (isError(L3normalEval(exp1, env)) ? Error("error") :
        evalExps(exps, env)) :
    Error("Never");
    
// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: Exp, exps: Exp[], env: Env): Value | Error => {
    if (isDefineExp(def)) {
        let rhs = L3normalEval(def.val, env);
        if (isError(rhs))
            return rhs;
        else {
            let newEnv = makeEnv(def.var.var, rhs, env);
            return evalExps(exps, newEnv);
        }
    } else {
        return Error("unexpected " + def);
    }
}



export const evalNormalParse = (s: string): Value | Error => {
    let ast: Parsed | Error = parseL3(s);
    if (isProgram(ast)) {
        return L3normalEvalProgram(ast);
    } else if (isExp(ast)) {
        return evalExps([ast], makeEmptyEnv());
    } else {
        return ast;
    }
}

