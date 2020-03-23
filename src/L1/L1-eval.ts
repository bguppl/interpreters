// L1-eval.ts

import { filter, map, reduce } from "ramda";
import { first, isEmpty, rest } from "../shared/list";
import { CExp, DefineExp, Exp, PrimOp, Program } from "./L1-ast";
import { hasError, isAppExp, isBoolExp, isCExp, isDefineExp, isError, isNumExp, isPrimOp,
         isProgram, isVarRef } from "./L1-ast";
import { parseL1 } from "./L1-ast";

// ========================================================
// Value type definition
export type Value = number | boolean | PrimOp | Error;

// ========================================================
// Environment data type
export type Env = EmptyEnv | NonEmptyEnv;
export interface EmptyEnv {tag: "EmptyEnv" };
export interface NonEmptyEnv {
    tag: "Env",
    var: string,
    val: Value,
    nextEnv: Env
};
export const makeEmptyEnv = (): EmptyEnv => ({tag: "EmptyEnv"});
export const makeEnv = (v: string, val: Value, env: Env): NonEmptyEnv =>
    ({tag: "Env", var: v, val: val, nextEnv: env});
export const isEmptyEnv = (x: any): x is EmptyEnv => x.tag === "EmptyEnv";
export const isNonEmptyEnv = (x: any): x is NonEmptyEnv => x.tag === "Env";
export const isEnv = (x: any): x is Env => isEmptyEnv(x) || isNonEmptyEnv(x);

const applyEnv = (env: Env, v: string): Value =>
    isEmptyEnv(env) ? Error("var not found " + v) :
    env.var === v ? env.val :
    applyEnv(env.nextEnv, v);

// ========================================================
// Eval functions

const L1applicativeEval = (exp: CExp, env: Env): Value =>
    isError(exp)  ? exp :
    isNumExp(exp) ? exp.val :
    isBoolExp(exp) ? exp.val :
    isPrimOp(exp) ? exp :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isAppExp(exp) ? L1applyProcedure(exp.rator,
                                     map((rand) => L1applicativeEval(rand, env),
                                     exp.rands)) :
    Error("Bad L1 AST " + exp)

const L1applyProcedure = (proc: CExp, args: Value[]): Value =>
    hasError(args) ? Error("Bad argument: " + JSON.stringify(first(filter(isError, args)))) :
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    Error("Bad procedure " + proc)

// @Pre: none of the args is an Error (checked in applyProcedure)
// @@There are type errors which we will address in L3
const applyPrimitive = (proc: PrimOp, args: Value[]): Value =>
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "+" ? reduce((x, y) => x + y, 0, args) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "-" ? reduce((x, y) => x - y, 0, args) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "*" ? reduce((x, y) => x * y, 1, args) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "/" ? reduce((x, y) => x / y, 1, args) :
    proc.op === ">" ? args[0] > args[1] :
    proc.op === "<" ? args[0] < args[1] :
    proc.op === "=" ? args[0] === args[1] :
    proc.op === "not" ? !args[0] :
    Error("Bad primitive op " + proc.op);

// Evaluate a sequence of expressions (in a program)
export const evalExps = (exps: Exp[], env: Env): Value =>
    isEmpty(exps) ? Error("Empty program") :
    evalExpsAux(first(exps), rest(exps), env);

const evalExpsAux = (exp1: Exp, exps: Exp[], env: Env): Value =>
    isDefineExp(exp1) ? evalDefineExps(exp1, exps, env) :
    isEmpty(exps) ? L1applicativeEval(exp1, env) :
    isError(L1applicativeEval(exp1, env)) ? Error("error") :
    evalExps(exps, env);

// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: DefineExp, exps: Exp[], env: Env): Value => {
    let rhs = L1applicativeEval(def.val, env),
        newEnv = makeEnv(def.var.var, rhs, env);
    return evalExps(exps, newEnv);
}

// Main program
export const evalL1program = (program: Program): Value =>
    evalExps(program.exps, makeEmptyEnv());
