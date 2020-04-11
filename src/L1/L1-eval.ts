// L1-eval.ts

import { reduce } from "ramda";
import { first, isEmpty, rest } from "../shared/list";
import { Result, makeOk, makeFailure, bind, mapResult } from "../shared/result";
import { CExp, DefineExp, Exp, PrimOp, Program } from "./L1-ast";
import { isAppExp, isBoolExp, isDefineExp, isNumExp, isPrimOp, isVarRef } from "./L1-ast";

// ========================================================
// Value type definition
export type Value = number | boolean | PrimOp;

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

const applyEnv = (env: Env, v: string): Result<Value> =>
    isEmptyEnv(env) ? makeFailure("var not found " + v) :
    env.var === v ? makeOk(env.val) :
    applyEnv(env.nextEnv, v);

// ========================================================
// Eval functions

const L1applicativeEval = (exp: CExp, env: Env): Result<Value> =>
    isNumExp(exp) ? makeOk(exp.val) :
    isBoolExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isAppExp(exp) ? bind(mapResult((rand: CExp) =>  L1applicativeEval(rand, env),
                                   exp.rands),
                         (rands: Value[]) => L1applyProcedure(exp.rator, rands)) :
    makeFailure("Bad L1 AST " + exp);

const L1applyProcedure = (proc: CExp, args: Value[]): Result<Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    makeFailure("Bad procedure " + proc)

// There are type errors which we will address in L3
const applyPrimitive = (proc: PrimOp, args: Value[]): Result<Value> =>
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "+" ? makeOk(reduce((x, y) => x + y, 0, args)) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "-" ? makeOk(reduce((x, y) => x - y, 0, args)) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "*" ? makeOk(reduce((x, y) => x * y, 1, args)) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "/" ? makeOk(reduce((x, y) => x / y, 1, args)) :
    proc.op === ">" ? makeOk(args[0] > args[1]) :
    proc.op === "<" ? makeOk(args[0] < args[1]) :
    proc.op === "=" ? makeOk(args[0] === args[1]) :
    proc.op === "not" ? makeOk(!args[0]) :
    makeFailure("Bad primitive op " + proc.op);

// Evaluate a sequence of expressions (in a program)
export const evalSequence = (seq: Exp[], env: Env): Result<Value> =>
    isEmpty(seq) ? makeFailure("Empty sequence") :
    evalSequenceFirst(first(seq), rest(seq), env);

const evalSequenceFirst = (first: Exp, rest: Exp[], env: Env): Result<Value> =>
    isDefineExp(first) ? evalDefineExps(first, rest, env) :
    isEmpty(rest) ? L1applicativeEval(first, env) :
    bind(L1applicativeEval(first, env), _ => evalSequence(rest, env));

// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: DefineExp, exps: Exp[], env: Env): Result<Value> =>
    bind(L1applicativeEval(def.val, env),
         (rhs: Value) => evalSequence(exps, makeEnv(def.var.var, rhs, env)));

// Main program
export const evalL1program = (program: Program): Result<Value> =>
    evalSequence(program.exps, makeEmptyEnv());
