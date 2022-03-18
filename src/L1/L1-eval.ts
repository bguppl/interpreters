// L1-eval.ts
import { pipe } from "fp-ts/function";
import * as E from "fp-ts/Either";
import { reduce } from "fp-ts/Array";
import { first, isEmpty, rest } from "../shared/list";
import { CExp, DefineExp, Exp, PrimOp, Program } from "./L1-ast";
import { isAppExp, isBoolExp, isDefineExp, isNumExp, isPrimOp, isVarRef } from "./L1-ast";

// ========================================================
// Value type definition
export type Value = number | boolean | PrimOp;

// ========================================================
// Environment data type
export type Env = EmptyEnv | NonEmptyEnv;
export interface EmptyEnv {tag: "EmptyEnv" }
export interface NonEmptyEnv {
    tag: "Env",
    var: string,
    val: Value,
    nextEnv: Env
}
export const makeEmptyEnv = (): EmptyEnv => ({tag: "EmptyEnv"});
export const makeEnv = (v: string, val: Value, env: Env): NonEmptyEnv =>
    ({tag: "Env", var: v, val: val, nextEnv: env});
export const isEmptyEnv = (x: any): x is EmptyEnv => x.tag === "EmptyEnv";
export const isNonEmptyEnv = (x: any): x is NonEmptyEnv => x.tag === "Env";
export const isEnv = (x: any): x is Env => isEmptyEnv(x) || isNonEmptyEnv(x);

const applyEnv = (env: Env, v: string): E.Either<string, Value> =>
    isEmptyEnv(env) ? E.left("var not found " + v) :
    env.var === v ? E.of(env.val) :
    applyEnv(env.nextEnv, v);

// ========================================================
// Eval functions

const L1applicativeEval = (exp: CExp, env: Env): E.Either<string, Value> =>
    isNumExp(exp) ? E.of(exp.val) :
    isBoolExp(exp) ? E.of(exp.val) :
    isPrimOp(exp) ? E.of(exp) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isAppExp(exp) ? pipe(exp.rands,
                         E.traverseArray(rand => L1applicativeEval(rand, env)),
                         E.chain(rands => L1applyProcedure(exp.rator, rands))) :
    exp;

const L1applyProcedure = (proc: CExp, args: readonly Value[]): E.Either<string, Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    E.left("Bad procedure " + proc);

// There are type errors which we will address in L3
const applyPrimitive = (proc: PrimOp, args: readonly Value[]): E.Either<string, Value> =>
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "+" ? E.of(reduce(0, (x, y) => x + y)(args)) :
    // This implementation is wrong - no type checking and no verification
    // of associativity: what should be (- 1 2 3): (- 1 (- 2 3)) i.e. 2 or (- (- 1 2) 3) i.e. -4
    // proc.op === "-" ? E.of(reduce((x, y) => x - y, 0, args)) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "-" ? E.of(args[0] - args[1]) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "*" ? E.of(reduce(1, (x, y) => x * y)(args)) :
    // This implementation is wrong - no type checking and no verification
    // of associativity: what should be (/ 1 2 3): (/ 1 (/ 2 3)) i.e. 1.5 or (/ (/ 1 2) 3) i.e. 1/6
    // proc.op === "/" ? E.of(reduce((x, y) => x / y, 1, args)) :
    // @ts-ignore: the rhs of an arithmetic operation must be a number
    proc.op === "/" ? E.of(args[0] / args[1]) :
    proc.op === ">" ? E.of(args[0] > args[1]) :
    proc.op === "<" ? E.of(args[0] < args[1]) :
    proc.op === "=" ? E.of(args[0] === args[1]) :
    proc.op === "not" ? E.of(!args[0]) :
    E.left("Bad primitive op " + proc.op);

// Evaluate a sequence of expressions (in a program)
export const evalSequence = (seq: readonly Exp[], env: Env): E.Either<string, Value> =>
    isEmpty(seq) ? E.left("Empty sequence") :
    evalSequenceFirst(first(seq), rest(seq), env);

const evalSequenceFirst = (first: Exp, rest: readonly Exp[], env: Env): E.Either<string, Value> =>
    isDefineExp(first) ? evalDefineExps(first, rest, env) :
    isEmpty(rest) ? L1applicativeEval(first, env) :
    pipe(L1applicativeEval(first, env), E.chain(_ => evalSequence(rest, env)));

// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: DefineExp, exps: readonly Exp[], env: Env): E.Either<string, Value> =>
    pipe(L1applicativeEval(def.val, env), E.chain((rhs: Value) => evalSequence(exps, makeEnv(def.var.var, rhs, env))));

// Main program
export const evalL1program = (program: Program): E.Either<string, Value> =>
    evalSequence(program.exps, makeEmptyEnv());
