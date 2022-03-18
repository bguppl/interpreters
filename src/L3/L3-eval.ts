// L3-eval.ts
import * as E from "fp-ts/Either";
import { map } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { isCExp, isLetExp } from "./L3-ast";
import { BoolExp, CExp, Exp, IfExp, LitExp, NumExp,
         PrimOp, ProcExp, Program, StrExp, VarDecl } from "./L3-ast";
import { isAppExp, isBoolExp, isDefineExp, isIfExp, isLitExp, isNumExp,
             isPrimOp, isProcExp, isStrExp, isVarRef } from "./L3-ast";
import { makeBoolExp, makeLitExp, makeNumExp, makeProcExp, makeStrExp } from "./L3-ast";
import { parseL3Exp } from "./L3-ast";
import { applyEnv, makeEmptyEnv, makeEnv, Env } from "./L3-env";
import { isClosure, makeClosure, Closure, Value } from "./L3-value";
import { first, rest, isEmpty } from '../shared/list';
import { isBoolean, isNumber, isString } from "../shared/type-predicates";
import { renameExps, substitute } from "./substitute";
import { applyPrimitive } from "./evalPrimitive";
import { parse as p } from "../shared/parser";

// ========================================================
// Eval functions

const L3applicativeEval = (exp: CExp, env: Env): E.Either<string, Value> =>
    isNumExp(exp) ? E.of(exp.val) : 
    isBoolExp(exp) ? E.of(exp.val) :
    isStrExp(exp) ? E.of(exp.val) :
    isPrimOp(exp) ? E.of(exp) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isLitExp(exp) ? E.of(exp.val) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? evalProc(exp, env) :
    isAppExp(exp) ? pipe(
        L3applicativeEval(exp.rator, env),
        E.chain(rator => pipe(
            exp.rands,
            E.traverseArray(rand => L3applicativeEval(rand, env)),
            E.chain(rands => L3applyProcedure(rator, rands, env))
        ))
    ) :
    isLetExp(exp) ? E.left('"let" not supported (yet)') :
    exp;

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env): E.Either<string, Value> =>
    pipe(
        L3applicativeEval(exp.test, env),
        E.chain(test => isTrueValue(test) ? L3applicativeEval(exp.then, env) : L3applicativeEval(exp.alt, env))
    );

const evalProc = (exp: ProcExp, env: Env): E.Either<string, Closure> =>
    E.of(makeClosure(exp.args, exp.body));

const L3applyProcedure = (proc: Value, args: readonly Value[], env: Env): E.Either<string, Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args, env) :
    E.left("Bad procedure " + JSON.stringify(proc));

// Applications are computed by substituting computed
// values into the body of the closure.
// To make the types fit - computed values of params must be
// turned back in Literal Expressions that eval to the computed value.
const valueToLitExp = (v: Value): NumExp | BoolExp | StrExp | LitExp | PrimOp | ProcExp =>
    isNumber(v) ? makeNumExp(v) :
    isBoolean(v) ? makeBoolExp(v) :
    isString(v) ? makeStrExp(v) :
    isPrimOp(v) ? v :
    isClosure(v) ? makeProcExp(v.params, v.body) :
    makeLitExp(v);

const applyClosure = (proc: Closure, args: readonly Value[], env: Env): E.Either<string, Value> => {
    const vars = map((v: VarDecl) => v.var)(proc.params);
    const body = renameExps(proc.body);
    const litArgs = map(valueToLitExp)(args);
    return evalSequence(substitute(body, vars, litArgs), env);
}

// Evaluate a sequence of expressions (in a program)
export const evalSequence = (seq: readonly Exp[], env: Env): E.Either<string, Value> =>
    isEmpty(seq) ? E.left("Empty sequence") :
    isDefineExp(first(seq)) ? evalDefineExps(first(seq), rest(seq), env) :
    evalCExps(first(seq), rest(seq), env);

const evalCExps = (first: Exp, rest: readonly Exp[], env: Env): E.Either<string, Value> =>
    isCExp(first) && isEmpty(rest) ? L3applicativeEval(first, env) :
    isCExp(first) ? pipe(L3applicativeEval(first, env), E.chain(_ => evalSequence(rest, env))) :
    E.left("Never");

// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: Exp, exps: readonly Exp[], env: Env): E.Either<string, Value> =>
    isDefineExp(def) ? pipe(
        L3applicativeEval(def.val, env),
        E.chain(rhs => evalSequence(exps, makeEnv(def.var.var, rhs, env)))
    ) :
    E.left("Unexpected " + def);

// Main program
export const evalL3program = (program: Program): E.Either<string, Value> =>
    evalSequence(program.exps, makeEmptyEnv());

export const evalParse = (s: string): E.Either<string, Value> =>
    pipe(
        p(s),
        E.chain(parseL3Exp),
        E.chain(exp => evalSequence([exp], makeEmptyEnv()))
    )
