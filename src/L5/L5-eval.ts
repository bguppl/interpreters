// L5-eval-box
import * as E from "fp-ts/Either";
import { map, replicate, zipWith } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { CExp, Exp, IfExp, LetrecExp, LetExp, ProcExp, Program, SetExp, isCExp } from './L5-ast';
import { Binding, VarDecl } from "./L5-ast";
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "./L5-ast";
import { parseL5Exp } from "./L5-ast";
import { isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isSetExp } from "./L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, FBinding } from "./L5-env";
import { isClosure, makeClosure, Closure, Value } from "./L5-value";
import { isEmpty, first, rest } from '../shared/list';
import { parse as p } from "../shared/parser";
import { applyPrimitive } from "./evalPrimitive";

// ========================================================
// Eval functions

export const applicativeEval = (exp: CExp, env: Env): E.Either<string, Value> =>
    isNumExp(exp) ? E.of(exp.val) :
    isBoolExp(exp) ? E.of(exp.val) :
    isStrExp(exp) ? E.of(exp.val) :
    isPrimOp(exp) ? E.of(exp) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isLitExp(exp) ? E.of(exp.val) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? evalProc(exp, env) :
    isLetExp(exp) ? evalLet(exp, env) :
    isLetrecExp(exp) ? evalLetrec(exp, env) :
    isSetExp(exp) ? evalSet(exp, env) :
    isAppExp(exp) ? pipe(
        applicativeEval(exp.rator, env),
        E.chain(proc => pipe(
            exp.rands,
            E.traverseArray(rand => applicativeEval(rand, env)),
            E.chain(args => applyProcedure(proc, args))
        ))
    ) :
    exp;

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env): E.Either<string, Value> =>
    pipe(
        applicativeEval(exp.test, env),
        E.chain(test => isTrueValue(test) ? applicativeEval(exp.then, env) : applicativeEval(exp.alt, env))
    );

const evalProc = (exp: ProcExp, env: Env): E.Either<string, Closure> =>
    E.of(makeClosure(exp.args, exp.body, env));

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
const applyProcedure = (proc: Value, args: readonly Value[]): E.Either<string, Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args) :
    E.left(`Bad procedure ${JSON.stringify(proc)}`);

const applyClosure = (proc: Closure, args: readonly Value[]): E.Either<string, Value> => {
    const vars = pipe(proc.params, map(v => v.var));
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env));
}

// Evaluate a sequence of expressions (in a program)
export const evalSequence = (seq: readonly Exp[], env: Env): E.Either<string, Value> =>
    isEmpty(seq) ? E.left("Empty sequence") :
    evalCExps(first(seq), rest(seq), env);
    
const evalCExps = (first: Exp, rest: readonly Exp[], env: Env): E.Either<string, Value> =>
    isDefineExp(first) ? evalDefineExps(first, rest) :
    isCExp(first) && isEmpty(rest) ? applicativeEval(first, env) :
    isCExp(first) ? pipe(applicativeEval(first, env), E.chain(_ => evalSequence(rest, env))) :
    first;
    
// define always updates theGlobalEnv
// We also only expect defineExps at the top level.
// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: Exp, exps: readonly Exp[]): E.Either<string, Value> =>
    isDefineExp(def) ? pipe(applicativeEval(def.val, theGlobalEnv), E.chain(rhs => {
        globalEnvAddBinding(def.var.var, rhs);
        return evalSequence(exps, theGlobalEnv);
    })) :
    E.left("Unexpected " + def);

// Main program
export const evalProgram = (program: Program): E.Either<string, Value> =>
    evalSequence(program.exps, theGlobalEnv);

export const evalParse = (s: string): E.Either<string, Value> =>
    pipe(
        p(s),
        E.chain(parseL5Exp),
        E.chain(exp => evalSequence([exp], theGlobalEnv))
    );

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env): E.Either<string, Value> => {
    const vals = pipe(
        exp.bindings,
        map(b => b.val),
        E.traverseArray(v => applicativeEval(v, env))
    );
    const vars = pipe(exp.bindings, map(b => b.var.var));
    return pipe(vals, E.chain(vals => evalSequence(exp.body, makeExtEnv(vars, vals, env))));
}

// LETREC: Direct evaluation rule without syntax expansion
// 1. extend the env with vars initialized to void (temporary value)
// 2. compute the vals in the new extended env
// 3. update the bindings of the vars to the computed vals
// 4. compute body in extended env
const evalLetrec = (exp: LetrecExp, env: Env): E.Either<string, Value> => {
    const vars = pipe(exp.bindings, map(b => b.var.var));
    const vals = pipe(exp.bindings, map(b => b.val));
    const extEnv = makeExtEnv(vars, replicate(vars.length, undefined), env);
    // @@ Compute the vals in the extended env
    const cvalsResult = pipe(vals, E.traverseArray(v => applicativeEval(v, extEnv)));
    return pipe(
        cvalsResult,
        E.map(cvals => zipWith(extEnv.frame.fbindings, cvals, setFBinding)),
        E.chain(_ => evalSequence(exp.body, extEnv))
    );
};

// L4-eval-box: Handling of mutation with set!
const evalSet = (exp: SetExp, env: Env): E.Either<string, void> =>
    pipe(
        applicativeEval(exp.val, env),
        E.chain(val => pipe(
            applyEnvBdg(env, exp.var.var),
            E.map(bdg => setFBinding(bdg, val))
        ))
    );
