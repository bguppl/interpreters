// L4-eval.ts
// Evaluator with Environments model
// Support letrec with recursive environments (RecEnv)
// No support for set! 
import * as E from "fp-ts/Either";
import { map } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { isBoolExp, isCExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef,
         isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp, isProcExp,
         CExp, Exp, IfExp, LetrecExp, LetExp, ProcExp, Program,
         parseL4Exp, isSetExp, DefineExp} from "./L4-ast";
import { applyEnv, makeEmptyEnv, makeExtEnv, makeRecEnv, Env } from "./L4-env";
import { isClosure, makeClosure, Closure, Value } from "./L4-value";
import { applyPrimitive } from "./evalPrimitive";
import { allT, first, rest, isEmpty } from "../shared/list";
import { parse as p } from "../shared/parser";

// ========================================================
// Eval functions

const applicativeEval = (exp: CExp, env: Env): E.Either<string, Value> =>
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
    isAppExp(exp) ? pipe(
        applicativeEval(exp.rator, env),
        E.chain(proc => pipe(
            exp.rands,
            E.traverseArray(rand => applicativeEval(rand, env)),
            E.chain(args => applyProcedure(proc, args))
        ))
    ) :
    isSetExp(exp) ? E.left(`To implement ${exp}`) :
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
    isDefineExp(first) ? evalDefineExps(first, rest, env) :
    isCExp(first) && isEmpty(rest) ? applicativeEval(first, env) :
    isCExp(first) ? pipe(applicativeEval(first, env), E.chain(_ => evalSequence(rest, env))) :
    first;
    
// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: DefineExp, exps: readonly Exp[], env: Env): E.Either<string, Value> =>
    pipe(
        applicativeEval(def.val, env),
        E.chain(rhs => evalSequence(exps, makeExtEnv([def.var.var], [rhs], env)))
    );

// Main program
export const evalProgram = (program: Program): E.Either<string, Value> =>
    evalSequence(program.exps, makeEmptyEnv());

export const evalParse = (s: string): E.Either<string, Value> =>
    pipe(
        p(s),
        E.chain(parseL4Exp),
        E.chain(exp => evalSequence([exp], makeEmptyEnv()))
    );

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env): E.Either<string, Value> => {
    const vals = pipe(exp.bindings, map(b => b.val), E.traverseArray(v => applicativeEval(v, env)));
    const vars = pipe(exp.bindings, map(b => b.var.var));
    return pipe(
        vals,
        E.chain(vals => evalSequence(exp.body, makeExtEnv(vars, vals, env)))
    );
}

// LETREC: Direct evaluation rule without syntax expansion
// prepare the values as a RecEnv, eval the body (no eval needed for the vals).
const evalLetrec = (exp: LetrecExp, env: Env): E.Either<string, Value> => {
    const vars = pipe(exp.bindings, map(b => b.var.var));
    const vals = pipe(exp.bindings, map(b => b.val));
    if (allT(isProcExp, vals)) {
        const paramss = pipe(vals, map(v => v.args));
        const bodies = pipe(vals, map(v => v.body));
        return evalSequence(exp.body, makeRecEnv(vars, paramss, bodies, env));
    } else {
        return E.left("Letrec: all variables must be bound to procedures");
    }
}
