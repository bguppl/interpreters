// L6-eval: CPS version of L5

import * as E from "fp-ts/Either";
import { map, replicate, zipWith } from "fp-ts/ReadonlyArray";
import { identity, pipe } from "fp-ts/function";
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
import { isEmpty, first, rest } from '../shared/list';
import { parse as p } from "../shared/parser";

// ========================================================
// Continuation datatype
type Cont = (res: E.Either<string, Value>) => E.Either<string, Value>;
type ContArray = (results: E.Either<string, readonly Value[]>) => E.Either<string, Value>;

// ========================================================
// Eval functions

export const evalCont = (exp: CExp, env: Env, cont: Cont): E.Either<string, Value> =>
    isNumExp(exp) ? cont(E.of(exp.val)) :
    isBoolExp(exp) ? cont(E.of(exp.val)) :
    isStrExp(exp) ? cont(E.of(exp.val)) :
    isPrimOp(exp) ? cont(E.of(exp)) :
    isVarRef(exp) ? cont(applyEnv(env, exp.var)) :
    isLitExp(exp) ? cont(E.of(exp.val)) :
    isIfExp(exp) ? evalIf(exp, env, cont) :
    isProcExp(exp) ? evalProc(exp, env, cont) :
    isLetExp(exp) ? evalLet(exp, env, cont) :
    isLetrecExp(exp) ? evalLetrec(exp, env, cont) :
    isSetExp(exp) ? evalSet(exp, env, cont) :
    isAppExp(exp) ? evalApp(exp, env, cont) :
    exp;

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env, cont: Cont): E.Either<string, Value> =>
    evalCont(exp.test, env,
             (testVal: E.Either<string, Value>) => pipe(
                testVal,
                E.chain(test => isTrueValue(test) ?
                                evalCont(exp.then, env, cont) :
                                evalCont(exp.alt, env, cont))
             ));


const evalProc = (exp: ProcExp, env: Env, cont: Cont): E.Either<string, Value> =>
    cont(E.of(makeClosure(exp.args, exp.body, env)));

// Return the vals (rhs) of the bindings of a let expression
const letVals = (exp: LetExp | LetrecExp): readonly CExp[] =>
    pipe(exp.bindings, map(b => b.val));

// Return the vars (lhs) of the bindings of a let expression
const letVars = (exp: LetExp | LetrecExp): readonly string[] =>
    pipe(exp.bindings, map(b => b.var.var));

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env, cont: Cont): E.Either<string, Value> =>
    evalExps(letVals(exp), env,
             (vals: E.Either<string, readonly Value[]>) => 
                pipe(
                    vals,
                    E.chain(values => evalSequence(exp.body, makeExtEnv(letVars(exp), values, env), cont))
                ));

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: readonly Exp[], env: Env, cont: Cont): E.Either<string, Value> =>
    isEmpty(exps) ? cont(E.left("Empty Sequence")) :
    evalSequenceFR(first(exps), rest(exps), env, cont);

const evalSequenceFR = (exp: Exp, exps: readonly Exp[], env: Env, cont: Cont): E.Either<string, Value> =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, (firstVal: E.Either<string, Value>) => pipe(firstVal, E.chain(_ => evalSequence(exps, env, cont))));

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
const evalDefineExps = (exp: DefineExp, exps: readonly Exp[], cont: Cont): E.Either<string, Value> =>
    evalCont(exp.val, theGlobalEnv,
             (rhsVal: E.Either<string, Value>) => pipe(
                 rhsVal,
                 E.chain(rhs => {
                     globalEnvAddBinding(exp.var.var, rhs);
                     return evalSequence(exps, theGlobalEnv, cont);
                 })
             ));

// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: readonly Exp[], env: Env, cont: ContArray): E.Either<string, Value> =>
    isEmpty(exps) ? cont(E.of([])) :
    evalExpsFR(first(exps), rest(exps), env, cont)

const evalExpsFR = (exp: Exp, exps: readonly Exp[], env: Env, cont: ContArray): E.Either<string, Value> =>
    isDefineExp(exp) ? cont(pipe(unparse(exp), E.chain(up => E.left(`Unexpected define: ${up}`)))) :
    evalCont(exp, env,
             (firstVal: E.Either<string, Value>) =>
                pipe(
                    firstVal,
                    E.chain(first => evalExps(exps, env, restVals => pipe(
                        restVals,
                        E.chain(restVals => cont(E.of([first, ...restVals])))
                    )))
                ));

// LETREC: Direct evaluation rule without syntax expansion
// 1. extend the env with vars initialized to void (temporary value)
// 2. compute the vals in the new extended env
// 3. update the bindings of the vars to the computed vals
// 4. compute body in extended env
const evalLetrec = (exp: LetrecExp, env: Env, cont: Cont): E.Either<string, Value> => {
    const vars = letVars(exp);
    const vals = letVals(exp);
    const extEnv = makeExtEnv(vars, replicate(vars.length, undefined), env);
    // Compute the vals in the extended env
    return evalExps(vals, extEnv,
                    (cvals: E.Either<string, readonly Value[]>) => pipe(
                        cvals,
                        E.chain(values => {
                            zipWith(extEnv.frame.fbindings, values, setFBinding);
                            return evalSequence(exp.body, extEnv, cont);
                        })
                    ));
}

// Handling of mutation with set!
const evalSet = (exp: SetExp, env: Env, cont: Cont): E.Either<string, Value> =>
    evalCont(exp.val, env,
             (rhsVal: E.Either<string, Value>) => pipe(
                 rhsVal,
                 E.chain(rhs => pipe(
                    applyEnvBdg(env, exp.var.var),
                    E.chain(bdg => cont(E.of(setFBinding(bdg, rhs))))
                 ))
             ));

const evalApp = (exp: AppExp, env: Env, cont: Cont): E.Either<string, Value> =>
    evalCont(exp.rator, env,
             (proc: E.Either<string, Value>) => pipe(
                 proc,
                 E.chain(p => evalExps(exp.rands, env, args => pipe(
                     args,
                     E.chain(as => applyProcedure(p, as, cont))
                 )))
             ));

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
const applyProcedure = (proc: Value, args: readonly Value[], cont: Cont): E.Either<string, Value> =>
    isPrimOp(proc) ? cont(applyPrimitive(proc, args)) :
    isClosure(proc) ? applyClosure(proc, args, cont) :
    cont(E.left(`Bad procedure ${JSON.stringify(proc)}`));

const applyClosure = (proc: Closure, args: readonly Value[], cont: Cont): E.Either<string, Value> => {
    const vars = pipe(proc.params, map(v => v.var));
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env), cont);
}

// Final continuation
// export const topCont: Cont = (val) => { console.log(parsedToString(val)); return val; }
export const topCont: Cont = identity;

// Evaluate a program
// Main program
export const evalProgram = (program: Program): E.Either<string, Value> =>
    evalSequence(program.exps, theGlobalEnv, topCont);

export const evalParse = (s: string): E.Either<string, Value> =>
    pipe(
        p(s),
        E.chain(parseL5Exp),
        E.chain(exp => evalSequence([exp], theGlobalEnv, topCont))
    );
