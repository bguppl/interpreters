// L7-eval: CPS version of L5 with concrete data-structure continuations

import { map, reduce, repeat, zipWith } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         Parsed, ProcExp, PrimOp, Program, SetExp, VarDecl } from '../L5/L5-ast';
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { parse, unparse } from "../L5/L5-ast";
import { isBoolean, isEmpty, isNumber, isString } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isProgram, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, ExtEnv } from "../L5/L5-env";
import { isEmptySExp, isSymbolSExp, makeEmptySExp, parsedToString, EmptySExp } from '../L5/L5-value';
import { isClosure, isCompoundSExp, makeClosure, makeCompoundSExp,
         Closure, CompoundSExp, Value } from "../L5/L5-value";
import { getErrorMessages, hasNoError, isError }  from "../shared/error";
import { allT, first, rest } from '../shared/list';
import { applyPrimitive } from "../L5/evalPrimitive";

// ========================================================
// Continuation datatype
type Cont = (res: Value | Error) => Value | Error;
type ContArray = (results: Array<Value | Error>) => Value | Error;

const applyCont = (cont: Cont, val: Value | Error) => cont(val);
const applyContArray = (cont: ContArray, val: Array<Value | Error>) => cont(val);

// ========================================================
// Eval functions

export const evalCont = (exp: CExp | Error, env: Env, cont: Cont): Value | Error =>
    isError(exp)  ? applyCont(cont, exp) :
    isNumExp(exp) ? applyCont(cont, exp.val) :
    isBoolExp(exp) ? applyCont(cont, exp.val) :
    isStrExp(exp) ? applyCont(cont, exp.val) :
    isPrimOp(exp) ? applyCont(cont, exp) :
    isVarRef(exp) ? applyCont(cont, applyEnv(env, exp.var)) :
    isLitExp(exp) ? applyCont(cont, exp.val) :
    isIfExp(exp) ? evalIf(exp, env, cont) :
    isProcExp(exp) ? evalProc(exp, env, cont) :
    isLetExp(exp) ? evalLet(exp, env, cont) :
    isLetrecExp(exp) ? evalLetrec(exp, env, cont) :
    isSetExp(exp) ? evalSet(exp, env, cont) :
    isAppExp(exp) ? evalApp(exp, env, cont) :
    applyCont(cont, Error(`Bad L5 AST ${exp}`));

export const isTrueValue = (x: Value | Error): boolean | Error =>
    isError(x) ? x :
    ! (x === false);

const evalIf = (exp: IfExp, env: Env, cont: Cont): Value | Error =>
    evalCont(exp.test, env, makeIfCont(exp, env, cont));

const makeIfCont = (exp: IfExp, env: Env, cont: Cont) =>
    (testVal: Value | Error) =>
        isError(testVal) ? applyCont(cont, testVal) :
        isTrueValue(testVal) ? evalCont(exp.then, env, cont) :
        evalCont(exp.alt, env, cont);


const evalProc = (exp: ProcExp, env: Env, cont: Cont): Value | Error =>
    applyCont(cont, makeClosure(exp.args, exp.body, env));

// Return the vals (rhs) of the bindings of a let expression
const letVals = (exp: LetExp | LetrecExp): CExp[] =>
        map((b) => b.val, exp.bindings);

// Return the vars (lhs) of the bindings of a let expression
const letVars = (exp: LetExp | LetrecExp): string[] =>
        map((b) => b.var.var, exp.bindings);

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env, cont: Cont): Value | Error =>
    evalExps(letVals(exp), env, makeLetCont(exp, env, cont));

const makeLetCont = (exp: LetExp, env: Env, cont: Cont) =>
    (vals: Array<Value | Error>) =>
        hasNoError(vals) ? evalSequence(exp.body, makeExtEnv(letVars(exp), vals, env), cont) :
        applyCont(cont, Error(getErrorMessages(vals)));

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Value | Error =>
    isEmpty(exps) ? applyCont(cont, Error("Empty Sequence")) :
    evalSequenceFR(first(exps), rest(exps), env, cont);

const evalSequenceFR = (exp: Exp, exps: Exp[], env: Env, cont: Cont): Value | Error =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, makeFirstCont(exps, env, cont));

export const makeFirstCont = (exps: Exp[], env: Env, cont: Cont) =>
    (firstVal: Value | Error) =>
        isError(firstVal) ? applyCont(cont, firstVal) :
        evalSequence(exps, env, cont);

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
export const evalDefineExps = (exp: DefineExp, exps: Exp[], cont: Cont): Value | Error =>
    evalCont(exp.val, theGlobalEnv, makeDefCont(exp, exps, cont));

export const makeDefCont = (exp: DefineExp, exps: Exp[], cont: Cont) =>
    (rhsVal: Value | Error) => {
        if (isError(rhsVal))
            return applyCont(cont, rhsVal);
        else {
            globalEnvAddBinding(exp.var.var, rhsVal);
            return evalSequence(exps, theGlobalEnv, cont);
        }
    };

// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Value | Error =>
    isEmpty(exps) ? applyContArray(cont, []) :
    evalExpsFR(first(exps), rest(exps), env, cont)

const evalExpsFR = (exp: Exp, exps: Exp[], env: Env, cont: ContArray): Value | Error =>
    isDefineExp(exp) ? applyContArray(cont, [Error("Unexpected define: "+unparse(exp))]) :
    evalCont(exp, env, makeExpsCont1(exps, env, cont));

const makeExpsCont1 = (exps: Exp[], env: Env, cont: ContArray) =>
    (firstVal: Value | Error) => 
        isError(firstVal) ? applyContArray(cont, [firstVal]) :    
        evalExps(exps, env, makeExpsCont2(firstVal, cont));

const makeExpsCont2 = (firstVal: Value, cont: ContArray) =>
    (restVals: Array<Value | Error>) => applyContArray(cont, [firstVal, ...restVals]);

// LETREC: Direct evaluation rule without syntax expansion
// 1. extend the env with vars initialized to void (temporary value)
// 2. compute the vals in the new extended env
// 3. update the bindings of the vars to the computed vals
// 4. compute body in extended env
export const evalLetrec = (exp: LetrecExp, env: Env, cont: Cont): Value | Error => {
    const vars = letVars(exp);
    const vals = letVals(exp);
    const extEnv = makeExtEnv(vars, repeat(undefined, vars.length), env);
    // Compute the vals in the extended env
    return evalExps(vals, extEnv, makeLetrecCont(exp, extEnv, cont));
}

export const makeLetrecCont = (exp: LetrecExp, extEnv: ExtEnv, cont: Cont): ContArray =>
    (cvals: Array<Value | Error>) => {
        if (hasNoError(cvals)) {
            // Bind vars in extEnv to the new values
            zipWith((bdg, cval) => setFBinding(bdg, cval), extEnv.frame.fbindings, cvals);
            return evalSequence(exp.body, extEnv, cont);
        } else {
            return applyCont(cont, Error(getErrorMessages(cvals)));
        }
    };


// L4-eval-box: Handling of mutation with set!
export const evalSet = (exp: SetExp, env: Env, cont: Cont): Value | Error =>
    evalCont(exp.val, env, makeSetCont(exp, env, cont));

export const makeSetCont = (exp: SetExp, env: Env, cont: Cont) =>
    (rhsVal: Value | Error) => {
        if (isError(rhsVal))
            return applyCont(cont, rhsVal);
        else {
            const v = exp.var.var;
            const bdg = applyEnvBdg(env, v);
            if (isError(bdg)) {
                return applyCont(cont, Error(`Var not found ${v}`));
            } else {
                setFBinding(bdg, rhsVal);
                return applyCont(cont, undefined);
            }
        }
    };

const evalApp = (exp: AppExp, env: Env, cont: Cont): Value | Error =>
    evalCont(exp.rator, env,
        (proc) => evalExps(exp.rands, env,
                    (args) => applyProcedure(proc, args, cont)));

const applyProcedure = (proc: Value | Error, args: Array<Value | Error>, cont: Cont): Value | Error =>
    isError(proc) ? applyCont(cont, proc) :
    !hasNoError(args) ? applyCont(cont, Error(`Bad argument: ${getErrorMessages(args)}`)) :
    isPrimOp(proc) ? applyCont(cont, applyPrimitive(proc, args)) :
    isClosure(proc) ? applyClosure(proc, args, cont) :
    applyCont(cont, Error(`Bad procedure ${JSON.stringify(proc)}`));

const applyClosure = (proc: Closure, args: Value[], cont: Cont): Value | Error => {
    let vars = map((v: VarDecl) => v.var, proc.params);
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env), cont);
}

// Final continuation
export const topCont: Cont = (val) => { console.log(parsedToString(val)); return val; }

// Evaluate a program
// Main program
export const evalProgram = (program: Program): Value | Error =>
    evalSequence(program.exps, theGlobalEnv, topCont);

export const evalParse = (s: string): Value | Error => {
    let ast: Parsed | Error = parse(s);
    if (isProgram(ast)) {
        return evalProgram(ast);
    } else if (isExp(ast)) {
        return evalSequence([ast], theGlobalEnv, topCont);
    } else {
        return ast;
    }
}
