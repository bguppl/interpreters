// L6-eval: CPS version of L5

import { map, reduce, repeat, zipWith } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         Parsed, PrimOp, ProcExp, Program, SetExp, VarDecl } from '../L5/L5-ast';
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { parse, unparse } from "../L5/L5-ast";
import { isBoolean, isEmpty, isNumber, isString } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isProgram, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env } from "../L5/L5-env";
import { isEmptySExp, isSymbolSExp, makeEmptySExp, parsedToString, EmptySExp } from '../L5/L5-value';
import { isClosure, isCompoundSExp, makeClosure, makeCompoundSExp,
         Closure, CompoundSExp, Value } from "../L5/L5-value";
import { getErrorMessages, hasNoError, isError }  from "../shared/error";
import { allT, first, rest } from '../shared/list';

// ========================================================
// Continuation datatype
type Cont = (res: Value | Error) => Value | Error;
type ContArray = (results: Array<Value | Error>) => Value | Error;

// ========================================================
// Eval functions

export const evalCont = <T>(exp: CExp | Error, env: Env, cont: Cont): Value | Error =>
    isError(exp)  ? cont(exp) :
    isNumExp(exp) ? cont(exp.val) :
    isBoolExp(exp) ? cont(exp.val) :
    isStrExp(exp) ? cont(exp.val) :
    isPrimOp(exp) ? cont(exp) :
    isVarRef(exp) ? cont(applyEnv(env, exp.var)) :
    isLitExp(exp) ? cont(exp.val) :
    isIfExp(exp) ? evalIf(exp, env, cont) :
    isProcExp(exp) ? evalProc(exp, env, cont) :
    isLetExp(exp) ? evalLet(exp, env, cont) :
    isLetrecExp(exp) ? evalLetrec(exp, env, cont) :
    isSetExp(exp) ? evalSet(exp, env, cont) :
    isAppExp(exp) ? evalApp(exp, env, cont) :
    cont(Error(`Bad L5 AST ${exp}`));

export const isTrueValue = (x: Value | Error): boolean | Error =>
    isError(x) ? x :
    ! (x === false);

const evalIf = (exp: IfExp, env: Env, cont: Cont): Value | Error =>
    evalCont(exp.test, env,
             (testVal) => isError(testVal) ? cont(testVal) :
                          isTrueValue(testVal) ? evalCont(exp.then, env, cont) :
                          evalCont(exp.alt, env, cont));


const evalProc = (exp: ProcExp, env: Env, cont: Cont): Value | Error =>
    cont(makeClosure(exp.args, exp.body, env));

// Return the vals (rhs) of the bindings of a let expression
const letVals = (exp: LetExp | LetrecExp): CExp[] =>
        map((b) => b.val, exp.bindings);

// Return the vars (lhs) of the bindings of a let expression
const letVars = (exp: LetExp | LetrecExp): string[] =>
        map((b) => b.var.var, exp.bindings);

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env, cont: Cont): Value | Error =>
    evalExps(letVals(exp), env,
            (vals) => hasNoError(vals) ? evalSequence(exp.body, makeExtEnv(letVars(exp), vals, env), cont) :
                      cont(Error(getErrorMessages(vals))));

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Value | Error =>
    isEmpty(exps) ? cont(Error("Empty Sequence")) :
    evalSequenceFR(first(exps), rest(exps), env, cont);

const evalSequenceFR = (exp: Exp, exps: Exp[], env: Env, cont: Cont): Value | Error =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, (firstVal) =>
        isError(firstVal) ? cont(firstVal) :
        evalSequence(exps, env, cont));

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
const evalDefineExps = (exp: DefineExp, exps: Exp[], cont: Cont): Value | Error =>
    evalCont(exp.val, theGlobalEnv,
             (rhsVal) => {
                if (isError(rhsVal))
                    return cont(rhsVal);
                else {
                    globalEnvAddBinding(exp.var.var, rhsVal);
                    return evalSequence(exps, theGlobalEnv, cont);
                }
             });

// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Value | Error =>
    isEmpty(exps) ? cont([]) :
    evalExpsFR(first(exps), rest(exps), env, cont)

const evalExpsFR = (exp: Exp, exps: Exp[], env: Env, cont: ContArray): Value | Error =>
    isDefineExp(exp) ? cont([Error("Unexpected define: "+unparse(exp))]) :
    evalCont(exp, env,
             (firstVal) =>
                isError(firstVal) ? cont([firstVal]) :
                evalExps(exps, env,
                         (restVals) => cont([firstVal, ...restVals])));

// LETREC: Direct evaluation rule without syntax expansion
// 1. extend the env with vars initialized to void (temporary value)
// 2. compute the vals in the new extended env
// 3. update the bindings of the vars to the computed vals
// 4. compute body in extended env
const evalLetrec = (exp: LetrecExp, env: Env, cont: Cont): Value | Error => {
    const vars = letVars(exp);
    const vals = letVals(exp);
    const extEnv = makeExtEnv(vars, repeat(undefined, vars.length), env);
    // Compute the vals in the extended env
    return evalExps(vals, extEnv,
                    (cvals) => {
                        if (hasNoError(cvals)) {
                            // Bind vars in extEnv to the new values
                            zipWith((bdg, cval) => setFBinding(bdg, cval), extEnv.frame.fbindings, cvals);
                            return evalSequence(exp.body, extEnv, cont);
                        } else {
                            return cont(Error(getErrorMessages(cvals)));
                        }
                    })
}

// Handling of mutation with set!
const evalSet = (exp: SetExp, env: Env, cont: Cont): Value | Error =>
    evalCont(exp.val, env,
        (rhsVal) => {
            if (isError(rhsVal))
                return cont(rhsVal);
            else {
                const v = exp.var.var;
                const bdg = applyEnvBdg(env, v);
                if (isError(bdg)) {
                    return cont(Error(`Var not found ${v}`));
                } else {
                    setFBinding(bdg, rhsVal);
                    return cont(undefined);
                }
            }
        });

const evalApp = (exp: AppExp, env: Env, cont: Cont): Value | Error =>
    evalCont(exp.rator, env,
        (proc) => evalExps(exp.rands, env,
                    (args) => applyProcedure(proc, args, cont)));

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
const applyProcedure = (proc: Value | Error, args: Array<Value | Error>, cont: Cont): Value | Error =>
    isError(proc) ? cont(proc) :
    !hasNoError(args) ? cont(Error(`Bad argument: ${getErrorMessages(args)}`)) :
    isPrimOp(proc) ? cont(applyPrimitive(proc, args)) :
    isClosure(proc) ? applyClosure(proc, args, cont) :
    cont(Error(`Bad procedure ${JSON.stringify(proc)}`));

const applyClosure = (proc: Closure, args: Value[], cont: Cont): Value | Error => {
    let vars = map((v: VarDecl) => v.var, proc.params);
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env), cont);
}


// Final continuation
// export const topCont: Cont = (val) => { console.log(parsedToString(val)); return val; }
export const topCont: Cont = val => val;

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

// ========================================================
// Primitives (Unchanged in L6)

// @Pre: none of the args is an Error (checked in applyProcedure)
export const applyPrimitive = (proc: PrimOp, args: Value[]): Value | Error =>
    proc.op === "+" ? (allT(isNumber, args) ? reduce((x, y) => x + y, 0, args) : Error("+ expects numbers only")) :
    proc.op === "-" ? minusPrim(args) :
    proc.op === "*" ? (allT(isNumber, args) ? reduce((x, y) => x * y, 1, args) : Error("* expects numbers only")) :
    proc.op === "/" ? divPrim(args) :
    proc.op === ">" ? args[0] > args[1] :
    proc.op === "<" ? args[0] < args[1] :
    proc.op === "=" ? args[0] === args[1] :
    proc.op === "not" ? ! args[0] :
    proc.op === "eq?" ? eqPrim(args) :
    proc.op === "string=?" ? args[0] === args[1] :
    proc.op === "cons" ? consPrim(args[0], args[1]) :
    proc.op === "car" ? carPrim(args[0]) :
    proc.op === "cdr" ? cdrPrim(args[0]) :
    proc.op === "list" ? listPrim(args) :
    proc.op === "list?" ? isListPrim(args[0]) :
    proc.op === "pair?" ? isPairPrim(args[0]) :
    proc.op === "number?" ? typeof(args[0]) === 'number' :
    proc.op === "boolean?" ? typeof(args[0]) === 'boolean' :
    proc.op === "symbol?" ? isSymbolSExp(args[0]) :
    proc.op === "string?" ? isString(args[0]) :
    // display, newline
    Error("Bad primitive op " + proc.op);

const minusPrim = (args: Value[]): number | Error => {
    // TODO complete
    let x = args[0], y = args[1];
    if (isNumber(x) && isNumber(y)) {
        return x - y;
    } else {
        return Error(`Type error: - expects numbers ${args}`)
    }
}

const divPrim = (args: Value[]): number | Error => {
    // TODO complete
    let x = args[0], y = args[1];
    if (isNumber(x) && isNumber(y)) {
        return x / y;
    } else {
        return Error(`Type error: / expects numbers ${args}`)
    }
}

const eqPrim = (args: Value[]): boolean | Error => {
    let x = args[0], y = args[1];
    if (isSymbolSExp(x) && isSymbolSExp(y)) {
        return x.val === y.val;
    } else if (isEmptySExp(x) && isEmptySExp(y)) {
        return true;
    } else if (isNumber(x) && isNumber(y)) {
        return x === y;
    } else if (isString(x) && isString(y)) {
        return x === y;
    } else if (isBoolean(x) && isBoolean(y)) {
        return x === y;
    } else {
        return false;
    }
}

const carPrim = (v: Value): Value | Error =>
    isCompoundSExp(v) ? v.val1 :
    Error(`Car: param is not compound ${v}`);

const cdrPrim = (v: Value): Value | Error =>
    isCompoundSExp(v) ? v.val2 :
    Error(`Cdr: param is not compound ${v}`);

const consPrim = (v1: Value, v2: Value): CompoundSExp =>
    makeCompoundSExp(v1, v2);

export const listPrim = (vals: Value[]): EmptySExp | CompoundSExp =>
    vals.length === 0 ? makeEmptySExp() :
    makeCompoundSExp(first(vals), listPrim(rest(vals)))

const isListPrim = (v: Value): boolean =>
    isEmptySExp(v) || isCompoundSExp(v);

const isPairPrim = (v: Value): boolean =>
    isCompoundSExp(v);

