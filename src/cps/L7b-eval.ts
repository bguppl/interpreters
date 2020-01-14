// L7-eval: CPS version of L5 with concrete data-structure continuations

import { map, reduce, repeat, zip, zipWith } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         Parsed, PrimOp, ProcExp, Program, SetExp, VarDecl } from '../L5/L5-ast';
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

// ========================================================
// Concrete Continuation datatype
// type Cont = (res: Value | Error) => Value | Error;
// type ContArray = (results: Array<Value | Error>) => Value | Error;
export type Cont = IfCont | FirstCont | SetCont | AppCont1 | ExpsCont1 | DefCont | TopCont;
export type ContArray = LetCont | LetrecCont | AppCont2 | ExpsCont2;

export const isCont = (x: any): x is Cont => isIfCont(x) || isFirstCont(x) || isSetCont(x) ||
            isAppCont1(x) || isExpsCont1(x) || isDefCont(x) || isTopCont(x);
export const isContArray = (x: any): x is ContArray => isLetCont(x) || isLetrecCont(x) || isAppCont2(x) || isExpsCont2(x);

export interface TopCont {tag: "TopCont"};
export const makeTopCont = (): TopCont => ({tag: "TopCont"});
export const isTopCont = (x: any): x is TopCont => x.tag === "TopCont";

export interface IfCont {tag: "IfCont", exp: IfExp, env: Env, cont: Cont};
export const makeIfCont = (exp: IfExp, env: Env, cont: Cont): IfCont => ({tag: "IfCont", env: env, exp: exp, cont: cont});
export const isIfCont = (x: any): x is IfCont => x.tag === "IfCont";

export interface FirstCont {tag: "FirstCont", exps: Exp[], env: Env, cont: Cont};
export const makeFirstCont = (exps: Exp[], env: Env, cont: Cont): FirstCont => ({tag: "FirstCont", env: env, exps: exps, cont: cont});
export const isFirstCont = (x: any): x is FirstCont => x.tag === "FirstCont";

export interface SetCont {tag: "SetCont", exp: SetExp, env: Env, cont: Cont};
export const makeSetCont = (exp: SetExp, env: Env, cont: Cont): SetCont => ({tag: "SetCont", env: env, exp: exp, cont: cont});
export const isSetCont = (x: any): x is SetCont => x.tag === "SetCont";

export interface AppCont1 {tag: "AppCont1", exp: AppExp, env: Env, cont: Cont};
export const makeAppCont1 = (exp: AppExp, env: Env, cont: Cont): AppCont1 => ({tag: "AppCont1", env: env, exp: exp, cont: cont});
export const isAppCont1 = (x: any): x is AppCont1 => x.tag === "AppCont1";

export interface ExpsCont1 {tag: "ExpsCont1", exps: Exp[], env: Env, cont: ContArray};
export const makeExpsCont1 = (exps: Exp[], env: Env, cont: ContArray): ExpsCont1 => ({tag: "ExpsCont1", env: env, exps: exps, cont: cont});
export const isExpsCont1 = (x: any): x is ExpsCont1 => x.tag === "ExpsCont1";

export interface LetCont {tag: "LetCont", exp: LetExp, env: Env, cont: Cont};
export const makeLetCont = (exp: LetExp, env: Env, cont: Cont): LetCont => ({tag: "LetCont", env: env, exp: exp, cont: cont});
export const isLetCont = (x: any): x is LetCont => x.tag === "LetCont";

export interface LetrecCont {tag: "LetrecCont", exp: LetrecExp, env: ExtEnv, cont: Cont};
export const makeLetrecCont = (exp: LetrecExp, env: ExtEnv, cont: Cont): LetrecCont => ({tag: "LetrecCont", env: env, exp: exp, cont: cont});
export const isLetrecCont = (x: any): x is LetrecCont => x.tag === "LetrecCont";

export interface AppCont2 {tag: "AppCont2", proc: Value | Error, env: Env, cont: Cont};
export const makeAppCont2 = (proc: Value | Error, env: Env, cont: Cont): AppCont2 => ({tag: "AppCont2", proc: proc, env: env, cont: cont});
export const isAppCont2 = (x: any): x is AppCont2 => x.tag === "AppCont2";

export interface ExpsCont2 {tag: "ExpsCont2", firstVal: Value | Error, cont: ContArray};
export const makeExpsCont2 = (firstVal: Value | Error, cont: ContArray): ExpsCont2 => ({tag: "ExpsCont2", firstVal: firstVal, cont: cont});
export const isExpsCont2 = (x: any): x is ExpsCont2 => x.tag === "ExpsCont2";

export interface DefCont {tag: "DefCont", exp: DefineExp, exps: Exp[], cont: Cont};
export const makeDefCont = (exp: DefineExp, exps: Exp[], cont: Cont): DefCont => ({tag: "DefCont", exp: exp, exps: exps, cont: cont});
export const isDefCont = (x: any): x is DefCont => x.tag === "DefCont";

const applyCont = (cont: Cont, val: Value | Error): Value | Error => 
    isIfCont(cont) ? applyIfCont(cont, val) :
    isFirstCont(cont) ? applyFirstCont(cont, val) :
    isSetCont(cont) ? applySetCont(cont, val) :
    isAppCont1(cont) ? applyAppCont1(cont, val) :
    isExpsCont1(cont) ? applyExpsCont1(cont, val) :
    isDefCont(cont) ? applyDefCont(cont, val) :
    isTopCont(cont) ? applyTopCont(cont, val) :
    Error(`Unknown cont ${cont}`);

const applyContArray = (cont: ContArray, val: Array<Value | Error>): Value | Error =>
    isLetCont(cont) ? applyLetCont(cont, val) :
    isLetrecCont(cont) ? applyLetrecCont(cont, val) :
    isAppCont2(cont) ? applyAppCont2(cont, val) :
    isExpsCont2(cont) ? applyExpsCont2(cont, val) :
    Error(`Unknown cont ${cont}`);

const applyTopCont = (cont: TopCont, val: Value | Error): Value | Error => {
    console.log(parsedToString(val)); return val;
}

const applyIfCont = (cont: IfCont, testVal: Value | Error): Value | Error =>
    isError(testVal) ? applyCont(cont.cont, testVal) :
    isTrueValue(testVal) ? evalCont(cont.exp.then, cont.env, cont.cont) :
    evalCont(cont.exp.alt, cont.env, cont.cont);

const applyLetCont = (cont: LetCont, vals: Array<Value | Error>): Value | Error =>
    hasNoError(vals) ? evalSequence(cont.exp.body, makeExtEnv(letVars(cont.exp), vals, cont.env), cont.cont) :
    applyCont(cont.cont, Error(getErrorMessages(vals)));

export const applyFirstCont = (cont: FirstCont, firstVal: Value | Error): Value | Error =>
    isError(firstVal) ? applyCont(cont.cont, firstVal) :
    evalSequence(cont.exps, cont.env, cont.cont);

export const applyLetrecCont = (cont: LetrecCont, cvals: Array<Value | Error>): Value | Error => {
    if (hasNoError(cvals)) {
        // Bind vars in extEnv to the new values
        zipWith((bdg, cval) => setFBinding(bdg, cval), cont.env.frame.fbindings, cvals);
        return evalSequence(cont.exp.body, cont.env, cont.cont);
    } else {
        return applyCont(cont.cont, Error(getErrorMessages(cvals)));
    }
};

export const applySetCont = (cont: SetCont, rhsVal: Value | Error): Value | Error => {
    if (isError(rhsVal))
        return applyCont(cont.cont, rhsVal);
    else {
        const v = cont.exp.var.var;
        const bdg = applyEnvBdg(cont.env, v);
        if (isError(bdg)) {
            return applyCont(cont.cont, Error(`Var not found ${v}`));
        } else {
            setFBinding(bdg, rhsVal);
            return applyCont(cont.cont, undefined);
        }
    }
};

export const applyAppCont1 = (cont: AppCont1, proc: Value | Error): Value | Error =>
    evalExps(cont.exp.rands, cont.env, makeAppCont2(proc, cont.env, cont.cont));

export const applyAppCont2 = (cont: AppCont2, args: Array<Value | Error>): Value | Error =>
    applyProcedure(cont.proc, args, cont.cont);

export const applyExpsCont1 = (cont: ExpsCont1, firstVal: Value | Error): Value | Error =>
    isError(firstVal) ? applyContArray(cont.cont, [firstVal]) :
    evalExps(cont.exps, cont.env, makeExpsCont2(firstVal, cont.cont));

export const applyExpsCont2 = (cont: ExpsCont2, restVals: Array<Value | Error>): Value | Error =>
    applyContArray(cont.cont, [cont.firstVal, ...restVals]);

export const applyDefCont = (cont: DefCont, rhsVal: Value | Error): Value | Error => {
    if (isError(rhsVal))
        return applyCont(cont.cont, rhsVal);
    else {
        globalEnvAddBinding(cont.exp.var.var, rhsVal);
        return evalSequence(cont.exps, theGlobalEnv, cont.cont);
    }
}

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

// L4-eval-box: Handling of mutation with set!
export const evalSet = (exp: SetExp, env: Env, cont: Cont): Value | Error =>
    evalCont(exp.val, env, makeSetCont(exp, env, cont));

// L4 evalApp
export const evalApp = (exp: AppExp, env: Env, cont: Cont): Value | Error =>
    evalCont(exp.rator, env, makeAppCont1(exp, env, cont));

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
export const applyProcedure = (proc: Value | Error, args: Array<Value | Error>, cont: Cont): Value | Error =>
    isError(proc) ? applyCont(cont, proc) :
    !hasNoError(args) ? applyCont(cont, Error(`Bad argument: ${getErrorMessages(args)}`)) :
    isPrimOp(proc) ? applyCont(cont, applyPrimitive(proc, args)) :
    isClosure(proc) ? applyClosure(proc, args, cont) :
    applyCont(cont, Error(`Bad procedure ${JSON.stringify(proc)}`));

export const applyClosure = (proc: Closure, args: Value[], cont: Cont): Value | Error => {
    let vars = map((v: VarDecl) => v.var, proc.params);
    return evalSequence(proc.body, makeExtEnv(vars, args, proc.env), cont);
}

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Value | Error =>
    isEmpty(exps) ? applyCont(cont, Error("Empty Sequence")) :
    evalSequenceFR(first(exps), rest(exps), env, cont);

const evalSequenceFR = (exp: Exp, exps: Exp[], env: Env, cont: Cont): Value | Error =>
    isDefineExp(exp) ? evalDefineExps(exp, exps, cont) :
    isEmpty(exps) ? evalCont(exp, env, cont) :
    evalCont(exp, env, makeFirstCont(exps, env, cont));

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
const evalDefineExps = (exp: DefineExp, exps: Exp[], cont: Cont): Value | Error =>
    evalCont(exp.val, theGlobalEnv, makeDefCont(exp, exps, cont));

// Evaluate an array of expressions - pass the result as an array to the continuation
export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Value | Error =>
    isEmpty(exps) ? applyContArray(cont, []) :
    evalExpsFR(first(exps), rest(exps), env, cont);

const evalExpsFR = (exp: Exp, exps: Exp[], env: Env, cont: ContArray): Value | Error =>
    isDefineExp(exp) ? applyContArray(cont, [Error("Unexpected define: "+unparse(exp))]) :
    evalCont(exp, env, makeExpsCont1(exps, env, cont));

// Evaluate a program
// Main program
export const evalProgram = (program: Program): Value | Error =>
    evalSequence(program.exps, theGlobalEnv, makeTopCont());

export const evalParse = (s: string): Value | Error => {
    let ast: Parsed | Error = parse(s);
    if (isProgram(ast)) {
        return evalProgram(ast);
    } else if (isExp(ast)) {
        return evalSequence([ast], theGlobalEnv, makeTopCont());
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

