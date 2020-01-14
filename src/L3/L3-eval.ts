// L3-eval.ts

import { filter, indexOf, map, reduce, contains, zip, KeyValuePair } from "ramda";
import { isCExp, isLetExp } from "./L3-ast";
import { BoolExp, CExp, Exp, IfExp, LitExp, NumExp,
         Parsed, PrimOp, ProcExp, Program, StrExp, VarDecl, VarRef } from "./L3-ast";
import { isAppExp, isBoolExp, isDefineExp, isExp, isIfExp, isLitExp, isNumExp,
             isPrimOp, isProcExp, isProgram, isStrExp, isVarRef } from "./L3-ast";
import { makeAppExp, makeBoolExp, makeIfExp, makeLitExp, makeNumExp, makeProcExp, makeStrExp,
         makeVarDecl, makeVarRef } from "./L3-ast";
import { parseL3 } from "./L3-ast";
import { applyEnv, makeEmptyEnv, makeEnv, Env } from "./L3-env";
import { isClosure, isCompoundSExp, isEmptySExp, isSymbolSExp,
         makeClosure, makeCompoundSExp, makeEmptySExp, 
         Closure, CompoundSExp, EmptySExp, Value } from "./L3-value";
import { getErrorMessages, hasNoError, isError }  from "../shared/error";
import { allT, first, rest, second, isBoolean, isEmpty, isNumber, isString } from '../shared/list';

// ========================================================
// Eval functions

const L3applicativeEval = (exp: CExp | Error, env: Env): Value | Error =>
    isError(exp)  ? exp :
    isNumExp(exp) ? exp.val : 
    isBoolExp(exp) ? exp.val :
    isStrExp(exp) ? exp.val :
    isPrimOp(exp) ? exp :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isLitExp(exp) ? exp.val :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? evalProc(exp, env) :
    isAppExp(exp) ? L3applyProcedure(L3applicativeEval(exp.rator, env),
                                     map((rand) => L3applicativeEval(rand, env),
                                         exp.rands),
                                     env) :
    isLetExp(exp) ? Error("") :
    Error(`Bad L3 AST ${exp}`);

export const isTrueValue = (x: Value | Error): boolean | Error =>
    isError(x) ? x :
    ! (x === false);

const evalIf = (exp: IfExp, env: Env): Value | Error => {
    const test = L3applicativeEval(exp.test, env);
    return isError(test) ? test :
        isTrueValue(test) ? L3applicativeEval(exp.then, env) :
        L3applicativeEval(exp.alt, env);
};

const evalProc = (exp: ProcExp, env: Env): Value =>
    makeClosure(exp.args, exp.body);

const L3applyProcedure = (proc: Value | Error, args: Array<Value | Error>, env: Env): Value | Error =>
    isError(proc) ? proc :
    !hasNoError(args) ? Error(`Bad argument: ${getErrorMessages(args)}`) :
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args, env) :
    Error("Bad procedure " + JSON.stringify(proc))

const valueToLitExp = (v: Value): NumExp | BoolExp | StrExp | LitExp | PrimOp | ProcExp =>
    isNumber(v) ? makeNumExp(v) :
    isBoolean(v) ? makeBoolExp(v) :
    isString(v) ? makeStrExp(v) :
    isPrimOp(v) ? v :
    isClosure(v) ? makeProcExp(v.params, v.body) :
    makeLitExp(v);

// @Pre: none of the args is an Error (checked in applyProcedure)
const applyClosure = (proc: Closure, args: Value[], env: Env): Value | Error => {
    let vars = map((v: VarDecl) => v.var, proc.params);
    let body = renameExps(proc.body);
    let litArgs = map(valueToLitExp, args);
    return evalExps(substitute(body, vars, litArgs), env);
}

// For applicative eval - the type of exps should be ValueExp[] | VarRef[];
// where ValueExp is an expression which directly encodes a value:
// export type ValueExp = LitExp | NumExp | BoolExp | StrExp | PrimOp;
// In order to support normal eval as well - we generalize the types to CExp.

// @Pre: vars and exps have the same length
export const substitute = (body: CExp[], vars: string[], exps: CExp[]): CExp[] => {
    const subVarRef = (e: VarRef): CExp => {
        const pos = indexOf(e.var, vars);
        return ((pos > -1) ? exps[pos] : e);
    };
    const subProcExp = (e: ProcExp): ProcExp => {
        const argNames = map((x) => x.var, e.args);
        const subst = zip(vars, exps);
        const freeSubst = filter((ve) => !contains(first(ve), argNames), subst);
        return makeProcExp(e.args,
                           substitute(e.body,
                                      map((x: KeyValuePair<string, CExp>) => x[0], freeSubst),
                                      map((x: KeyValuePair<string, CExp>) => x[1], freeSubst)));
    };
    const sub = (e: CExp): CExp =>
        isNumExp(e) ? e :
        isBoolExp(e) ? e :
        isPrimOp(e) ? e :
        isLitExp(e) ? e :
        isStrExp(e) ? e :
        isVarRef(e) ? subVarRef(e) :
        isIfExp(e) ? makeIfExp(sub(e.test), sub(e.then), sub(e.alt)) :
        isProcExp(e) ? subProcExp(e) :
        isAppExp(e) ? makeAppExp(sub(e.rator), map(sub, e.rands)) :
        e;
    return map(sub, body);
}

/*
    Purpose: create a generator of new symbols of the form v__n
    with n incremented at each call.
*/
export const makeVarGen = (): (v: string) => string => {
    let count: number = 0;
    return (v: string) => {
        count++;
        return `${v}__${count}`;
    }
}

/*
Purpose: Consistently rename bound variables in 'exps' to fresh names.
         Start numbering at 1 for all new var names.
*/
export const renameExps = (exps: CExp[]): CExp[] => {
    const varGen = makeVarGen();
    const replace = (e: CExp): CExp =>
        isIfExp(e) ? makeIfExp(replace(e.test), replace(e.then), replace(e.alt)) :
        isAppExp(e) ? makeAppExp(replace(e.rator), map(replace, e.rands)) :
        isProcExp(e) ? replaceProc(e) :
        e;
    // Rename the params and substitute old params with renamed ones.
    //  First recursively rename all ProcExps inside the body.
    const replaceProc = (e: ProcExp): ProcExp => {
        const oldArgs = map((arg: VarDecl): string => arg.var, e.args);
        const newArgs = map(varGen, oldArgs);
        const newBody = map(replace, e.body);
        return makeProcExp(map(makeVarDecl, newArgs),
                           substitute(newBody, oldArgs, map(makeVarRef, newArgs)));
    }
    return map(replace, exps);
}


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
    proc.op === "and" ? isBoolean(args[0]) && isBoolean(args[1]) && args[0] && args[1] :
    proc.op === "or" ? isBoolean(args[0]) && isBoolean(args[1]) && (args[0] || args[1]) :
    proc.op === "eq?" ? eqPrim(args) :
    proc.op === "string=?" ? args[0] === args[1] :
    proc.op === "cons" ? consPrim(args[0], args[1]) :
    proc.op === "car" ? carPrim(args[0]) :
    proc.op === "cdr" ? cdrPrim(args[0]) :
    proc.op === "list" ? listPrim(args) :
    proc.op === "pair?" ? isPairPrim(args[0]) :
    proc.op === "number?" ? typeof(args[0]) === 'number' :
    proc.op === "boolean?" ? typeof(args[0]) === 'boolean' :
    proc.op === "symbol?" ? isSymbolSExp(args[0]) :
    proc.op === "string?" ? isString(args[0]) :
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

const isPairPrim = (v: Value): boolean =>
    isCompoundSExp(v);

// Evaluate a sequence of expressions (in a program)
export const evalExps = (exps: Exp[], env: Env): Value | Error =>
    isEmpty(exps) ? Error("Empty program") :
    isDefineExp(first(exps)) ? evalDefineExps(first(exps), rest(exps), env) :
    evalCExps(first(exps), rest(exps), env);
    
const evalCExps = (exp1: Exp, exps: Exp[], env: Env): Value | Error =>
    isCExp(exp1) && isEmpty(exps) ? L3applicativeEval(exp1, env) :
    isCExp(exp1) ? (isError(L3applicativeEval(exp1, env)) ? Error("error") :
        evalExps(exps, env)) :
    Error("Never");
    
// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: Exp, exps: Exp[], env: Env): Value | Error => {
    if (isDefineExp(def)) {
        let rhs = L3applicativeEval(def.val, env);
        if (isError(rhs))
            return rhs;
        else {
            let newEnv = makeEnv(def.var.var, rhs, env);
            return evalExps(exps, newEnv);
        }
    } else {
        return Error("unexpected " + def);
    }
}

// Main program
export const evalL3program = (program: Program): Value | Error =>
    evalExps(program.exps, makeEmptyEnv());

export const evalParse = (s: string): Value | Error => {
    let ast: Parsed | Error = parseL3(s);
    if (isProgram(ast)) {
        return evalL3program(ast);
    } else if (isExp(ast)) {
        return evalExps([ast], makeEmptyEnv());
    } else {
        return ast;
    }
}

