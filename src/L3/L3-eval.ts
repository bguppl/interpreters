// L3-eval.ts
import p from "s-expression";
import { filter, indexOf, map, reduce, contains, zip, KeyValuePair } from "ramda";
import { isCExp, isLetExp } from "./L3-ast";
import { BoolExp, CExp, Exp, IfExp, LitExp, NumExp,
         PrimOp, ProcExp, Program, StrExp, VarDecl, VarRef } from "./L3-ast";
import { isAppExp, isBoolExp, isDefineExp, isExp, isIfExp, isLitExp, isNumExp,
             isPrimOp, isProcExp, isProgram, isStrExp, isVarRef } from "./L3-ast";
import { makeAppExp, makeBoolExp, makeIfExp, makeLitExp, makeNumExp, makeProcExp, makeStrExp,
         makeVarDecl, makeVarRef } from "./L3-ast";
import { parseL3Exp } from "./L3-ast";
import { applyEnv, makeEmptyEnv, makeEnv, Env } from "./L3-env";
import { isClosure, isCompoundSExp, isEmptySExp, isSymbolSExp,
         makeClosure, makeCompoundSExp, makeEmptySExp, 
         Closure, CompoundSExp, EmptySExp, Value } from "./L3-value";
import { allT, first, rest, second, isEmpty } from '../shared/list';
import { isBoolean, isNumber, isString } from "../shared/type-predicates";
import { Result, makeOk, makeFailure, bind, safe2, mapResult } from "../shared/result";

// ========================================================
// Eval functions

const L3applicativeEval = (exp: CExp, env: Env): Result<Value> =>
    isNumExp(exp) ? makeOk(exp.val) : 
    isBoolExp(exp) ? makeOk(exp.val) :
    isStrExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isLitExp(exp) ? makeOk(exp.val) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? makeOk(evalProc(exp, env)) :
    isAppExp(exp) ? safe2((rator: Value, rands: Value[]) => L3applyProcedure(rator, rands, env))
        (L3applicativeEval(exp.rator, env), mapResult(rand => L3applicativeEval(rand, env), exp.rands)) :
    isLetExp(exp) ? makeFailure('"let" not supported (yet)') :
    makeFailure(`Bad L3 AST ${exp}`);

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

const evalIf = (exp: IfExp, env: Env): Result<Value> =>
    bind(L3applicativeEval(exp.test, env),
         (test: Value) => isTrueValue(test) ? L3applicativeEval(exp.then, env) : L3applicativeEval(exp.alt, env));

const evalProc = (exp: ProcExp, env: Env): Value =>
    makeClosure(exp.args, exp.body);

const L3applyProcedure = (proc: Value, args: Value[], env: Env): Result<Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args, env) :
    makeFailure("Bad procedure " + JSON.stringify(proc))

const valueToLitExp = (v: Value): NumExp | BoolExp | StrExp | LitExp | PrimOp | ProcExp =>
    isNumber(v) ? makeNumExp(v) :
    isBoolean(v) ? makeBoolExp(v) :
    isString(v) ? makeStrExp(v) :
    isPrimOp(v) ? v :
    isClosure(v) ? makeProcExp(v.params, v.body) :
    makeLitExp(v);

const applyClosure = (proc: Closure, args: Value[], env: Env): Result<Value> => {
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
export const applyPrimitive = (proc: PrimOp, args: Value[]): Result<Value> =>
    proc.op === "+" ? (allT(isNumber, args) ? makeOk(reduce((x, y) => x + y, 0, args)) : makeFailure("+ expects numbers only")) :
    proc.op === "-" ? minusPrim(args) :
    proc.op === "*" ? (allT(isNumber, args) ? makeOk(reduce((x, y) => x * y, 1, args)) : makeFailure("* expects numbers only")) :
    proc.op === "/" ? divPrim(args) :
    proc.op === ">" ? makeOk(args[0] > args[1]) :
    proc.op === "<" ? makeOk(args[0] < args[1]) :
    proc.op === "=" ? makeOk(args[0] === args[1]) :
    proc.op === "not" ? makeOk(! args[0]) :
    proc.op === "and" ? makeOk(isBoolean(args[0]) && isBoolean(args[1]) && args[0] && args[1]) :
    proc.op === "or" ? makeOk(isBoolean(args[0]) && isBoolean(args[1]) && args[0] || args[1]) :
    proc.op === "eq?" ? makeOk(eqPrim(args)) :
    proc.op === "string=?" ? makeOk(args[0] === args[1]) :
    proc.op === "cons" ? makeOk(consPrim(args[0], args[1])) :
    proc.op === "car" ? carPrim(args[0]) :
    proc.op === "cdr" ? cdrPrim(args[0]) :
    proc.op === "list" ? makeOk(listPrim(args)) :
    proc.op === "pair?" ? makeOk(isPairPrim(args[0])) :
    proc.op === "number?" ? makeOk(typeof(args[0]) === 'number') :
    proc.op === "boolean?" ? makeOk(typeof(args[0]) === 'boolean') :
    proc.op === "symbol?" ? makeOk(isSymbolSExp(args[0])) :
    proc.op === "string?" ? makeOk(isString(args[0])) :
    makeFailure("Bad primitive op " + proc.op);

const minusPrim = (args: Value[]): Result<number> => {
    // TODO complete
    let x = args[0], y = args[1];
    if (isNumber(x) && isNumber(y)) {
        return makeOk(x - y);
    } else {
        return makeFailure(`Type error: - expects numbers ${args}`)
    }
}

const divPrim = (args: Value[]): Result<number> => {
    // TODO complete
    let x = args[0], y = args[1];
    if (isNumber(x) && isNumber(y)) {
        return makeOk(x / y);
    } else {
        return makeFailure(`Type error: / expects numbers ${args}`)
    }
}

const eqPrim = (args: Value[]): boolean => {
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

const carPrim = (v: Value): Result<Value> =>
    isCompoundSExp(v) ? makeOk(v.val1) :
    makeFailure(`Car: param is not compound ${v}`);

const cdrPrim = (v: Value): Result<Value> =>
    isCompoundSExp(v) ? makeOk(v.val2) :
    makeFailure(`Cdr: param is not compound ${v}`);

const consPrim = (v1: Value, v2: Value): CompoundSExp =>
    makeCompoundSExp(v1, v2);

export const listPrim = (vals: Value[]): EmptySExp | CompoundSExp =>
    vals.length === 0 ? makeEmptySExp() :
    makeCompoundSExp(first(vals), listPrim(rest(vals)))

const isPairPrim = (v: Value): boolean =>
    isCompoundSExp(v);

// Evaluate a sequence of expressions (in a program)
export const evalExps = (exps: Exp[], env: Env): Result<Value> =>
    isEmpty(exps) ? makeFailure("Empty program") :
    isDefineExp(first(exps)) ? evalDefineExps(first(exps), rest(exps), env) :
    evalCExps(first(exps), rest(exps), env);

const evalCExps = (exp1: Exp, exps: Exp[], env: Env): Result<Value> =>
    isCExp(exp1) && isEmpty(exps) ? L3applicativeEval(exp1, env) :
    isCExp(exp1) ? bind(L3applicativeEval(exp1, env), _ => evalExps(exps, env)) :
    makeFailure("Never");

// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: Exp, exps: Exp[], env: Env): Result<Value> =>
    isDefineExp(def) ? bind(L3applicativeEval(def.val, env),
                            (rhs: Value) => evalExps(exps, makeEnv(def.var.var, rhs, env))) :
    makeFailure("Unexpected " + def);

// Main program
export const evalL3program = (program: Program): Result<Value> =>
    evalExps(program.exps, makeEmptyEnv());

export const evalParse = (s: string): Result<Value> =>
    bind(parseL3Exp(p(s)),
         (exp: Exp) => evalExps([exp], makeEmptyEnv()));