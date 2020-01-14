// L4-eval.ts
// Evaluator with Environments model
// Support letrec with recursive environments (RecEnv)
// No support for set! 

import { map, reduce } from "ramda";
import { allT, first, rest, isBoolean, isEmpty, isNumber, isString } from "../shared/list";
import { getErrorMessages, hasNoError, isError }  from "../shared/error";
import { isBoolExp, isCExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef,
         isAppExp, isDefineExp, isExp, isIfExp, isLetrecExp, isLetExp, isProcExp, isProgram, 
         Binding, PrimOp, VarDecl, CExp, Exp, IfExp, LetrecExp, LetExp, Parsed, ProcExp, Program,
         parse } from "./L4-ast";
import { applyEnv, makeEmptyEnv, makeExtEnv, makeRecEnv, Env } from "./L4-env";
import { isEmptySExp, isSymbolSExp, isClosure, isCompoundSExp, makeClosure, makeCompoundSExp, Closure, 
         CompoundSExp, EmptySExp, makeEmptySExp, Value } from "./L4-value";

// ========================================================
// Eval functions

const applicativeEval = (exp: CExp | Error, env: Env): Value | Error =>
    isError(exp)  ? exp :
    isNumExp(exp) ? exp.val :
    isBoolExp(exp) ? exp.val :
    isStrExp(exp) ? exp.val :
    isPrimOp(exp) ? exp :
    isVarRef(exp) ? applyEnv(env, exp.var) :
    isLitExp(exp) ? exp.val :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? evalProc(exp, env) :
    isLetExp(exp) ? evalLet(exp, env) :
    isLetrecExp(exp) ? evalLetrec(exp, env) :
    isAppExp(exp) ? applyProcedure(applicativeEval(exp.rator, env),
                                   map((rand: CExp) => applicativeEval(rand, env),
                                        exp.rands)) :
    Error(`Bad L4 AST ${exp}`);

export const isTrueValue = (x: Value | Error): boolean | Error =>
    isError(x) ? x :
    ! (x === false);

const evalIf = (exp: IfExp, env: Env): Value | Error => {
    const test = applicativeEval(exp.test, env);
    return isError(test) ? test :
        isTrueValue(test) ? applicativeEval(exp.then, env) :
        applicativeEval(exp.alt, env);
};

const evalProc = (exp: ProcExp, env: Env): Closure =>
    makeClosure(exp.args, exp.body, env);

// @Pre: none of the args is an Error (checked in applyProcedure)
// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
const applyProcedure = (proc: Value | Error, args: Array<Value | Error>): Value | Error =>
    isError(proc) ? proc :
    !hasNoError(args) ? Error(`Bad argument: ${getErrorMessages(args)}`) :
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args) :
    Error(`Bad procedure ${JSON.stringify(proc)}`);

const applyClosure = (proc: Closure, args: Value[]): Value | Error => {
    let vars = map((v: VarDecl) => v.var, proc.params);
    return evalExps(proc.body, makeExtEnv(vars, args, proc.env));
}

// Evaluate a sequence of expressions (in a program)
export const evalExps = (exps: Exp[], env: Env): Value | Error =>
    isEmpty(exps) ? Error("Empty program") :
    isDefineExp(first(exps)) ? evalDefineExps(first(exps), rest(exps), env) :
    evalCExps(first(exps), rest(exps), env);
    
const evalCExps = (exp1: Exp, exps: Exp[], env: Env): Value | Error =>
    isCExp(exp1) && isEmpty(exps) ? applicativeEval(exp1, env) :
    isCExp(exp1) ? (isError(applicativeEval(exp1, env)) ? Error("error") :
                    evalExps(exps, env)) :
    Error("Never");
    
// Eval a sequence of expressions when the first exp is a Define.
// Compute the rhs of the define, extend the env with the new binding
// then compute the rest of the exps in the new env.
const evalDefineExps = (def: Exp, exps: Exp[], env: Env): Value | Error => {
    if (isDefineExp(def)) {
        let rhs = applicativeEval(def.val, env);
        if (isError(rhs))
            return rhs;
        else {
            let newEnv = makeExtEnv([def.var.var], [rhs], env);
            return evalExps(exps, newEnv);
        }
    } else {
        return Error("unexpected " + def);
    }
}

// Main program
export const evalProgram = (program: Program): Value | Error =>
    evalExps(program.exps, makeEmptyEnv());

export const evalParse = (s: string): Value | Error => {
    let ast: Parsed | Error = parse(s);
    if (isProgram(ast)) {
        return evalProgram(ast);
    } else if (isExp(ast)) {
        return evalExps([ast], makeEmptyEnv());
    } else {
        return ast;
    }
}

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env): Value | Error => {
    const vals = map((v: CExp) => applicativeEval(v, env), map((b: Binding) => b.val, exp.bindings));
    const vars = map((b: Binding) => b.var.var, exp.bindings);
    if (hasNoError(vals)) {
        return evalExps(exp.body, makeExtEnv(vars, vals, env));
    } else {
        return Error(getErrorMessages(vals));
    }
}

// LETREC: Direct evaluation rule without syntax expansion
// prepare the values as a RecEnv, eval the body (no eval needed for the vals).
const evalLetrec = (exp: LetrecExp, env: Env): Value | Error => {
    const vars = map((b: Binding) => b.var.var, exp.bindings);
    const vals = map((b: Binding) => b.val, exp.bindings);
    if (allT(isProcExp, vals)) {
        const paramss = map((v: ProcExp) => v.args, vals);
        const bodies = map((v: ProcExp) => v.body, vals);
        return evalExps(exp.body, makeRecEnv(vars, paramss, bodies, env));
    } else {
        return Error("Letrec: all variables must be bound to procedures");
    }
}

// ========================================================
// Primitives

const zero: number = 0;
const one: number = 1;

// @Pre: none of the args is an Error (checked in applyProcedure)
// TODO: Add explicit type checking in all primitives
export const applyPrimitive = (proc: PrimOp, args: Value[]): Value | Error =>
    proc.op === "+" ? (allT(isNumber, args) ? reduce((x: number, y: number) => x + y, zero, args) : Error("+ expects numbers only")) :
    proc.op === "-" ? minusPrim(args) :
    proc.op === "*" ? (allT(isNumber, args) ? reduce((x: number, y: number) => x * y, one, args) : Error("* expects numbers only")) :
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
    proc.op === "list?" ? isListPrim(args[0]) :
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

const isListPrim = (v: Value): boolean =>
    isEmptySExp(v) || isCompoundSExp(v);

const isPairPrim = (v: Value): boolean =>
    isCompoundSExp(v);

