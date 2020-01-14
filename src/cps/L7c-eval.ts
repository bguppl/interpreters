// L7c-eval: CPS version of L5 with concrete data-structure continuations
// and registerization

import { map, reduce, repeat, zipWith, join } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         Parsed, PrimOp, Program, SetExp, VarDecl, makeProgram } from '../L5/L5-ast';
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { parse, unparse } from "../L5/L5-ast";
import { isBoolean, isEmpty, isNumber, isString } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isProgram, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, ExtEnv } from "../L5/L5-env";
import { isEmptySExp, isSymbolSExp, makeEmptySExp, parsedToString, EmptySExp, valueToString } from '../L5/L5-value';
import { isClosure, isCompoundSExp, makeClosure, makeCompoundSExp, CompoundSExp, Value } from "../L5/L5-value";
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

export type InstructionSet =
    'applyCont' | 'applyContArray' | 'halt' |
    'applyIfCont' | 'applyFirstCont' | 'applySetCont' |
    'applyLetCont' | 'applyLetrecCont' | 'applyAppCont2' | 'applyExpsCont2' |
    'applyAppCont1' | 'applyExpsCont1' | 'applyDefCont' |
    'applyTopCont' | 'evalCont' | 'evalIf' | 'evalProc' | 'evalApp' | 'evalSet' |
    'evalLet' | 'evalLetrec' | 'evalSequence' | 'evalSequenceFR' |
    'evalExps' | 'evalExpsFR' |
    'evalDefineExps' |
    'applyProcedure' | 'applyClosure';

// ============================================================================
// REGISTERS
// Define global variable for registers - one for all the parameters of the functions
// involved in the interpreter.
export let contREG: Cont;
export let contArrayREG: ContArray | undefined;
export let valREG: Value | Error | undefined;
export let valsREG: Array<Value | Error> | undefined;
export let expREG: Exp;
export let expsREG: Exp[];
export let envREG: Env;
export let pcREG: InstructionSet;

export let TRACE = false;
export let MAXCOUNT = 10000000;

// ============= Debug utilities ===================
// Safe ways to print values, expressions, continuations registries
// taking into account undefined, Error and circular structures.

const up = (exp: Exp): string => {
    const u = unparse(exp);
    return isError(u) ? u.message : u
};

const pe = (exp: Exp | Error | undefined): string =>
    exp === undefined ? "undefined" :
    isExp(exp) ? up(exp) :
    isError(exp) ? exp.message :
    "undefined";

const pes = (es: Array<Exp | Error> | undefined): string =>
    es === undefined ? "undefined" :
    isEmpty(es) ? "[]" :
    join(",", map(pe, es));

const pc = (c: Cont | ContArray | undefined): string =>
    c === undefined ? "undefined" : c.tag;

const pv = (v: Value | Error | undefined): string => {
    return v === undefined ? "undefined" :
    isError(v) ? v.message :
    valueToString(v);
}

const pvs = (vs: Array<Value | Error> | undefined): string =>
    vs === undefined ? "undefined" :
    isEmpty(vs) ? "[]" :
    join(",", map(pv, vs));

export const dumpREG = () => {
    if (TRACE) {
        console.log(`In ${pcREG}:`);
        console.log(valREG);
        console.log(`valREG = ${pv(valREG)}`);
        console.log(`valsREG = ${pvs(valsREG)}`);
        console.log(`expREG = ${pe(expREG)}`);
        console.log(`expsREG = ${pes(expsREG)}`);
        console.log(`contREG = ${pc(contREG)}`);
        console.log(`contArrayREG = ${pc(contArrayREG)}`);
        console.log(`\n`);
    }
}

// type Cont = IfCont | FirstCont | SetCont | AppCont1 | ExpsCont1 | DefCont | TopCont;
// const applyContVal = (cont: ContVal, val: Value | Error): Value | Error =>
export const applyCont = (): void => {
    dumpREG();
    pcREG =
    isIfCont(contREG) ? 'applyIfCont' :
    isFirstCont(contREG) ? 'applyFirstCont' :
    isSetCont(contREG) ? 'applySetCont' :
    isAppCont1(contREG) ? 'applyAppCont1' :
    isExpsCont1(contREG) ? 'applyExpsCont1' :
    isDefCont(contREG) ? 'applyDefCont' :
    isTopCont(contREG) ? 'applyTopCont' :
    (console.error(`1 Unknown cont ${JSON.stringify(contREG)}`), 'halt');
}

// type ContArray = LetCont | LetrecCont | AppCont2 | ExpsCont2;
// const applyContArray = (cont: ContArray, vals: Array<Value | Error>): Value | Error =>
export const applyContArray = (): void => {
    dumpREG();
    pcREG =
    isLetCont(contArrayREG) ? 'applyLetCont' :
    isLetrecCont(contArrayREG) ? 'applyLetrecCont' :
    isAppCont2(contArrayREG) ? 'applyAppCont2' :
    isExpsCont2(contArrayREG) ? 'applyExpsCont2' :
    (console.error(`2 Unknown contArray ${JSON.stringify(contArrayREG)}`), 'halt');
}

// const applyTopCont = (cont: TopCont, val: Value | Error): Value | Error => {
export const applyTopCont = (): void => {
    // console.log(parsedToString(valREG));
    pcREG = 'halt';
}

// const applyIfCont = (cont: IfCont, val: Value | Error): Value | Error =>
export const applyIfCont = (): void => {
    dumpREG();
    if (isIfCont(contREG)) {
        if (isError(valREG)) {
            contREG = contREG.cont;
            pcREG = 'applyCont';
        } else if (isTrueValue(valREG)) {
            expREG = contREG.exp.then;
            envREG = contREG.env;
            contREG = contREG.cont;
            pcREG = 'evalCont';
        } else {
            expREG = contREG.exp.alt;
            envREG = contREG.env;
            contREG = contREG.cont;
            pcREG = 'evalCont';
        }
    } else {
        console.error(`3 Unknown cont ${JSON.stringify(contREG)}`);
        pcREG = 'halt';
    }
}

// const applyLetCont = (cont: LetCont, vals: Array<Value | Error>): Value | Error =>
export const applyLetCont = (): void => {
    dumpREG();
    if (isLetCont(contArrayREG)) {
        if (valsREG === undefined) {
            console.log(`Empty valsREG in applyLetCont`);
            return;
        }
        if (hasNoError(valsREG)) {
            expsREG = contArrayREG.exp.body;
            envREG = makeExtEnv(letVars(contArrayREG.exp), valsREG, contArrayREG.env);
            contREG = contArrayREG.cont;
            pcREG = 'evalSequence';
        } else {
            valREG = Error(getErrorMessages(valsREG));
            contREG = contArrayREG.cont;
            pcREG = 'applyCont';
        }
    } else {
        console.error(`Unknown cont ${JSON.stringify(contArrayREG)}`);
        pcREG = 'halt';
    }
}

// export const applyFirstCont = (cont: FirstCont, val: Value | Error): Value | Error =>
export const applyFirstCont = (): void => {
    dumpREG();
    if (isFirstCont(contREG)) {
        if (isError(valREG)) {
            contREG = contREG.cont;
            pcREG = 'applyCont';
        } else {
            expsREG = contREG.exps;
            envREG = contREG.env;
            contREG = contREG.cont;
            pcREG = 'evalSequence';
        }
    } else {
        console.error(`Unknown cont ${JSON.stringify(contREG)}`);
        pcREG = 'halt';
    }
}

// export const applyLetrecCont = (cont: LetrecCont, vals: Array<Value | Error>): Value | Error => {
export const applyLetrecCont = (): void => {
    dumpREG();
    if (isLetrecCont(contArrayREG)) {
        if (valsREG === undefined) {
            console.log(`Empty valsREG in applyLetrecCont`);
            return;
        }
        if (hasNoError(valsREG)) {
            // Bind vars in extEnv to the new values
            zipWith((bdg, cval) => setFBinding(bdg, cval), contArrayREG.env.frame.fbindings, valsREG);
            expsREG = contArrayREG.exp.body;
            envREG = contArrayREG.env;
            contREG = contArrayREG.cont;
            pcREG = 'evalSequence';
        } else {
            valREG = Error(getErrorMessages(valsREG));
            contREG = contArrayREG.cont;
            pcREG = 'applyCont';
        }
    } else {
        console.error(`Unknown cont ${JSON.stringify(contArrayREG)}`);
        pcREG = 'halt';
    }
}

// export const applySetCont = (cont: SetCont, val: Value | Error): Value | Error => {
export const applySetCont = (): void => {
    dumpREG();
    if (isSetCont(contREG)) {
        if (isError(valREG)) {
            contREG = contREG.cont;
            pcREG = 'applyCont';
        } else {
            const v = contREG.exp.var.var;
            const bdg = applyEnvBdg(contREG.env, v);
            if (isError(bdg)) {
                valREG = Error(`Var not found ${v}`);
                contREG = contREG.cont;
                pcREG = 'applyCont';
            } else {
                setFBinding(bdg, valREG);
                valREG = undefined;
                contREG = contREG.cont;
                pcREG = 'applyCont';
            }
        }
    } else {
        console.error(`Unknown cont ${JSON.stringify(contREG)}`);
        pcREG = 'halt';
    }
}

// export const applyAppCont1 = (cont: AppCont1, val: Value | Error): Value | Error =>
export const applyAppCont1 = (): void => {
    dumpREG();
    if (isAppCont1(contREG)) {
        expsREG = contREG.exp.rands;
        envREG = contREG.env;
        contArrayREG = makeAppCont2(valREG, contREG.env, contREG.cont)
        pcREG = 'evalExps';
    } else {
        console.error(`Unknown cont ${JSON.stringify(contREG)}`);
        pcREG = 'halt';
    }
}

// export const applyAppCont2 = (cont: AppCont2, vals: Array<Value | Error>): Value | Error =>
export const applyAppCont2 = (): void => {
    dumpREG();
    if (isAppCont2(contArrayREG)) {
        valREG = contArrayREG.proc;
        contREG = contArrayREG.cont;
        pcREG = 'applyProcedure';
    } else {
        console.error(`Unknown cont ${JSON.stringify(contArrayREG)}`);
        pcREG = 'halt';
    }
}

// export const applyExpsCont1 = (cont: ExpsCont1, val: Value | Error): Value | Error =>
export const applyExpsCont1 = (): void => {
    dumpREG();
    if (isExpsCont1(contREG)) {
        if (isError(valREG)) {
            contArrayREG = contREG.cont;
            valsREG = [valREG];
            pcREG = 'applyContArray';
        } else {
            expsREG = contREG.exps;
            envREG = contREG.env;
            contArrayREG = makeExpsCont2(valREG, contREG.cont);
            pcREG = 'evalExps';
        }
    } else {
        console.error(`Unknown cont ${JSON.stringify(contREG)}`);
        pcREG = 'halt';
    }
}

// export const applyExpsCont2 = (cont: ExpsCont2, vals: Array<Value | Error>): Value | Error =>
export const applyExpsCont2 = (): void => {
    dumpREG();
    if (valsREG === undefined) {
        console.log(`Empty valsREG in applyExpsCont2`);
        return;
    }
    if (isExpsCont2(contArrayREG)) {
        valsREG = [contArrayREG.firstVal, ...valsREG];
        contArrayREG = contArrayREG.cont;
        pcREG = 'applyContArray';
    } else {
        console.error(`Unknown cont ${JSON.stringify(contArrayREG)}`);
        pcREG = 'halt';
    }
}

// export const applyDefCont = (cont: DefCont, val: Value | Error): Value | Error => {
export const applyDefCont = (): void => {
    dumpREG();
    if (isDefCont(contREG)) {
        if (isError(valREG)) {
            contREG = contREG.cont;
            pcREG = 'applyCont';
        } else {
            globalEnvAddBinding(contREG.exp.var.var, valREG);
            expsREG = contREG.exps;
            envREG = theGlobalEnv;
            contREG = contREG.cont;
            pcREG = 'evalSequence';
        }
    } else {
        console.error(`Unknown cont ${JSON.stringify(contREG)}`);
        pcREG = 'halt';
    }
}

// ========================================================
// Eval functions

// export const evalCont = (exp: CExp | Error, env: Env, cont: Cont): Value | Error =>
export const evalCont = (): void => {
    dumpREG();
    if (isError(expREG)) {
        valREG = expREG;
        pcREG = 'applyCont';
    } else if (isNumExp(expREG)) {
        valREG = expREG.val;
        pcREG = 'applyCont';
    } else if (isBoolExp(expREG)) {
        valREG = expREG.val;
        pcREG = 'applyCont';
    } else if (isStrExp(expREG)) {
        valREG = expREG.val;
        pcREG = 'applyCont';
    } else if (isPrimOp(expREG)) {
        valREG = expREG;
        pcREG = 'applyCont';
    } else if (isVarRef(expREG)) {
        valREG = applyEnv(envREG, expREG.var);
        pcREG = 'applyCont';
    } else if (isLitExp(expREG)) {
        valREG = expREG.val;
        pcREG = 'applyCont';
    } else if (isIfExp(expREG)) {
        pcREG = 'evalIf';
    } else if (isProcExp(expREG)) {
        pcREG = 'evalProc';
    } else if (isLetExp(expREG)) {
        pcREG = 'evalLet';
    } else if (isLetrecExp(expREG)) {
        pcREG = 'evalLetrec';
    } else if (isSetExp(expREG)) {
        pcREG = 'evalSet';
    } else if (isAppExp(expREG)) {
        pcREG = 'evalApp';
    } else {
        valREG = Error(`Bad L5 AST ${expREG}`);
        pcREG = 'applyCont';
    }
}

export const isTrueValue = (x: Value | Error): boolean | Error =>
    isError(x) ? x :
    ! (x === false);

// const evalIf = (exp: IfExp, env: Env, cont: Cont): Value | Error =>
export const evalIf = (): void => {
    dumpREG();
    if (isIfExp(expREG)) {
        contREG = makeIfCont(expREG, envREG, contREG);
        expREG = expREG.test;
        pcREG = 'evalCont';
    } else {
        valREG = Error(`Bad expREG in evalIf ${expREG}`);
        pcREG = 'halt';
    }
}

// const evalProc = (exp: ProcExp, env: Env, cont: Cont): Value | Error =>
export const evalProc = (): void => {
    dumpREG();
    if (isProcExp(expREG)) {
        valREG = makeClosure(expREG.args, expREG.body, envREG);
        pcREG = 'applyCont';
    } else {
        valREG = Error(`Bad expREG in evalProc ${expREG}`);
        pcREG = 'halt';
    }
}

// Return the vals (rhs) of the bindings of a let expression
const letVals = (exp: LetExp | LetrecExp): CExp[] =>
        map((b) => b.val, exp.bindings);

// Return the vars (lhs) of the bindings of a let expression
const letVars = (exp: LetExp | LetrecExp): string[] =>
        map((b) => b.var.var, exp.bindings);

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
// const evalLet = (exp: LetExp, env: Env, cont: Cont): Value | Error =>
export const evalLet = (): void => {
    dumpREG();
    if (isLetExp(expREG)) {
        expsREG = letVals(expREG);
        contArrayREG = makeLetCont(expREG, envREG, contREG);
        pcREG = 'evalExps';
    } else {
        valREG = Error(`Bad expREG in evalLet ${expREG}`);
        pcREG = 'halt';
    }
}

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
// export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Value | Error =>
export const evalSequence = (): void => {
    dumpREG();
    if (isEmpty(expsREG)) {
        valREG = Error("Empty Sequence");
        pcREG = 'applyCont';
    } else {
        expREG = first(expsREG);
        expsREG = rest(expsREG);
        pcREG = 'evalSequenceFR';
    }
}

// const evalSequenceFR = (exp: Exp, exps: Exp[], env: Env, cont: Cont): Value | Error =>
const evalSequenceFR = (): void => {
    dumpREG();
    if (isDefineExp(expREG)) {
        pcREG = 'evalDefineExps';
    } else if (isEmpty(expsREG)) {
        pcREG = 'evalCont';
    } else {
        contREG = makeFirstCont(expsREG, envREG, contREG);
        pcREG = 'evalCont';
    }
}

// LETREC: Direct evaluation rule without syntax expansion
// 1. extend the env with vars initialized to void (temporary value)
// 2. compute the vals in the new extended env
// 3. update the bindings of the vars to the computed vals
// 4. compute body in extended env
// export const evalLetrec = (exp: LetrecExp, env: Env, cont: Cont): Value | Error => {
export const evalLetrec = (): void => {
    dumpREG();
    if (isLetrecExp(expREG)) {
        const vars = letVars(expREG);
        const vals = letVals(expREG);
        const extEnv = makeExtEnv(vars, repeat(undefined, vars.length), envREG);
        // Compute the vals in the extended env
        expsREG = vals;
        envREG = extEnv;
        contArrayREG = makeLetrecCont(expREG, extEnv, contREG);
        pcREG = 'evalExps';
    } else {
        valREG = Error(`Bad expREG in evalLetrec ${expREG}`);
        pcREG = 'halt';
    }
}

// Handling of mutation with set!
// export const evalSet = (exp: SetExp, env: Env, cont: Cont): Value | Error =>
export const evalSet = (): void => {
    dumpREG();
    if (isSetExp(expREG)) {
        contREG = makeSetCont(expREG, envREG, contREG);
        expREG = expREG.val;
        pcREG = 'evalCont';
    } else {
        valREG = Error(`Bad expREG in evalSet ${expREG}`);
        pcREG = 'halt';
    }
}

// export const evalApp = (exp: AppExp, env: Env, cont: Cont): Value | Error =>
export const evalApp = (): void => {
    dumpREG();
    if (isAppExp(expREG)) {
        contREG = makeAppCont1(expREG, envREG, contREG);
        expREG = expREG.rator;
        pcREG = 'evalCont';
    } else {
        valREG = Error(`Bad expREG in evalApp ${expREG}`);
        pcREG = 'halt';
    }
}

// export const applyProcedure = (proc: Value | Error, args: Array<Value | Error>, cont: Cont): Value | Error =>
// Arguments in valREG, valsREG, contREG
export const applyProcedure = (): void => {
    dumpREG();
    if (valsREG === undefined) {
        console.log(`Empty valsREG in applyProcedure`);
        return;
    }
    if (isError(valREG)) {
        pcREG = 'applyCont';
    } else if (!hasNoError(valsREG)) {
        valREG = Error(`Bad argument: ${getErrorMessages(valsREG)}`);
        pcREG = 'applyCont';
    } else if (isPrimOp(valREG)) {
        valREG = applyPrimitive(valREG, valsREG);
        pcREG = 'applyCont';
    } else if (isClosure(valREG)) {
        pcREG = 'applyClosure';
    } else {
        valREG = Error(`Bad procedure ${JSON.stringify(valREG)}`);
        pcREG = 'applyCont';
    }
}

// export const applyClosure = (proc: Closure, args: Value[], cont: Cont): Value | Error => {
// Parameters in proc = valREG, args = valsREG
export const applyClosure = (): void => {
    dumpREG();
    if (isClosure(valREG)) {
        let vars = map((v: VarDecl) => v.var, valREG.params);
        expsREG = valREG.body;
        // This noError was tested in applyProcedure
        // but we don't keep the type across procedures
        if (valsREG === undefined) {
            console.log(`Empty valsREG in applyClosure`);
            return;
        }
        if (hasNoError(valsREG)) {
            envREG = makeExtEnv(vars, valsREG, valREG.env);
            pcREG = 'evalSequence';
        } else {
            valREG = Error(`Bad argument: ${getErrorMessages(valsREG)}`);
            pcREG = 'applyCont';
        }
    } else {
        valREG = Error(`Bad expREG in evalApp ${expREG}`);
        pcREG = 'halt';
    }
}

// Evaluate an array of expressions - pass the result as an array to the continuation
// export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Value | Error =>
export const evalExps = (): void => {
    dumpREG();
    if (isEmpty(expsREG)) {
        valsREG = [];
        pcREG = 'applyContArray';
    } else {
        expREG = first(expsREG);
        expsREG = rest(expsREG);
        pcREG = 'evalExpsFR';
    }
}

// const evalExpsFR = (exp: Exp, exps: Exp[], env: Env, cont: ContArray): Value | Error =>
const evalExpsFR = (): void => {
    dumpREG();
    if (isDefineExp(expREG)) {
        valsREG = [Error("Unexpected define: "+unparse(expREG))];
        pcREG = 'applyContArray';
    } else {
        if (contArrayREG === undefined) {
            console.error(`undefined contArrayREG`);
            return;
        }
        contREG = makeExpsCont1(expsREG, envREG, contArrayREG);
        pcREG = 'evalCont';
    }
}

// define always updates theGlobalEnv
// We only expect defineExps at the top level.
// const evalDefineExps = (exp: DefineExp, exps: Exp[], cont: Cont): Value | Error =>
export const evalDefineExps = (): void => {
    dumpREG();
    if (isDefineExp(expREG)) {
        contREG = makeDefCont(expREG, expsREG, contREG);
        expREG = expREG.val;
        envREG = theGlobalEnv;
        pcREG = 'evalCont';
    } else {
        valREG = Error(`Bad expREG in evalDefine ${expREG}`);
        pcREG = 'halt';
    }
}

// Evaluate a program
// Main program - EVALUATION LOOP of the Virtual Machine
export const evalProgram = (program: Program): Value | Error => {
    valREG = undefined;
    valsREG = undefined;
    expsREG = program.exps;
    envREG = theGlobalEnv;
    contREG = makeTopCont();
    contArrayREG = undefined;
    pcREG = 'evalSequence';
    let count = 0;
    while (true) {
        count++;
        if (pcREG === 'evalSequence') evalSequence();
        else if (pcREG === 'evalSequenceFR') evalSequenceFR();
        else if (pcREG === 'halt') break;
        else if (pcREG === 'applyCont') applyCont();
        else if (pcREG === 'applyContArray') applyContArray();
        else if (pcREG === 'applyIfCont') applyIfCont();
        else if (pcREG === 'applyFirstCont') applyFirstCont();
        else if (pcREG === 'applySetCont') applySetCont();
        else if (pcREG === 'applyLetCont') applyLetCont();
        else if (pcREG === 'applyLetrecCont') applyLetrecCont();
        else if (pcREG === 'applyAppCont2') applyAppCont2();
        else if (pcREG === 'applyExpsCont2') applyExpsCont2();
        else if (pcREG === 'applyAppCont1') applyAppCont1();
        else if (pcREG === 'applyExpsCont1') applyExpsCont1();
        else if (pcREG === 'applyDefCont') applyDefCont();
        else if (pcREG === 'applyTopCont') applyTopCont();
        else if (pcREG === 'evalCont') evalCont();
        else if (pcREG === 'evalIf') evalIf();
        else if (pcREG === 'evalApp') evalApp();
        else if (pcREG === 'evalProc') evalProc();
        else if (pcREG === 'evalSet') evalSet();
        else if (pcREG === 'evalLet') evalLet();
        else if (pcREG === 'evalLetrec') evalLetrec();
        else if (pcREG === 'evalExps') evalExps();
        else if (pcREG === 'evalExpsFR') evalExpsFR();
        else if (pcREG === 'evalDefineExps') evalDefineExps();
        else if (pcREG === 'applyProcedure') applyProcedure();
        else if (pcREG === 'applyClosure') applyClosure();
        else {
            console.error(`Bad instruction: ${pcREG}`);
            break;
        }
        if (count > MAXCOUNT) {
            console.error(`STOP: ${count} instructions`);
            dumpREG();
            break;
        }
    }
    return valREG;
}

export const evalParse = (s: string): Value | Error => {
    let ast: Parsed | Error = parse(s);
    if (isProgram(ast)) {
        return evalProgram(ast);
    } else if (isExp(ast)) {
        return evalProgram(makeProgram([ast]));
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

