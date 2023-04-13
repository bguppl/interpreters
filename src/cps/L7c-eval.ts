// L7c-eval: CPS version of L5 with concrete data-structure continuations
// and registerization

import { map, repeat, zipWith, join, identity } from "ramda";
import { AppExp, CExp, DefineExp, Exp, IfExp, LetrecExp, LetExp,
         Program, SetExp, VarDecl, makeProgram } from '../L5/L5-ast';
import { isBoolExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef } from "../L5/L5-ast";
import { parseL5Exp, unparse } from "../L5/L5-ast";
import { isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp,
         isProcExp, isSetExp } from "../L5/L5-ast";
import { applyEnv, applyEnvBdg, globalEnvAddBinding, makeExtEnv, setFBinding,
         theGlobalEnv, Env, ExtEnv } from "../L5/L5-env";
import { valueToString } from '../L5/L5-value';
import { isClosure, makeClosure, Value } from "../L5/L5-value";
import { isEmpty, first, rest } from '../shared/list';
import { Result, either, makeOk, bind, makeFailure } from "../shared/result";
import { parse as p } from "../shared/parser";
import { applyPrimitive } from "../L5/evalPrimitive";
import { format } from "../shared/format";

// ========================================================
// Concrete Continuation datatype
// type Cont = (res: Value | Error) => Value | Error;
// type ContArray = (results: Array<Value | Error>) => Value | Error;
export type Cont = IfCont | FirstCont | SetCont | AppCont1 | ExpsCont1 | DefCont | TopCont;
export type ContArray = LetCont | LetrecCont | AppCont2 | ExpsCont2;

export const isCont = (x: any): x is Cont =>
    isIfCont(x) || isFirstCont(x) || isSetCont(x) ||
    isAppCont1(x) || isExpsCont1(x) || isDefCont(x) || isTopCont(x);
export const isContArray = (x: any): x is ContArray =>
    isLetCont(x) || isLetrecCont(x) || isAppCont2(x) || isExpsCont2(x);

export type TopCont = {tag: "TopCont"}
export const makeTopCont = (): TopCont => ({tag: "TopCont"});
export const isTopCont = (x: any): x is TopCont => x.tag === "TopCont";

export type IfCont = {tag: "IfCont", exp: IfExp, env: Env, cont: Cont}
export const makeIfCont = (exp: IfExp, env: Env, cont: Cont): IfCont => ({tag: "IfCont", env: env, exp: exp, cont: cont});
export const isIfCont = (x: any): x is IfCont => x.tag === "IfCont";

export type FirstCont = {tag: "FirstCont", exps: Exp[], env: Env, cont: Cont}
export const makeFirstCont = (exps: Exp[], env: Env, cont: Cont): FirstCont => ({tag: "FirstCont", env: env, exps: exps, cont: cont});
export const isFirstCont = (x: any): x is FirstCont => x.tag === "FirstCont";

export type SetCont = {tag: "SetCont", exp: SetExp, env: Env, cont: Cont}
export const makeSetCont = (exp: SetExp, env: Env, cont: Cont): SetCont => ({tag: "SetCont", env: env, exp: exp, cont: cont});
export const isSetCont = (x: any): x is SetCont => x.tag === "SetCont";

export type AppCont1 = {tag: "AppCont1", exp: AppExp, env: Env, cont: Cont}
export const makeAppCont1 = (exp: AppExp, env: Env, cont: Cont): AppCont1 => ({tag: "AppCont1", env: env, exp: exp, cont: cont});
export const isAppCont1 = (x: any): x is AppCont1 => x.tag === "AppCont1";

export type ExpsCont1 = {tag: "ExpsCont1", exps: Exp[], env: Env, cont: ContArray}
export const makeExpsCont1 = (exps: Exp[], env: Env, cont: ContArray): ExpsCont1 => ({tag: "ExpsCont1", env: env, exps: exps, cont: cont});
export const isExpsCont1 = (x: any): x is ExpsCont1 => x.tag === "ExpsCont1";

export type LetCont = {tag: "LetCont", exp: LetExp, env: Env, cont: Cont}
export const makeLetCont = (exp: LetExp, env: Env, cont: Cont): LetCont => ({tag: "LetCont", env: env, exp: exp, cont: cont});
export const isLetCont = (x: any): x is LetCont => x.tag === "LetCont";

export type LetrecCont = {tag: "LetrecCont", exp: LetrecExp, env: ExtEnv, cont: Cont}
export const makeLetrecCont = (exp: LetrecExp, env: ExtEnv, cont: Cont): LetrecCont => ({tag: "LetrecCont", env: env, exp: exp, cont: cont});
export const isLetrecCont = (x: any): x is LetrecCont => x.tag === "LetrecCont";

export type AppCont2 = {tag: "AppCont2", proc: Result<Value>, env: Env, cont: Cont}
export const makeAppCont2 = (proc: Result<Value>, env: Env, cont: Cont): AppCont2 => ({tag: "AppCont2", proc: proc, env: env, cont: cont});
export const isAppCont2 = (x: any): x is AppCont2 => x.tag === "AppCont2";

export type ExpsCont2 = {tag: "ExpsCont2", firstVal: Value, cont: ContArray}
export const makeExpsCont2 = (firstVal: Value, cont: ContArray): ExpsCont2 => ({tag: "ExpsCont2", firstVal: firstVal, cont: cont});
export const isExpsCont2 = (x: any): x is ExpsCont2 => x.tag === "ExpsCont2";

export type DefCont = {tag: "DefCont", exp: DefineExp, exps: Exp[], cont: Cont}
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
export let valREG: Result<Value> | undefined;
export let valsREG: Result<Value[]> | undefined;
export let expREG: Exp;
export let expsREG: Exp[];
export let envREG: Env;
export let pcREG: InstructionSet;

export let TRACE = false;
export let MAXCOUNT = 10000000;

// ============= Debug utilities ===================
// Safe ways to print values, expressions, continuations registries
// taking into account undefined, Error and circular structures.

const up = (exp: Exp | undefined): string =>
    exp ? either(unparse(exp), identity, identity) : "undefined";

const pes = (es: Exp[]): string =>
    `[${join(", ", map(up, es))}]`;

const pc = (c: Cont | ContArray | undefined): string =>
    c === undefined ? "undefined" : c.tag;

const pv = (v: Result<Value> | undefined): string =>
    v ? either(v, valueToString, identity) : "undefined";

const pvs = (vs: Result<Value[]> | undefined): string =>
    vs ? either(vs, values => `[${join(", ", map(pv, map(makeOk, values)))}]`, identity) : "undefined";

export const dumpREG = () => {
    if (TRACE) {
        console.log(`In ${pcREG}:`);
        // console.log(valREG);
        console.log(`valREG = ${pv(valREG)}`);
        console.log(`valsREG = ${pvs(valsREG)}`);
        console.log(`expREG = ${up(expREG)}`);
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
    contREG;
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
    (console.error(`2 Unknown contArray ${format(contArrayREG)}`), 'halt');
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
        const cont = contREG;
        either(valREG !== undefined ? valREG : makeOk(undefined),
               val => {
                   if (isTrueValue(val)) {
                       expREG = cont.exp.then;
                       envREG = cont.env;
                       contREG = cont.cont;
                       pcREG = "evalCont";
                   } else {
                       expREG = cont.exp.alt;
                       envREG = cont.env;
                       contREG = cont.cont;
                       pcREG = 'evalCont';
                   }
               },
               _ => {
                   contREG = cont.cont;
                   pcREG = "applyCont";
               });
    } else {
        console.error(`3 Unknown cont ${format(contREG)}`);
        pcREG = 'halt';
    }
}

// const applyLetCont = (cont: LetCont, vals: Array<Value | Error>): Value | Error =>
export const applyLetCont = (): void => {
    dumpREG();
    if (isLetCont(contArrayREG)) {
        const cont = contArrayREG;
        if (valsREG === undefined) {
            console.log(`Empty valsREG in applyLetCont`);
            return;
        }
        either(valsREG,
               vals => {
                   expsREG = cont.exp.body;
                   envREG = makeExtEnv(letVars(cont.exp), vals, cont.env);
                   contREG = cont.cont;
                   pcREG = "evalSequence";
               },
               message => {
                   valREG = makeFailure(message);
                   contREG = cont.cont;
                   pcREG = "applyCont";
               });
    } else {
        console.error(`Unknown cont ${format(contArrayREG)}`);
        pcREG = 'halt';
    }
}

// export const applyFirstCont = (cont: FirstCont, val: Value | Error): Value | Error =>
export const applyFirstCont = (): void => {
    dumpREG();
    if (isFirstCont(contREG)) {
        const cont = contREG;
        either(valREG !== undefined ? valREG : makeOk(undefined),
               () => {
                   expsREG = cont.exps;
                   envREG = cont.env;
                   contREG = cont.cont;
                   pcREG = "evalSequence";
               },
               _ => {
                   contREG = cont.cont;
                   pcREG = "applyCont";
               });
    } else {
        console.error(`Unknown cont ${format(contREG)}`);
        pcREG = 'halt';
    }
}

// export const applyLetrecCont = (cont: LetrecCont, vals: Array<Value | Error>): Value | Error => {
export const applyLetrecCont = (): void => {
    dumpREG();
    if (isLetrecCont(contArrayREG)) {
        const cont = contArrayREG;
        if (valsREG === undefined) {
            console.log(`Empty valsREG in applyLetrecCont`);
            return;
        }

        either(valsREG,
               vals => {
                   zipWith((bdg, cval) => setFBinding(bdg, cval), cont.env.frame.fbindings, vals);
                   expsREG = cont.exp.body;
                   envREG = cont.env;
                   contREG = cont.cont;
                   pcREG = "evalSequence";
               },
               message => {
                   valREG = makeFailure(message);
                   contREG = cont.cont;
                   pcREG = "applyCont";
               });
    } else {
        console.error(`Unknown cont ${format(contArrayREG)}`);
        pcREG = 'halt';
    }
}

// export const applySetCont = (cont: SetCont, val: Value | Error): Value | Error => {
export const applySetCont = (): void => {
    dumpREG();
    if (isSetCont(contREG)) {
        const cont = contREG;
        either(valREG !== undefined ? valREG : makeOk(undefined),
               val => {
                   const v = cont.exp.var.var;
                   const bdgResult = applyEnvBdg(cont.env, v);
                   either(bdgResult,
                          bdg => {
                              setFBinding(bdg, val);
                              valREG = undefined;
                              contREG = cont.cont;
                              pcREG = "applyCont";
                          },
                          _ => {
                              valREG = makeFailure(`var not found: ${format(v)}`);
                              contREG = cont.cont;
                              pcREG = "applyCont";
                          });
               },
               _ => {
                   contREG = cont.cont;
                   pcREG = "applyCont";
               });
    } else {
        console.error(`Unknown cont ${format(contREG)}`);
        pcREG = 'halt';
    }
}

// export const applyAppCont1 = (cont: AppCont1, val: Value | Error): Value | Error =>
export const applyAppCont1 = (): void => {
    dumpREG();
    if (isAppCont1(contREG)) {
        if (valREG === undefined) {
            console.error("valREG undefined in applyAppCont1");
            return;
        }
        expsREG = contREG.exp.rands;
        envREG = contREG.env;
        contArrayREG = makeAppCont2(valREG, contREG.env, contREG.cont)
        pcREG = 'evalExps';
    } else {
        console.error(`Unknown cont ${format(contREG)}`);
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
        console.error(`Unknown cont ${format(contArrayREG)}`);
        pcREG = 'halt';
    }
}

// export const applyExpsCont1 = (cont: ExpsCont1, val: Value | Error): Value | Error =>
export const applyExpsCont1 = (): void => {
    dumpREG();
    if (isExpsCont1(contREG)) {
        const cont = contREG;
        either(valREG !== undefined ? valREG : makeOk(undefined),
               val => {
                   expsREG = cont.exps;
                   envREG = cont.env;
                   contArrayREG = makeExpsCont2(val, cont.cont);
                   pcREG = "evalExps";
               },
               message => {
                   contArrayREG = cont.cont;
                   valsREG = makeFailure(message);
                   pcREG = "applyContArray";
               });
    } else {
        console.error(`Unknown cont ${format(contREG)}`);
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
        const cont = contArrayREG;
        either(valsREG,
               vals => {
                   valsREG = makeOk([cont.firstVal, ...vals]);
                   contArrayREG = cont.cont;
                   pcREG = "applyContArray";
               },
               message => {
                   valsREG = makeFailure(message);
                   contArrayREG = cont.cont;
                   pcREG = "applyContArray";
               });
    } else {
        console.error(`Unknown cont ${format(contArrayREG)}`);
        pcREG = 'halt';
    }
}

// export const applyDefCont = (cont: DefCont, val: Value | Error): Value | Error => {
export const applyDefCont = (): void => {
    dumpREG();
    if (isDefCont(contREG)) {
        const cont = contREG;
        either(valREG !== undefined ? valREG : makeOk(undefined),
               val => {
                   globalEnvAddBinding(cont.exp.var.var, val);
                   expsREG = cont.exps;
                   envREG = theGlobalEnv;
                   contREG = cont.cont;
                   pcREG = "evalSequence";
               },
               _ => {
                   contREG = cont.cont;
                   pcREG = "applyCont";
               });
    } else {
        console.error(`Unknown cont ${format(contREG)}`);
        pcREG = 'halt';
    }
}

// ========================================================
// Eval functions

// export const evalCont = (exp: CExp | Error, env: Env, cont: Cont): Value | Error =>
export const evalCont = (): void => {
    dumpREG();
    if (isNumExp(expREG)) {
        valREG = makeOk(expREG.val);
        pcREG = 'applyCont';
    } else if (isBoolExp(expREG)) {
        valREG = makeOk(expREG.val);
        pcREG = 'applyCont';
    } else if (isStrExp(expREG)) {
        valREG = makeOk(expREG.val);
        pcREG = 'applyCont';
    } else if (isPrimOp(expREG)) {
        valREG = makeOk(expREG);
        pcREG = 'applyCont';
    } else if (isVarRef(expREG)) {
        valREG = applyEnv(envREG, expREG.var);
        pcREG = 'applyCont';
    } else if (isLitExp(expREG)) {
        valREG = makeOk(expREG.val);
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
        valREG = makeFailure(`Bad L5 AST ${format(expREG)}`);
        pcREG = 'applyCont';
    }
}

export const isTrueValue = (x: Value): boolean =>
    ! (x === false);

// const evalIf = (exp: IfExp, env: Env, cont: Cont): Value | Error =>
export const evalIf = (): void => {
    dumpREG();
    if (isIfExp(expREG)) {
        contREG = makeIfCont(expREG, envREG, contREG);
        expREG = expREG.test;
        pcREG = 'evalCont';
    } else {
        valREG = makeFailure(`Bad expREG in evalIf ${format(expREG)}`);
        pcREG = 'halt';
    }
}

// const evalProc = (exp: ProcExp, env: Env, cont: Cont): Value | Error =>
export const evalProc = (): void => {
    dumpREG();
    if (isProcExp(expREG)) {
        valREG = makeOk(makeClosure(expREG.args, expREG.body, envREG));
        pcREG = 'applyCont';
    } else {
        valREG = makeFailure(`Bad expREG in evalProc ${format(expREG)}`);
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
        valREG = makeFailure(`Bad expREG in evalLet ${format(expREG)}`);
        pcREG = 'halt';
    }
}

// Evaluate an array of expressions in sequence - pass the result of the last element to cont
// @Pre: exps is not empty
// export const evalSequence = (exps: Exp[], env: Env, cont: Cont): Value | Error =>
export const evalSequence = (): void => {
    dumpREG();
    if (isEmpty(expsREG)) {
        valREG = makeFailure("Empty Sequence");
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
        valREG = makeFailure(`Bad expREG in evalLetrec ${format(expREG)}`);
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
        valREG = makeFailure(`Bad expREG in evalSet ${format(expREG)}`);
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
        valREG = makeFailure(`Bad expREG in evalApp ${format(expREG)}`);
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

    either(valsREG,
           vals => {
               if (valREG === undefined) {
                   valREG = makeFailure(`"undefined" procedure in applyProcedure`);
                   pcREG = "applyCont";
                   return;
               }
               either(valREG,
                      val => {
                          if (isPrimOp(val)) {
                              valREG = applyPrimitive(val, vals);
                              pcREG = "applyCont";
                          } else if (isClosure(val)) {
                              pcREG = "applyClosure";
                          } else {
                              valREG = makeFailure(`Bad procedure: ${format(val)}`);
                              pcREG = "applyCont";
                          }
                      },
                      _ => {
                          pcREG = "applyCont";
                      });
           },
           _ => {
               valREG = makeFailure(`Bad argument: ${format(valsREG)}`);
               pcREG = "applyCont";
           });
}

// export const applyClosure = (proc: Closure, args: Value[], cont: Cont): Value | Error => {
// Parameters in proc = valREG, args = valsREG
export const applyClosure = (): void => {
    dumpREG();
    if (valREG) {
        either(valREG, 
               value => {
                   if (isClosure(value)) {
                       const vars = map((v: VarDecl) => v.var, value.params);
                       expsREG = value.body;
                       if (valsREG === undefined) {
                           console.error("Empty valsREG in applyClosure");
                           return;
                       }
                       either(valsREG,
                              vals => {
                                  envREG = makeExtEnv(vars, vals, value.env);
                                  pcREG = "evalSequence";
                              },
                              message => {
                                  valREG = makeFailure(message);
                                  pcREG = "applyCont";
                              });
                   }
               },
               _ => {
                   valREG = makeFailure(`Bad expREG in evalApp ${format(expREG)}`);
                   pcREG = "halt";
               });
    } else {
        valREG = makeFailure(`valREG is undefined`)
        pcREG = "halt";
    }
}

// Evaluate an array of expressions - pass the result as an array to the continuation
// export const evalExps = (exps: Exp[], env: Env, cont: ContArray): Value | Error =>
export const evalExps = (): void => {
    dumpREG();
    if (isEmpty(expsREG)) {
        valsREG = makeOk([]);
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
        valsREG = bind(unparse(expREG), e => makeFailure(`Unexpected define: ${format(e)}`));
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
        valREG = makeFailure(`Bad expREG in evalDefine ${format(expREG)}`);
        pcREG = 'halt';
    }
}

// Evaluate a program
// Main program - EVALUATION LOOP of the Virtual Machine
export const evalProgram = (program: Program): Result<Value> => {
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
    return valREG ? valREG : makeOk(undefined);
}

export const evalParse = (s: string): Result<Value> =>
    bind(bind(p(s), parseL5Exp), (exp: Exp) => evalProgram(makeProgram([exp])));
