import { any as some } from "ramda";
import { map, reduce, union, includes } from "ramda";
import { VarRef, Exp, Program } from "./L3-ast";
import { isAppExp, isAtomicExp, isBoolExp, isDefineExp, isIfExp, isLetExp, isLitExp, isNumExp,
         isPrimOp, isProcExp, isProgram, isStrExp, isVarRef } from './L3-ast';

const zero: number = 0
const varRefUnion = (x: VarRef[], y: VarRef[]) => union(x, y);

// TODO: No error handling
export const height = (exp: Program | Exp): number =>
    isAtomicExp(exp) ? 1 :
    isLitExp(exp) ? 1 :
    isDefineExp(exp) ? 1 + height(exp.val) :
    isIfExp(exp) ? 1 + Math.max(height(exp.test), height(exp.then), height(exp.alt)) :
    isProcExp(exp) ? 1 + reduce(Math.max, zero,
                                map((bodyExp) => height(bodyExp), exp.body)) :
    isLetExp(exp) ? 1 + Math.max(
                            reduce(Math.max, zero,
                                   map((binding) => height(binding.val), exp.bindings)),
                            reduce(Math.max, zero,
                                   map((bodyExp) => height(bodyExp), exp.body))) :
    isAppExp(exp) ? Math.max(height(exp.rator),
                             reduce(Math.max, zero,
                                    map((rand) => height(rand), exp.rands))) :
    isProgram(exp) ? 1 + reduce(Math.max, zero,
                                map((e) => height(e), exp.exps)) :
    -1;

export const occursFree = (v: string, e: Program | Exp): boolean =>
    isBoolExp(e) ? false :
    isNumExp(e) ? false :
    isStrExp(e) ? false :
    isLitExp(e) ? false :
    isVarRef(e) ? (v === e.var) :
    isIfExp(e) ? occursFree(v, e.test) || occursFree(v, e.then) || occursFree(v, e.alt) :
    isProcExp(e) ? ! includes(v, map((p) => p.var, e.args)) &&
                   some((b) => occursFree(v, b), e.body) :
    isPrimOp(e) ? false :
    isAppExp(e) ? occursFree(v, e.rator) ||
                  some((rand) => occursFree(v, rand), e.rands) :
    isDefineExp(e) ? (v !== e.var.var) && occursFree(v, e.val) :
    isLetExp(e) ? false : // TODO
    isProgram(e) ? false : // TODO
    false;

export const referencedVars = (e: Program | Exp): VarRef[] =>
    isBoolExp(e) ? Array<VarRef>() :
    isNumExp(e) ? Array<VarRef>() :
    isStrExp(e) ? Array<VarRef>() :
    isLitExp(e) ? Array<VarRef>() :
    isPrimOp(e) ? Array<VarRef>() :
    isVarRef(e) ? [e] :
    isIfExp(e) ? reduce(varRefUnion, Array<VarRef>(),
                        map(referencedVars, [e.test, e.then, e.alt])) :
    isAppExp(e) ? union(referencedVars(e.rator),
                        reduce(varRefUnion, Array<VarRef>(), map(referencedVars, e.rands))) :
    isProcExp(e) ? reduce(varRefUnion, Array<VarRef>(), map(referencedVars, e.body)) :
    isDefineExp(e) ? referencedVars(e.val) :
    isProgram(e) ? reduce(varRefUnion, Array<VarRef>(), map(referencedVars, e.exps)) :
    isLetExp(e) ? Array<VarRef>() : // TODO
    Array<VarRef>();
