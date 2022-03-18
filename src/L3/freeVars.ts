import { elem, exists, map, reduce, union } from "fp-ts/ReadonlyArray";
import { fromEquals, Eq } from "fp-ts/Eq";
import { pipe } from "fp-ts/function";
import * as S from "fp-ts/string";
import { VarRef, Exp, Program } from "./L3-ast";
import { isAppExp, isAtomicExp, isBoolExp, isDefineExp, isIfExp, isLetExp, isLitExp, isNumExp,
         isPrimOp, isProcExp, isProgram, isStrExp, isVarRef } from './L3-ast';

const varRefEq: Eq<VarRef> = fromEquals((x, y) => x.var === y.var);

const zero: number = 0
const varRefUnion = union(varRefEq);

// TODO: No error handling
export const height = (exp: Program | Exp): number =>
    isAtomicExp(exp) ? 1 :
    isLitExp(exp) ? 1 :
    isDefineExp(exp) ? 1 + height(exp.val) :
    isIfExp(exp) ? 1 + Math.max(height(exp.test), height(exp.then), height(exp.alt)) :
    isProcExp(exp) ? 1 + pipe(exp.body, map(height), reduce(zero, Math.max)) :
    isLetExp(exp) ? 1 + Math.max(
        pipe(exp.bindings, map(b => height(b.val)), reduce(zero, Math.max)),
        pipe(exp.body, map(height), reduce(zero, Math.max))) :
    isAppExp(exp) ? Math.max(height(exp.rator), pipe(exp.rands, map(height), reduce(zero, Math.max))) :
    isProgram(exp) ? 1 + pipe(exp.exps, map(height), reduce(zero, Math.max)) :
    exp;

const includes = elem(S.Eq);

export const occursFree = (v: string, e: Program | Exp): boolean =>
    isBoolExp(e) ? false :
    isNumExp(e) ? false :
    isStrExp(e) ? false :
    isLitExp(e) ? false :
    isVarRef(e) ? (v === e.var) :
    isIfExp(e) ? occursFree(v, e.test) || occursFree(v, e.then) || occursFree(v, e.alt) :
    isProcExp(e) ? !pipe(e.args, map(p => p.var), includes(v)) &&
                   pipe(e.body, exists(b => occursFree(v, b))) :
    isPrimOp(e) ? false :
    isAppExp(e) ? occursFree(v, e.rator) ||
                  pipe(e.rands, exists(rand => occursFree(v, rand))) :
    isDefineExp(e) ? (v !== e.var.var) && occursFree(v, e.val) :
    isLetExp(e) ? false : // TODO
    isProgram(e) ? false : // TODO
    e;

export const referencedVars = (e: Program | Exp): readonly VarRef[] =>
    isBoolExp(e) ? [] :
    isNumExp(e) ? [] :
    isStrExp(e) ? [] :
    isLitExp(e) ? [] :
    isPrimOp(e) ? [] :
    isVarRef(e) ? [e] :
    isIfExp(e) ? pipe([e.test, e.then, e.alt], map(referencedVars), reduce([], varRefUnion)) :
    isAppExp(e) ? pipe(e.rands, map(referencedVars), reduce(referencedVars(e.rator), varRefUnion)) :
    isProcExp(e) ? pipe(e.body, map(referencedVars), reduce([], varRefUnion)) :
    isDefineExp(e) ? referencedVars(e.val) :
    isProgram(e) ? pipe(e.exps, map(referencedVars), reduce([], varRefUnion)) :
    isLetExp(e) ? [] : // TODO
    e;
