/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Environment
;; ================
;; An environment represents a partial function from symbols (variable names) to type expressions.
;; It supports the operation: applyTenv(tenv,var)
;; which either returns the type of var in the type-environment, or else an error.
;;
;; TEnv is defined exactly as Env - except that we map vars to type-expressions (TExp) instead of values.
;; * <tenv> ::= <empty-tenv> | <extended-tenv>
;; * <empty-tenv> ::= empty-tenv()
;; * <extended-tenv> ::= (tenv (symbol+) (type-exp+) enclosing-tenv) // env(vars:List(Symbol), tes:List(Type-exp), enclosing-tenv: TEnv)
*/

import { TExp } from './TExp';
import { Result, makeOk, makeFailure } from '../shared/result';

export type TEnv = EmptyTEnv | ExtendTEnv;

export type EmptyTEnv = { tag: "EmptyTEnv" }
export const makeEmptyTEnv = (): EmptyTEnv => ({tag: "EmptyTEnv"});
export const isEmptyTEnv = (x: any): x is EmptyTEnv => x.tag === "EmptyTEnv";

export type ExtendTEnv = { tag: "ExtendTEnv"; vars: string[]; texps: TExp[]; tenv: TEnv; }
export const makeExtendTEnv = (vars: string[], texps: TExp[], tenv: TEnv): ExtendTEnv =>
    ({tag: "ExtendTEnv", vars: vars, texps: texps, tenv: tenv});
export const isExtendTEnv = (x: any): x is ExtendTEnv => x.tag === "ExtendTEnv";

export const applyTEnv = (tenv: TEnv, v: string): Result<TExp> =>
    isEmptyTEnv(tenv) ? makeFailure(`Type Variable not found ${v}`) :
    applyExtendTEnv(tenv.texps, tenv.tenv, v, tenv.vars.indexOf(v));

export const applyExtendTEnv = (texps: TExp[], tenv: TEnv, v: string, pos: number): Result<TExp> =>
    (pos === -1) ? applyTEnv(tenv, v) :
    makeOk(texps[pos]);
