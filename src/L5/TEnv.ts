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
import * as E from "fp-ts/Either";

import { TExp } from './TExp';

export type TEnv = EmptyTEnv | ExtendTEnv;

export interface EmptyTEnv { tag: "EmptyTEnv" }
export const makeEmptyTEnv = (): EmptyTEnv => ({tag: "EmptyTEnv"});
export const isEmptyTEnv = (x: any): x is EmptyTEnv => x.tag === "EmptyTEnv";

export interface ExtendTEnv { tag: "ExtendTEnv"; vars: readonly string[]; texps: readonly TExp[]; tenv: TEnv; }
export const makeExtendTEnv = (vars: readonly string[], texps: readonly TExp[], tenv: TEnv): ExtendTEnv =>
    ({tag: "ExtendTEnv", vars: vars, texps: texps, tenv: tenv});
export const isExtendTEnv = (x: any): x is ExtendTEnv => x.tag === "ExtendTEnv";

export const applyTEnv = (tenv: TEnv, v: string): E.Either<string, TExp> =>
    isEmptyTEnv(tenv) ? E.left(`Type Variable not found ${v}`) :
    applyExtendTEnv(tenv.texps, tenv.tenv, v, tenv.vars.indexOf(v));

export const applyExtendTEnv = (texps: readonly TExp[], tenv: TEnv, v: string, pos: number): E.Either<string, TExp> =>
    (pos === -1) ? applyTEnv(tenv, v) :
    E.of(texps[pos]);
