// ========================================================
// Environment data type for L3
import * as E from "fp-ts/Either";
import { Value } from './L3-value';

export type Env = EmptyEnv | NonEmptyEnv;
export interface EmptyEnv {tag: "EmptyEnv" }
export interface NonEmptyEnv {
    tag: "Env";
    var: string;
    val: Value;
    nextEnv: Env;
}
export const makeEmptyEnv = (): EmptyEnv => ({tag: "EmptyEnv"});
export const makeEnv = (v: string, val: Value, env: Env): NonEmptyEnv =>
    ({tag: "Env", var: v, val: val, nextEnv: env});
export const isEmptyEnv = (x: any): x is EmptyEnv => x.tag === "EmptyEnv";
export const isNonEmptyEnv = (x: any): x is NonEmptyEnv => x.tag === "Env";
export const isEnv = (x: any): x is Env => isEmptyEnv(x) || isNonEmptyEnv(x);

export const applyEnv = (env: Env, v: string): E.Either<string, Value> =>
    isEmptyEnv(env) ? E.left("var not found " + v) :
    env.var === v ? E.of(env.val) :
    applyEnv(env.nextEnv, v);

