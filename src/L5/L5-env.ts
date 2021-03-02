// Environment for L5 with mutation
// ================================
// An environment represents a partial function from symbols (variable names) to values.
// It supports the operation: applyEnv(env,var)
// which either returns the value of var in the environment, or else an error.
//
// A box-env represents an environment as a mapping from var to boxes containing values.
// The global environment is the root of all extended environment.
// It contains a frame that is initialized with primitive bindings
// and can be extended with the define operator.
//
// Box-Env is defined inductively by the following cases:
// * <box-env> ::= <global-env> | <extended-box-env>
// * <global-env> ::= (global-env frame) // global-env(frame:Box(Frame))
// * <extended-box-env> ::= (extended-box-env frame enclosing-env)
//      // extended-box-env(frame: Frame, enclosing-env: Box-env)
//
// Frame:
// * <fbinding> ::= (var val) // binding(var:string, val:Box(Value))
// * <frame> ::= (frame (var val)*) // frame(bindings:List(fbinding))
// applyFrame(frame, var) => val
// applyFrameBdg(frame, var) => Box(val)
//
// The key operation on env is applyEnv(env, var) which returns the value associated to var in env
// or returns an error if var is not defined in env.
// To support mutation - we also add applyEnvBdg(env, var) -> Box(val)

import { map, zipWith } from "ramda";
import { Value } from './L5-value';
import { makeBox, setBox, unbox, Box} from '../shared/box';
import { Result, makeFailure, makeOk, bind, either } from "../shared/result";
import { cons } from "../shared/list";

// ========================================================
// Frame binding
export interface FBinding {
    tag: "FBinding";
    var: string;
    val: Box<Value>;
}

const isFBinding = (x: any): x is FBinding => x.tag === "FBinding";
const makeFBinding = (v: string, val: Value): FBinding =>
    ({tag: "FBinding", var: v, val: makeBox(val)});
const getFBindingVar = (f: FBinding): string => f.var;
export const getFBindingVal = (f: FBinding): Value => unbox(f.val);
export const setFBinding = (f: FBinding, val: Value): void => { setBox(f.val, val); return; };

// ========================================================
// Frame
interface Frame {
    tag: "Frame";
    fbindings: FBinding[];
}

const makeFrame = (vars: string[], vals: Value[]): Frame =>
    ({tag: "Frame", fbindings: zipWith(makeFBinding, vars, vals)});
const extendFrame = (frame: Frame, v: string, val: Value): Frame =>
    ({tag: "Frame", fbindings: cons(makeFBinding(v, val), frame.fbindings)});
const isFrame = (x: any): x is Frame => x.tag === "Frame";
const frameVars = (frame: Frame): string[] => map(getFBindingVar, frame.fbindings);
const frameVals = (frame: Frame): Value[] => map(getFBindingVal, frame.fbindings);

const applyFrame = (frame: Frame, v: string): Result<FBinding> => {
    const pos = frameVars(frame).indexOf(v);
    return (pos > -1) ? makeOk(frame.fbindings[pos]) : makeFailure(`Var not found: ${v}`);
};
const setVarFrame = (frame: Frame, v: string, val: Value): Result<void> =>
    bind(applyFrame(frame, v), (bdg: FBinding) => makeOk(setFBinding(bdg, val)));

// ========================================================
// Environment data type
export type Env = GlobalEnv | ExtEnv;
export const isEnv = (x: any): x is Env => isExtEnv(x) || isGlobalEnv(x);

/*
Purpose: lookup the value of var in env and return a mutable binding
Signature: applyEnvBdg(env, var)
Type: [Env * string -> Result<FBinding>]
*/
export const applyEnvBdg = (env: Env, v: string): Result<FBinding> =>
    isGlobalEnv(env) ? applyGlobalEnvBdg(env, v) :
    isExtEnv(env) ? applyExtEnvBdg(env, v) :
    env;

/*
Purpose: lookup the value of var in env.
Signature: applyEnv(env, var)
Type: [Env * string -> Result<Value>]
*/
export const applyEnv = (env: Env, v: string): Result<Value> =>
    bind(applyEnvBdg(env, v), (bdg: FBinding) => makeOk(getFBindingVal(bdg)));

// ========================================================
// ExtEnv
export interface ExtEnv {
    tag: "ExtEnv";
    frame: Frame;
    env: Env;
}
export const isExtEnv = (x: any): x is ExtEnv => x.tag === "ExtEnv";
export const makeExtEnv = (vs: string[], vals: Value[], env: Env): ExtEnv =>
    ({tag: "ExtEnv", frame: makeFrame(vs, vals), env: env});
export const ExtEnvVars = (env: ExtEnv): string[] =>
    map(getFBindingVar, env.frame.fbindings);
export const ExtEnvVals = (env: ExtEnv): Value[] =>
    map(getFBindingVal, env.frame.fbindings);

const applyExtEnvBdg = (env: ExtEnv, v: string): Result<FBinding> =>
    either(applyFrame(env.frame, v), makeOk, _ => applyEnvBdg(env.env, v));

// ========================================================
// GlobalEnv
// global-env - has a mutable frame - so that we can add bindings at any time.
interface GlobalEnv {
    tag: "GlobalEnv";
    frame: Box<Frame>;
}
export const isGlobalEnv = (x: any): x is GlobalEnv => x.tag === "GlobalEnv";
const makeGlobalEnv = (): GlobalEnv => ({tag: "GlobalEnv", frame: makeBox(makeFrame([], []))});
// There is a single mutable value in the type Global-env
export const theGlobalEnv = makeGlobalEnv();

const globalEnvSetFrame = (ge: GlobalEnv, f: Frame): void => setBox(ge.frame, f);

export const globalEnvAddBinding = (v: string, val: Value): void =>
    globalEnvSetFrame(theGlobalEnv,
                      extendFrame(unbox(theGlobalEnv.frame), v, val));

const applyGlobalEnvBdg = (ge: GlobalEnv, v: string): Result<FBinding> =>
    applyFrame(unbox(ge.frame), v);
