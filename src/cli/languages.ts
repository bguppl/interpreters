// languages.ts
// Uniform language registry used by the interactive REPL.
// Each entry provides parse / eval / display for one interpreter level.

import { Sexp } from 's-expression';
import { Result } from '../shared/result';

// ---- L1 (env types live in L1-eval, not a separate env file) ----
import { parseL1Exp, Exp as L1Exp, CExp as L1CExp } from '../L1/L1-ast';
import { evalL1Exp, makeEmptyEnv as makeL1EmptyEnv, makeEnv as makeL1Env,
         Value as L1Value, Env as L1Env } from '../L1/L1-eval';

// ---- L3 ----
import { parseL3Exp, Exp as L3Exp, CExp as L3CExp } from '../L3/L3-ast';
import { evalL3Exp } from '../L3/L3-eval';
import { makeEmptyEnv as makeL3EmptyEnv, makeEnv as makeL3Env,
         Env as L3Env } from '../L3/L3-env';
import { valueToString as L3valueToString, isClosure as isL3Closure,
         Value as L3Value } from '../L3/L3-value';

// ---- L4 ----
import { parseL4Exp, Exp as L4Exp, CExp as L4CExp } from '../L4/L4-ast';
import { evalL4Exp } from '../L4/L4-eval';
import { makeEmptyEnv as makeL4EmptyEnv, makeExtEnv as makeL4ExtEnv,
         Env as L4Env } from '../L4/L4-env';
import { valueToString as L4valueToString, isClosure as isL4Closure,
         Value as L4Value } from '../L4/L4-value';

// ---- L5 ----
import { parseL5Exp, makeProgram, Exp as L5Exp, CExp as L5CExp } from '../L5/L5-ast';
import { applicativeEval as L5applicativeEval } from '../L5/L5-eval';
import { globalEnvAddBinding, theGlobalEnv } from '../L5/L5-env';
import { valueToString as L5valueToString, isClosure as isL5Closure,
         Value as L5Value } from '../L5/L5-value';

// ---- L7 (CPS) — reuses L5 parser and global env ----
import { evalProgram as L7evalProgram } from '../cps/L7c-eval';

// ============================================================
// Abstract union types spanning all interpreter levels
// ============================================================

/** Any expression type from any supported language (includes DefineExp). */
export type AllExp   = L1Exp | L3Exp | L4Exp | L5Exp;

/** Any non-define (compound/atomic) expression type from any supported language. */
export type AllCExp  = L1CExp | L3CExp | L4CExp | L5CExp;

/** Any value type from any supported language. */
export type AllValue = L1Value | L3Value | L4Value | L5Value;

/**
 * Any environment type used in the REPL.
 * L5 and L7 bypass the env parameter and use theGlobalEnv directly,
 * so their REPL env slot is null.
 */
export type AllEnv   = L1Env | L3Env | L4Env | null;

// ============================================================
// Structural helper types
// ============================================================

/**
 * Structural shape shared by all language DefineExp variants.
 * val is typed as AllCExp: every language's DefineExp.val is a CExp.
 */
export type DefineExpLike = {
    tag: 'DefineExp';
    var: { var: string };
    val: AllCExp;
};

/** Structural shape shared by all language Closure variants. */
type ClosureLike = {
    params: ReadonlyArray<{ var: string }>;
    body: unknown[];
};

// ============================================================
// LangConfig type
//
// Two expression type parameters:
//   E  — the full Exp type (DefineExp | CExp), returned by parseExpr
//   CE — the CExp type (no DefineExp), accepted by evalExpr / evalDefineRHS
//
// Method syntax (rather than arrow-function properties) is used intentionally:
// TypeScript checks method declarations bivariantly, which lets a precisely-typed
// LangConfig<L1Exp, L1CExp, L1Value, L1Env> be safely stored in the registry
// typed as LangConfig<AllExp, AllCExp, AllValue, AllEnv>.
// ============================================================

export type LangConfig<
    E  extends AllExp,
    CE extends AllCExp,
    V  extends AllValue,
    Env extends AllEnv
> = {
    name: string;
    /** Primitive operation names available in this language (from applyPrimitive). */
    readonly primitives: readonly string[];
    /** Parse one Sexp token/list into a language expression (Exp). */
    parseExpr(sexp: Sexp): Result<E>;
    /**
     * Evaluate a non-define expression with the current REPL environment.
     * Callers guarantee exp is a CExp (not a DefineExp).
     */
    evalExpr(exp: CE, env: Env): Result<V>;
    /** Evaluate only the RHS of a (define var rhs) expression. */
    evalDefineRHS(rhsExp: CE, env: Env): Result<V>;
    /** Create the initial empty environment for a fresh REPL session. */
    makeInitialEnv(): Env;
    /**
     * Extend the env with a new binding.
     * L1/L3/L4: returns a new immutable env.
     * L5/L7: mutates theGlobalEnv as a side-effect and returns null.
     */
    extendEnv(env: Env, varName: string, val: V): Env;
    /** Convert an evaluated value to a display string. */
    display(val: V): string;
};

/** Concrete registry / REPL type: union-typed parameters, no `any`. */
export type AnyLangConfig = LangConfig<AllExp, AllCExp, AllValue, AllEnv>;

// ============================================================
// Generic helpers — all languages share the same DefineExp shape
// ============================================================

export const isDefine = (exp: unknown): exp is DefineExpLike =>
    typeof exp === 'object' &&
    exp !== null &&
    (exp as { tag?: unknown }).tag === 'DefineExp';

export const defineVarName = (exp: DefineExpLike): string  => exp.var.var;
/** Returns the RHS of a define expression, already typed as AllCExp. */
export const defineRHS     = (exp: DefineExpLike): AllCExp => exp.val;

// ============================================================
// Display helpers — mirror the displayClosure logic in each L*.ts CLI
// ============================================================

const closureDisplay = (c: ClosureLike): string => {
    const params = c.params.map(p => p.var).join(' ');
    return `<Closure (${params}) ${c.body}>`;
};

const displayL3 = (val: L3Value): string =>
    isL3Closure(val) ? closureDisplay(val) : L3valueToString(val);

const displayL4 = (val: L4Value): string =>
    isL4Closure(val) ? closureDisplay(val) : L4valueToString(val);

const displayL5 = (val: L5Value): string =>
    isL5Closure(val) ? closureDisplay(val) : L5valueToString(val);

// ============================================================
// Language configurations
// ============================================================
//
// Each config uses its precise (E, CE, V, Env) types.
// evalExpr and evalDefineRHS accept CE (the CExp type) directly,
// so no casts are needed — the underlying evaluators accept exactly that type.

export const L1Config: LangConfig<L1Exp, L1CExp, L1Value, L1Env> = {
    name: 'L1',
    primitives: ['+', '-', '*', '/', '>', '<', '=', 'not'],
    parseExpr(sexp)             { return parseL1Exp(sexp); },
    evalExpr(exp, env)          { return evalL1Exp(exp, env); },
    evalDefineRHS(rhs, env)     { return evalL1Exp(rhs, env); },
    makeInitialEnv()            { return makeL1EmptyEnv(); },
    extendEnv(env, varName, val){ return makeL1Env(varName, val, env); },
    display(val)                { return JSON.stringify(val); },
};

export const L3Config: LangConfig<L3Exp, L3CExp, L3Value, L3Env> = {
    name: 'L3',
    primitives: ['+', '-', '*', '/', '>', '<', '=', 'not', 
        'and', 'or', 'eq?', 'string=?', 'cons', 'car', 'cdr', 'list', 
        'pair?', 'number?', 'boolean?', 'symbol?', 'string?'],
    parseExpr(sexp)             { return parseL3Exp(sexp); },
    evalExpr(exp, env)          { return evalL3Exp(exp, env); },
    evalDefineRHS(rhs, env)     { return evalL3Exp(rhs, env); },
    makeInitialEnv()            { return makeL3EmptyEnv(); },
    extendEnv(env, varName, val){ return makeL3Env(varName, val, env); },
    display(val)                { return displayL3(val); },
};

export const L4Config: LangConfig<L4Exp, L4CExp, L4Value, L4Env> = {
    name: 'L4',
    primitives: ['+', '-', '*', '/', '>', '<', '=', 'not', 
        'and', 'or', 'eq?', 'string=?', 'cons', 'car', 'cdr', 'list', 
        'list?', 'pair?', 'number?', 'boolean?', 'symbol?', 'string?'],
    parseExpr(sexp)             { return parseL4Exp(sexp); },
    evalExpr(exp, env)          { return evalL4Exp(exp, env); },
    evalDefineRHS(rhs, env)     { return evalL4Exp(rhs, env); },
    makeInitialEnv()            { return makeL4EmptyEnv(); },
    extendEnv(env, varName, val){ return makeL4ExtEnv([varName], [val], env); },
    display(val)                { return displayL4(val); },
};

export const L5Config: LangConfig<L5Exp, L5CExp, L5Value, null> = {
    name: 'L5',
    primitives: ['+', '-', '*', '/', '>', '<', '=', 'not', 
        'and', 'or', 'eq?', 'string=?', 'cons', 'car', 'cdr', 'list', 
        'list?', 'pair?', 'number?', 'boolean?', 'symbol?', 'string?'],
    parseExpr(sexp)             { return parseL5Exp(sexp); },
    evalExpr(exp, _env)         { return L5applicativeEval(exp, theGlobalEnv); },
    evalDefineRHS(rhs, _env)    { return L5applicativeEval(rhs, theGlobalEnv); },
    makeInitialEnv()            { return null; },
    extendEnv(_env, varName, val) {
        globalEnvAddBinding(varName, val);
        return null;
    },
    display(val)                { return displayL5(val); },
};

export const L7Config: LangConfig<L5Exp, L5CExp, L5Value, null> = {
    name: 'L7',
    primitives: ['+', '-', '*', '/', '>', '<', '=', 'not', 
        'and', 'or', 'eq?', 'string=?', 'cons', 'car', 'cdr', 'list', 
        'list?', 'pair?', 'number?', 'boolean?', 'symbol?', 'string?'],
    parseExpr(sexp)             { return parseL5Exp(sexp); },
    // Non-define: run through the full CPS evaluator (makeProgram accepts Exp[], L5CExp ⊆ L5Exp)
    evalExpr(exp, _env)         { return L7evalProgram(makeProgram([exp])); },
    // Define RHS: use L5's direct evaluator (shared global env, no CPS needed for meta-ops)
    evalDefineRHS(rhs, _env)    { return L5applicativeEval(rhs, theGlobalEnv); },
    makeInitialEnv()            { return null; },
    extendEnv(_env, varName, val) {
        globalEnvAddBinding(varName, val);
        return null;
    },
    display(val)                { return displayL5(val); },
};

export const LANGUAGES: Record<string, AnyLangConfig> = {
    L1: L1Config,
    L3: L3Config,
    L4: L4Config,
    L5: L5Config,
    L7: L7Config,
};

export const DEFAULT_LANGUAGE = 'L3';
