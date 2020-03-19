/// <reference path="../shared/s-expression.d.ts" />
// ===========================================================
// AST type models
import { map, zipWith } from "ramda";
import p, { StringTree } from "s-expression";
import { allT, first, second, rest, isArray, isEmpty, isSexpString, isString, isNumericString } from "../shared/list";
import {isError, hasNoError, safeF, safeFL, safeF2, getErrorMessages} from '../shared/error'
import { isSymbolSExp, isEmptySExp, isCompoundSExp } from './L4-value';
import { makeEmptySExp, makeSymbolSExp, SExp, makeCompoundSExp, valueToString } from './L4-value'

/*
;; =============================================================================
;; Scheme Parser
;;
;; L2 extends L1 with support for IfExp and ProcExp
;; L3 extends L2 with support for:
;; - Pair and List datatypes
;; - Compound literal expressions denoted with quote
;; - Primitives: cons, car, cdr, list?
;; - The empty-list literal expression
;; - The Let abbreviation is also supported.
;; L4 extends L3 with:
;; - letrec
;; - set!

;; <program> ::= (L4 <exp>+) // Program(exps:List(exp))
;; <exp4> ::= <define> | <cexp>               / DefExp | CExp
;; <define> ::= ( define <var> <cexp> )       / DefExp(var:VarDecl, val:CExp)
;; <var> ::= <identifier>                     / VarRef(var:string)
;; <cexp> ::= <number>                        / NumExp(val:number)
;;         |  <boolean>                       / BoolExp(val:boolean)
;;         |  <string>                        / StrExp(val:string)
;;         |  ( lambda ( <var>* ) <cexp>+ )   / ProcExp(params:VarDecl[], body:CExp[]))
;;         |  ( if <cexp> <cexp> <cexp> )     / IfExp(test: CExp, then: CExp, alt: CExp)
;;         |  ( let ( <binding>* ) <cexp>+ )  / LetExp(bindings:Binding[], body:CExp[]))
;;         |  ( quote <sexp> )                / LitExp(val:SExp)
;;         |  ( <cexp> <cexp>* )              / AppExp(operator:CExp, operands:CExp[]))
;;         |  ( letrec ( binding*) <cexp>+ )  / LetrecExp(bindings:Bindings[], body: CExp) #### L4
;;         |  ( set! <var> <cexp>)            / SetExp(var: varRef, val: CExp)             #### L4
;; <binding>  ::= ( <var> <cexp> )            / Binding(var:VarDecl, val:Cexp)
;; <prim-op>  ::= + | - | * | / | < | > | = | not |  eq? | string=?
;;                  | cons | car | cdr | list | pair? | list? | number?
;;                  | boolean? | symbol? | string?      ##### L3
;; <num-exp>  ::= a number token
;; <bool-exp> ::= #t | #f
;; <str-exp>  ::= "tokens*"
;; <var-ref>  ::= an identifier token
;; <var-decl> ::= an identifier token
;; <sexp>     ::= symbol | number | bool | string | ( <sexp>* )              ##### L3
*/

// A value returned by parse
export type Parsed = Exp | Program;

export type Exp = DefineExp | CExp;
export type AtomicExp = NumExp | BoolExp | StrExp | PrimOp | VarRef;
export type CompoundExp = AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp;
export type CExp =  AtomicExp | CompoundExp;

export interface Program {tag: "Program"; exps: Exp[]; };
export interface DefineExp {tag: "DefineExp"; var: VarDecl; val: CExp; };
export interface NumExp {tag: "NumExp"; val: number; };
export interface BoolExp {tag: "BoolExp"; val: boolean; };
export interface StrExp {tag: "StrExp"; val: string; };
export interface PrimOp {tag: "PrimOp"; op: string; };
export interface VarRef {tag: "VarRef"; var: string; };
export interface VarDecl {tag: "VarDecl"; var: string; };
export interface AppExp {tag: "AppExp"; rator: CExp; rands: CExp[]; };
// L2
export interface IfExp {tag: "IfExp"; test: CExp; then: CExp; alt: CExp; };
export interface ProcExp {tag: "ProcExp"; args: VarDecl[], body: CExp[]; };
export interface Binding {tag: "Binding"; var: VarDecl; val: CExp; };
export interface LetExp {tag: "LetExp"; bindings: Binding[]; body: CExp[]; };
// L3
export interface LitExp {tag: "LitExp"; val: SExp; };
// L4
export interface LetrecExp {tag: "LetrecExp"; bindings: Binding[]; body: CExp[]; };
export interface SetExp {tag: "SetExp", var: VarRef; val: CExp; };

// Type value constructors for disjoint types
export const makeProgram = (exps: Exp[]): Program => ({tag: "Program", exps: exps});
export const makeDefineExp = (v: VarDecl, val: CExp): DefineExp =>
    ({tag: "DefineExp", var: v, val: val});
export const makeNumExp = (n: number): NumExp => ({tag: "NumExp", val: n});
export const makeBoolExp = (b: boolean): BoolExp => ({tag: "BoolExp", val: b});
export const makeStrExp = (s: string): StrExp => ({tag: "StrExp", val: s});
export const makePrimOp = (op: string): PrimOp => ({tag: "PrimOp", op: op});
export const makeVarRef = (v: string): VarRef => ({tag: "VarRef", var: v});
export const makeVarDecl = (v: string): VarDecl => ({tag: "VarDecl", var: v});
export const makeAppExp = (rator: CExp, rands: CExp[]): AppExp =>
    ({tag: "AppExp", rator: rator, rands: rands});
// L2
export const makeIfExp = (test: CExp, then: CExp, alt: CExp): IfExp =>
    ({tag: "IfExp", test: test, then: then, alt: alt});
export const makeProcExp = (args: VarDecl[], body: CExp[]): ProcExp =>
    ({tag: "ProcExp", args: args, body: body});
export const makeBinding = (v: string, val: CExp): Binding =>
    ({tag: "Binding", var: makeVarDecl(v), val: val});
export const makeLetExp = (bindings: Binding[], body: CExp[]): LetExp =>
    ({tag: "LetExp", bindings: bindings, body: body});
// L3
export const makeLitExp = (val: SExp): LitExp =>
    ({tag: "LitExp", val: val});
// L4
export const makeLetrecExp = (bindings: Binding[], body: CExp[]): LetrecExp =>
    ({tag: "LetrecExp", bindings: bindings, body: body});
export const makeSetExp = (v: VarRef, val: CExp): SetExp =>
    ({tag: "SetExp", var: v, val: val});

// Type predicates for disjoint types
export const isProgram = (x: any): x is Program => x.tag === "Program";
export const isDefineExp = (x: any): x is DefineExp => x.tag === "DefineExp";

export const isNumExp = (x: any): x is NumExp => x.tag === "NumExp";
export const isBoolExp = (x: any): x is BoolExp => x.tag === "BoolExp";
export const isStrExp = (x: any): x is StrExp => x.tag === "StrExp";
export const isPrimOp = (x: any): x is PrimOp => x.tag === "PrimOp";
export const isVarRef = (x: any): x is VarRef => x.tag === "VarRef";
export const isVarDecl = (x: any): x is VarDecl => x.tag === "VarDecl";
export const isAppExp = (x: any): x is AppExp => x.tag === "AppExp";
// L2
export const isIfExp = (x: any): x is IfExp => x.tag === "IfExp";
export const isProcExp = (x: any): x is ProcExp => x.tag === "ProcExp";
export const isBinding = (x: any): x is Binding => x.tag === "Binding";
export const isLetExp = (x: any): x is LetExp => x.tag === "LetExp";
// L3
export const isLitExp = (x: any): x is LitExp => x.tag === "LitExp";
// L4
export const isLetrecExp = (x: any): x is LetrecExp => x.tag === "LetrecExp";
export const isSetExp = (x: any): x is SetExp => x.tag === "SetExp";

// Type predicates for type unions
export const isExp = (x: any): x is Exp => isDefineExp(x) || isCExp(x);
export const isAtomicExp = (x: any): x is AtomicExp =>
    isNumExp(x) || isBoolExp(x) || isStrExp(x) ||
    isPrimOp(x) || isVarRef(x);
export const isCompoundExp = (x: any): x is CompoundExp =>
    isAppExp(x) || isIfExp(x) || isProcExp(x) || isLitExp(x) || isLetExp(x) ||
    isLetrecExp(x) || isSetExp(x);
export const isCExp = (x: any): x is CExp =>
    isAtomicExp(x) || isCompoundExp(x);


// ========================================================
// Parsing

export const parse = (x: string): Parsed | Error =>
    parseSexp(p(x));

export const parseSexp = (sexp: StringTree): Parsed | Error =>
    isEmpty(sexp) ? Error("Parse: Unexpected empty") :
    isArray(sexp) ? parseCompound(sexp) :
    isString(sexp) ? parseAtomic(sexp) :
    isSexpString(sexp) ? parseAtomic(sexp) :
    Error(`Parse: Unexpected type ${sexp}`);

// const parseCompound = (sexps: StringTree[]): Program | DefineExp | CExp | Error =>
//     first(sexps) === "L4" ? parseProgram(map(parseSexp, rest(sexps))) :
//     first(sexps) === "define" ? safeF((val: CExp) => makeDefineExp(makeVarDecl(sexps[1]), val))(parseCExp(sexps[2])) :
//     parseCExp(sexps);

const parseCompound = (sexps: StringTree[]): Program | DefineExp | CExp | Error => {
    const form = sexps[0];
    if (isString(form)) {
        if (form === "L4") {
            return parseProgram(map(parseSexp, rest(sexps)))
        } else if (form === "define") {
            const v = sexps[1];
            if (isString(v)) {
                return safeF((val: CExp) => makeDefineExp(makeVarDecl(v), val))(parseCExp(sexps[2]));
            } else {
                return Error(`${v} is not a valid variable for "define" expression`);
            }
        } else {
            return parseCExp(sexps);
        }
    } else {
        return parseCExp(sexps);
    }
}

const parseProgram = (es: Array<Parsed | Error>): Program | Error =>
    isEmpty(es) ? Error("Empty program") :
    allT(isExp, es) ? makeProgram(es) :
    hasNoError(es) ? Error(`Program cannot be embedded in another program - ${es}`) :
    Error(getErrorMessages(es));

export const parseAtomic = (sexp: string | String): CExp =>
    sexp === "#t" ? makeBoolExp(true) :
    sexp === "#f" ? makeBoolExp(false) :
    isString(sexp) && isNumericString(sexp) ? makeNumExp(+sexp) :
    isSexpString(sexp) ? makeStrExp(sexp.toString()) :
    isPrimitiveOp(sexp) ? makePrimOp(sexp) :
    makeVarRef(sexp);

/*
    ;; <prim-op>  ::= + | - | * | / | < | > | = | not | and | or | eq? | string=?
    ;;                  | cons | car | cdr | pair? | number? | list
    ;;                  | boolean? | symbol? | string?      ##### L3
*/
const isPrimitiveOp = (x: string): boolean =>
    ["+", "-", "*", "/", ">", "<", "=", "not", "and", "or", 
     "eq?", "string=?", "cons", "car", "cdr", "list", "pair?",
     "list?", "number?", "boolean?", "symbol?", "string?"].includes(x);

export const parseCExp = (sexp: StringTree): CExp | Error =>
    isArray(sexp) ? parseCompoundCExp(sexp) :
    isString(sexp) ? parseAtomic(sexp) :
    isSexpString(sexp) ? parseAtomic(sexp) :
    Error("Unexpected type " + sexp);

const parseCompoundCExp = (sexps: StringTree[]): CExp | Error =>
    first(sexps) === "if" ? parseIfExp(sexps) :
    first(sexps) === "lambda" ? parseProcExp(sexps) :
    first(sexps) === "let" ? parseLetExp(sexps) :
    first(sexps) === "quote" ? parseLitExp(sexps) :
    first(sexps) === "letrec" ? parseLetrecExp(sexps) :
    first(sexps) === "set!" ? parseSetExp(rest(sexps)) :
    parseAppExp(sexps)

const parseAppExp = (sexps: StringTree[]): AppExp | Error =>
    safeFL((cexps: CExp[]) => makeAppExp(first(cexps), rest(cexps)))(map(parseCExp, sexps));

const parseIfExp = (sexps: StringTree[]): IfExp | Error =>
    safeFL((cexps: CExp[]) => makeIfExp(cexps[0], cexps[1], cexps[2]))(map(parseCExp, rest(sexps)));

const parseProcExp = (sexps: StringTree[]): ProcExp | Error => {
    const vars = sexps[1];
    if (isArray(vars) && allT(isString, vars)) {
        return safeFL((body: CExp[]) => makeProcExp( map(makeVarDecl, vars), body))(map(parseCExp, rest(rest(sexps))));
    } else {
        return Error("Invalid vars for ProcExp");
    }
}

const parseBindings = (bdgs: StringTree[]): Binding[] | Error => {
    if (allT(isArray, bdgs)) {
        const vars = map(first, bdgs);
        if (!allT(isString, vars)) {
            return Error(`Bad bindings ${bdgs}`);
        }
        const vals = map(pair => parseCExp(second(pair)), bdgs);
        if (!hasNoError(vals)) {
            return Error(`Bad value ${bdgs}`);
        }
        return zipWith(makeBinding, vars, vals);
    } else {
        return Error(`Bad bindings ${bdgs}`);
    }
}

const parseLetExp = (sexps: StringTree[]): LetExp | Error => {
    if (isArray(sexps[1])) {
        let bindings = parseBindings(sexps[1]);
        if (isError(bindings)) {
            return bindings;
        }
        let body: Array<CExp | Error> = map(parseCExp, rest(rest(sexps)));
        if (!hasNoError(body)) {
            return Error(`Parse: Bad let: ${getErrorMessages(body)}`);
        } else {
            return makeLetExp(bindings, body);
        }
    } else {
        return Error(`Bad bindings ${sexps[1]}`);
    }
}

const parseLetrecExp = (sexps: StringTree[]): LetrecExp | Error => {
    if (isArray(sexps[1])) {
        let bindings = parseBindings(sexps[1]);
        if (isError(bindings)) {
            return bindings;
        }
        let body: Array<CExp | Error> = map(parseCExp, rest(rest(sexps)));
        if (!hasNoError(body)) {
            return Error(`Parse: Bad letrec: ${getErrorMessages(body)}`);
        } else {
            return makeLetrecExp(bindings, body);
        }
    } else {
        return Error(`Bad bindings ${sexps[1]}`);
    }
}

const parseSetExp = (es: StringTree[]): SetExp | Error => {
    if (es.length !== 2) {
        return Error(`set! should be (set! var val) - ${es}`);
    } else if (!isString(es[0])) {
        return Error(`Expected (set! <var> <CExp>) - ${es[0]}`)
    } else {
        const v = es[0];
        return safeF((val: CExp) => makeSetExp(makeVarRef(v), val))(parseCExp(es[1]));
    }
}

// LitExp has the shape (quote <sexp>)
export const parseLitExp = (sexps: StringTree[]): LitExp | Error =>
    safeF(makeLitExp)(parseSExp(second(sexps)));

export const isDottedPair = (sexps: StringTree[]): boolean =>
    sexps.length === 3 && 
    sexps[1] === "."

export const makeDottedPair = (sexps : StringTree[]): SExp | Error =>
    safeF2(makeCompoundSExp)(parseSExp(sexps[0]), parseSExp(sexps[2]))

// x is the output of p (sexp parser)
export const parseSExp = (x: StringTree): SExp | Error =>
    x === "#t" ? true :
    x === "#f" ? false :
    isString(x) && isNumericString(x) ? +x :
    isSexpString(x) ? x.toString() :
    isString(x) ? makeSymbolSExp(x) :
    x.length === 0 ? makeEmptySExp() :
    isDottedPair(x) ? makeDottedPair(x) :
    isArray(x) ? (
        // fail on (x . y z)
        x[0] === '.' ? Error("Bad dotted sexp: " + x) : 
        safeF2(makeCompoundSExp)(parseSExp(first(x)), parseSExp(rest(x)))) :
    Error(`Bad literal expression: ${x}`);


// ==========================================================================
// Unparse: Map an AST to a concrete syntax string.

// Add a quote for symbols, empty and compound sexp - strings and numbers are not quoted.
const unparseLitExp = (le: LitExp): string =>
    isEmptySExp(le.val) ? `'()` :
    isSymbolSExp(le.val) ? `'${valueToString(le.val)}` :
    isCompoundSExp(le.val) ? `'${valueToString(le.val)}` :
    `${le.val}`;

const unparseLExps = (les: Exp[]): string =>
    map(unparse, les).join(" ");

const unparseProcExp = (pe: ProcExp): string => 
    `(lambda (${map((p: VarDecl) => p.var, pe.args).join(" ")}) ${unparseLExps(pe.body)})`

const unparseBindings = (bdgs: Binding[]): string =>
    map((b: Binding) => `(${b.var.var} ${unparse(b.val)})`, bdgs).join(" ");

const unparseLetExp = (le: LetExp) : string => 
    `(let (${unparseBindings(le.bindings)}) ${unparseLExps(le.body)})`

const unparseLetrecExp = (le: LetrecExp): string => 
    `(letrec (${unparseBindings(le.bindings)}) ${unparseLExps(le.body)})`

const unparseSetExp = (se: SetExp): string =>
    `(set! ${se.var.var} ${unparse(se.val)})`;

export const unparse = (exp: Parsed | Error): string =>
    isError(exp) ? exp.message :
    isBoolExp(exp) ? valueToString(exp.val) :
    isNumExp(exp) ? valueToString(exp.val) :
    isStrExp(exp) ? valueToString(exp.val) :
    isLitExp(exp) ? unparseLitExp(exp) :
    isVarRef(exp) ? exp.var :
    isProcExp(exp) ? unparseProcExp(exp) :
    isIfExp(exp) ? `(if ${unparse(exp.test)} ${unparse(exp.then)} ${unparse(exp.alt)})` :
    isAppExp(exp) ? `(${unparse(exp.rator)} ${unparseLExps(exp.rands)})` :
    isPrimOp(exp) ? exp.op :
    isLetExp(exp) ? unparseLetExp(exp) :
    isLetrecExp(exp) ? unparseLetrecExp(exp) :
    isSetExp(exp) ? unparseSetExp(exp) :
    isDefineExp(exp) ? `(define ${exp.var.var} ${unparse(exp.val)})` :
    isProgram(exp) ? `(L4 ${unparseLExps(exp.exps)})` :
    "";
