// ===========================================================
// AST type models
import { map, zipWith } from "ramda";
import { makeEmptySExp, makeSymbolSExp, SExp, makeCompoundSExp, valueToString } from './L3-value'
import { first, second, rest, allT, isArray, isString, isEmpty, isSexpString, isNumericString } from "../shared/list";
// @ts-ignore
import p from "s-expression";
import {hasNoError, safeF, safeFL, safeF2, getErrorMessages} from '../shared/error'

/*
;; =============================================================================
;; Scheme Parser
;;
;; L2 extends L1 with support for IfExp and ProcExp
;; L3 extends L2 with support for:
;; - Pair datatype
;; - The empty-list literal expression
;; - Compound literal expressions denoted with quote
;; - Primitives: cons, car, cdr, pair?, number?, boolean?, symbol?, string?, list
;; - Primitives: and, or, not
;; - The Let abbreviation is also supported.

;; <program> ::= (L3 <exp>+) // Program(exps:List(Exp))
;; <exp> ::= <define> | <cexp>              / DefExp | CExp
;; <define> ::= ( define <var> <cexp> )     / DefExp(var:VarDecl, val:CExp)
;; <var> ::= <identifier>                   / VarRef(var:string)
;; <cexp> ::= <number>                      / NumExp(val:number)
;;         |  <boolean>                     / BoolExp(val:boolean)
;;         |  <string>                      / StrExp(val:string)
;;         |  ( lambda ( <var>* ) <cexp>+ ) / ProcExp(params:VarDecl[], body:CExp[]))
;;         |  ( if <cexp> <cexp> <cexp> )   / IfExp(test: CExp, then: CExp, alt: CExp)
;;         |  ( let ( binding* ) <cexp>+ )  / LetExp(bindings:Binding[], body:CExp[]))
;;         |  ( quote <sexp> )              / LitExp(val:SExp)
;;         |  ( <cexp> <cexp>* )            / AppExp(operator:CExp, operands:CExp[]))
;; <binding>  ::= ( <var> <cexp> )           / Binding(var:VarDecl, val:Cexp)
;; <prim-op>  ::= + | - | * | / | < | > | = | not |  and | or | eq? | string=?
;;                  | cons | car | cdr | pair? | number? | list 
;;                  | boolean? | symbol? | string?      ##### L3
;; <num-exp>  ::= a number token
;; <bool-exp> ::= #t | #f
;; <var-ref>  ::= an identifier token
;; <var-decl> ::= an identifier token
;; <sexp>     ::= symbol | number | bool | string | 
;;                (<sexp>+ . <sexp>) | ( <sexp>* )       ##### L3
*/

// A value returned by parseL3
export type Parsed = Exp | Program;

export type Exp = DefineExp | CExp;
export type AtomicExp = NumExp | BoolExp | StrExp | PrimOp | VarRef;
export type CompoundExp = AppExp | IfExp | ProcExp | LetExp | LitExp;
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

// Type predicates for type unions
export const isExp = (x: any): x is Exp => isDefineExp(x) || isCExp(x);
export const isAtomicExp = (x: any): x is AtomicExp =>
    isNumExp(x) || isBoolExp(x) || isStrExp(x) ||
    isPrimOp(x) || isVarRef(x);
export const isCompoundExp = (x: any): x is CompoundExp =>
    isAppExp(x) || isIfExp(x) || isProcExp(x) || isLitExp(x) || isLetExp(x);
export const isCExp = (x: any): x is CExp =>
    isAtomicExp(x) || isCompoundExp(x);

// ========================================================
// Parsing

export const parseL3 = (x: string): Parsed | Error =>
    parseL3Sexp(p(x));

export const parseL3Sexp = (sexp: any): Parsed | Error =>
    isEmpty(sexp) ? Error("Parse: Unexpected empty") :
    isArray(sexp) ? parseL3Compound(sexp) :
    isString(sexp) ? parseL3Atomic(sexp) :
    isSexpString(sexp) ? parseL3Atomic(sexp) :
    Error(`Parse: Unexpected type ${sexp}`);

const parseL3Compound = (sexps: any[]): Program | DefineExp | CExp | Error =>
    first(sexps) === "L3" ? parseProgram(map(parseL3Sexp, rest(sexps))) :
    first(sexps) === "define" ? 
    safeF((val: CExp) => makeDefineExp(makeVarDecl(sexps[1]), val))(parseL3CExp(sexps[2])) :
    parseL3CExp(sexps);

const parseProgram = (es: Array<Parsed | Error>): Program | Error =>
    isEmpty(es) ? Error("Empty program") :
    allT(isExp, es) ? makeProgram(es) :
    hasNoError(es) ? Error(`Program cannot be embedded in another program - ${es}`) :
    Error(getErrorMessages(es));

const parseL3Atomic = (sexp: string): CExp =>
    sexp === "#t" ? makeBoolExp(true) :
    sexp === "#f" ? makeBoolExp(false) :
    isNumericString(sexp) ? makeNumExp(+sexp) :
    isSexpString(sexp) ? makeStrExp(sexp.toString()) :
    isPrimitiveOp(sexp) ? makePrimOp(sexp) :
    makeVarRef(sexp);

/*
    ;; <prim-op>  ::= + | - | * | / | < | > | = | not | and | or | eq? | string=?
    ;;                  | cons | car | cdr | pair? | number? | list
    ;;                  | boolean? | symbol? | string?      ##### L3
*/
const isPrimitiveOp = (x: string): boolean =>
    x === "+" ||
    x === "-" ||
    x === "*" ||
    x === "/" ||
    x === ">" ||
    x === "<" ||
    x === "=" ||
    x === "not" ||
    x === "and" ||
    x === "or" ||
    x === "eq?" ||
    x === "string=?" ||
    x === "cons" ||
    x === "car" ||
    x === "cdr" ||
    x === "list" ||
    x === "pair?" ||
    x === "number?" ||
    x === "boolean?" ||
    x === "symbol?" ||
    x === "string?";

export const parseL3CExp = (sexp: any): CExp | Error =>
    isArray(sexp) ? parseL3CompoundCExp(sexp) :
    isString(sexp) ? parseL3Atomic(sexp) :
    isSexpString(sexp) ? parseL3Atomic(sexp) :
    Error("Unexpected type" + sexp);

const parseL3CompoundCExp = (sexps: any[]): CExp | Error =>
    first(sexps) === "if" ? parseIfExp(sexps) :
    first(sexps) === "lambda" ? parseProcExp(sexps) :
    first(sexps) === "let" ? parseLetExp(sexps) :
    first(sexps) === "quote" ? parseLitExp(sexps) :
    parseAppExp(sexps)

const parseAppExp = (sexps: any[]): AppExp | Error =>
    safeFL((cexps: CExp[]) => makeAppExp(first(cexps), rest(cexps)))(map(parseL3CExp, sexps));

const parseIfExp = (sexps: any[]): IfExp | Error =>
    safeFL((cexps: CExp[]) => makeIfExp(cexps[0], cexps[1], cexps[2]))(map(parseL3CExp, rest(sexps)));

const parseProcExp = (sexps: any[]): ProcExp | Error =>
    safeFL((body: CExp[]) => makeProcExp( map(makeVarDecl, sexps[1]), body))
        (map(parseL3CExp, rest(rest(sexps))));

const parseLetExp = (sexps: any[]): LetExp | Error => {
    let bdgs = sexps[1];
    let vars = map(first, bdgs);
    if (! (vars instanceof Array)) {
        return Error(`Bad bindings in let ${bdgs}`);
    }
    if (!allT(isString, vars)) {
        return Error(`Bad bindings in let ${bdgs}`);
    }
    let vals = map((pair) => parseL3CExp(second(pair)), bdgs);
    if (!hasNoError(vals)) {
        return Error(`Bad value in let vars ${bdgs}`);
    }
    let bindings = zipWith(makeBinding, vars, vals);
    let body: Array<CExp | Error> = map(parseL3CExp, rest(rest(sexps)));
    if (!hasNoError(body)) {
        return Error(`Parse: Bad let: ${getErrorMessages(body)}`);
    } else {
        return makeLetExp(bindings, body);
    }
}

// sexps has the shape (quote <sexp>)
export const parseLitExp = (sexps: any[]): LitExp | Error =>
    safeF(makeLitExp)(parseSExp(second(sexps)));

export const isDottedPair = (sexps: any[]): boolean =>
    sexps.length === 3 && 
    sexps[1] === "."

export const makeDottedPair = (sexps : any[]): SExp | Error =>
    safeF2(makeCompoundSExp)(parseSExp(sexps[0]), parseSExp(sexps[2]))

// x is the output of p (sexp parser)
export const parseSExp = (x: any): SExp | Error =>
    x === "#t" ? true :
    x === "#f" ? false :
    isNumericString(x) ? +x :
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

import { isSymbolSExp, isEmptySExp, isCompoundSExp } from './L3-value';
import { isError } from '../shared/error';

// Add a quote for symbols, empty and compound sexp - strings and numbers are not quoted.
const unparseLitExp = (le: LitExp): string =>
    isEmptySExp(le.val) ? `'()` :
    isSymbolSExp(le.val) ? `'${valueToString(le.val)}` :
    isCompoundSExp(le.val) ? `'${valueToString(le.val)}` :
    `${le.val}`;

const unparseLExps = (les: Exp[]): string =>
    map(unparseL3, les).join(" ");

const unparseProcExp = (pe: ProcExp): string => 
    `(lambda (${map((p: VarDecl) => p.var, pe.args).join(" ")}) ${unparseLExps(pe.body)})`

const unparseLetExp = (le: LetExp) : string => 
    `(let (${map((b: Binding) => `(${b.var.var} ${unparseL3(b.val)})`, le.bindings).join(" ")}) ${unparseLExps(le.body)})`

export const unparseL3 = (exp: Parsed | Error): string =>
    isError(exp) ? exp.message :
    isBoolExp(exp) ? valueToString(exp.val) :
    isNumExp(exp) ? valueToString(exp.val) :
    isStrExp(exp) ? valueToString(exp.val) :
    isLitExp(exp) ? unparseLitExp(exp) :
    isVarRef(exp) ? exp.var :
    isProcExp(exp) ? unparseProcExp(exp) :
    isIfExp(exp) ? `(if ${unparseL3(exp.test)} ${unparseL3(exp.then)} ${unparseL3(exp.alt)})` :
    isAppExp(exp) ? `(${unparseL3(exp.rator)} ${unparseLExps(exp.rands)})` :
    isPrimOp(exp) ? exp.op :
    isLetExp(exp) ? unparseLetExp(exp) :
    isDefineExp(exp) ? `(define ${exp.var.var} ${unparseL3(exp.val)})` :
    isProgram(exp) ? `(L3 ${unparseLExps(exp.exps)})` :
    "";
