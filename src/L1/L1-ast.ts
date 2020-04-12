/// <reference path="../shared/s-expression.d.ts" />
// L1 Parser
// =========

// A parser provides 2 components to the clients:
// - Type definitions for the AST of the language (with type predicates, constructors, getters)
// - A parser function which constructs AST values from strings.

import { isString, isArray, isNumericString, isIdentifier } from '../shared/type-predicates';
import { first, rest, second, isEmpty } from '../shared/list';
import { Result, makeOk, makeFailure, bind, mapResult, safe2 } from "../shared/result";

// ===============
// AST type models

// A toplevel expression in L1 - can appear in a program
export type Exp = DefineExp | CExp;
// An expression which can be embedded inside other expressions (constituent expression)
export type CExp = NumExp | BoolExp | PrimOp | VarRef | AppExp;

export interface Program {tag: "Program"; exps: Exp[]; };

export interface DefineExp {tag: "DefineExp"; var: VarDecl; val: CExp; };
export interface NumExp {tag: "NumExp"; val: number; };
export interface BoolExp {tag: "BoolExp"; val: boolean; };
export interface PrimOp {tag: "PrimOp", op: string; };
export interface VarRef {tag: "VarRef", var: string; };
export interface VarDecl {tag: "VarDecl", var: string; };
export interface AppExp {tag: "AppExp", rator: CExp, rands: CExp[]; };

// Type value constructors for disjoint types
export const makeProgram = (exps: Exp[]): Program => ({tag: "Program", exps: exps});
export const makeDefineExp = (v: VarDecl, val: CExp): DefineExp =>
    ({tag: "DefineExp", var: v, val: val});
export const makeNumExp = (n: number): NumExp => ({tag: "NumExp", val: n});
export const makeBoolExp = (b: boolean): BoolExp => ({tag: "BoolExp", val: b});
export const makePrimOp = (op: string): PrimOp => ({tag: "PrimOp", op: op});
export const makeVarRef = (v: string): VarRef => ({tag: "VarRef", var: v});
export const makeVarDecl = (v: string): VarDecl => ({tag: "VarDecl", var: v});
export const makeAppExp = (rator: CExp, rands: CExp[]): AppExp =>
    ({tag: "AppExp", rator: rator, rands: rands});

// Type predicates for disjoint types
export const isProgram = (x: any): x is Program => x.tag === "Program";
export const isDefineExp = (x: any): x is DefineExp => x.tag === "DefineExp";
export const isNumExp = (x: any): x is NumExp => x.tag === "NumExp";
export const isBoolExp = (x: any): x is BoolExp => x.tag === "BoolExp";
export const isPrimOp = (x: any): x is PrimOp => x.tag === "PrimOp";
export const isVarRef = (x: any): x is VarRef => x.tag === "VarRef";
export const isVarDecl = (x: any): x is VarDecl => x.tag === "VarDecl";
export const isAppExp = (x: any): x is AppExp => x.tag === "AppExp";

// Type predicates for type unions
export const isExp = (x: any): x is Exp => isDefineExp(x) || isCExp(x);
export const isCExp = (x: any): x is CExp => 
    isNumExp(x) || isBoolExp(x) || isPrimOp(x) || isVarRef(x) || isAppExp(x);

// ========================================================
// Parsing

// Make sure to run "npm install ramda s-expression --save"
import { Sexp, Token } from "s-expression";
import parseSexp, { isToken } from "../shared/parser"

// combine Sexp parsing with the L1 parsing
export const parseL1 = (x: string): Result<Program> =>
    bind(parseSexp(x), parseL1Program);

// L1 concrete syntax
// <Program> -> (L1 <Exp>+)
// <Exp> -> <DefineExp> | <CExp>
// <DefineExp> -> (define <varDecl> <CExp>)
// <CExp> -> <AtomicExp> | <AppExp>
// <AtomicExp> -> <number> | <boolean> | <primOp>
// <AppExp> -> (<CExp>+)

// <Program> -> (L1 <Exp>+)
export const parseL1Program = (sexp: Sexp): Result<Program> =>
    isString(sexp) && sexp.length === 0 ? makeFailure("Unexpected empty program") :
    isToken(sexp) ? makeFailure("Program cannot be a single token") :
    isArray(sexp) ? parseL1GoodProgram(first(sexp), rest(sexp)) :
    makeFailure("Unexpected type " + sexp);

const parseL1GoodProgram = (keyword: Sexp, body: Sexp[]): Result<Program> =>
    keyword === "L1" && !isEmpty(body) ? bind(mapResult(parseL1Exp, body),
                                              (exps: Exp[]) => makeOk(makeProgram(exps))) :
    makeFailure("Program must be of the form (L1 <exp>+)");

// Exp -> <DefineExp> | <Cexp>
export const parseL1Exp = (sexp: Sexp): Result<Exp> =>
    isEmpty(sexp) ? makeFailure("Exp cannot be an empty list") :
    isArray(sexp) ? parseL1CompoundExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL1Atomic(sexp) :
    makeFailure("Unexpected type " + sexp);

// Compound -> DefineExp | CompoundCExp
export const parseL1CompoundExp = (op: Sexp, params: Sexp[]): Result<Exp> => 
    op === "define"? parseDefine(params) :
    parseL1CompoundCExp(op, params);

// CompoundCExp -> AppExp
export const parseL1CompoundCExp = (op: Sexp, params: Sexp[]): Result<CExp> =>
    parseAppExp(op, params);

// DefineExp -> (define <varDecl> <CExp>)
export const parseDefine = (params: Sexp[]): Result<DefineExp> =>
    isEmpty(params) ? makeFailure("define missing 2 arguments") :
    isEmpty(rest(params)) ? makeFailure("define missing 1 arguments") :
    ! isEmpty(rest(rest(params))) ? makeFailure("define has too many arguments") :
    parseGoodDefine(first(params), second(params));

const parseGoodDefine = (variable: Sexp, val: Sexp): Result<DefineExp> =>
    ! isIdentifier(variable) ? makeFailure("First arg of define must be an identifier") :
    bind(parseL1CExp(val),
         (value: CExp) => makeOk(makeDefineExp(makeVarDecl(variable), value)));

// CExp -> AtomicExp | CompondCExp
export const parseL1CExp = (sexp: Sexp): Result<CExp> =>
    isEmpty(sexp) ? makeFailure("CExp cannot be an empty list") :
    isArray(sexp) ? parseL1CompoundCExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL1Atomic(sexp) :
    makeFailure("Unexpected type " + sexp);

// Atomic -> number | boolean | primitiveOp
export const parseL1Atomic = (token: Token): Result<CExp> =>
    token === "#t" ? makeOk(makeBoolExp(true)) :
    token === "#f" ? makeOk(makeBoolExp(false)) :
    isString(token) && isNumericString(token) ? makeOk(makeNumExp(+token)) :
    isString(token) && isPrimitiveOp(token) ? makeOk(makePrimOp(token)) :
    isString(token) ? makeOk(makeVarRef(token)) :
    makeFailure("Invalid atomic token: " + token);

export const isPrimitiveOp = (x: string): boolean =>
    ["+", "-", "*", "/", ">", "<", "=", "not"].includes(x)

// AppExp -> ( <cexp>+ )
export const parseAppExp = (op: Sexp, params: Sexp[]): Result<CExp> =>
    safe2((rator: CExp, rands: CExp[]) => makeOk(makeAppExp(rator, rands)))
        (parseL1CExp(op), mapResult(parseL1CExp, params))
