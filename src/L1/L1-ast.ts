/// <reference path="../shared/s-expression.d.ts" />
// ===========================================================
// AST type models
import { filter, map } from "ramda";
import { first, rest, isEmpty, isString, isArray, isNumericString } from '../shared/list';

export type Exp = DefineExp | CExp;
export type CExp = NumExp | BoolExp | PrimOp | VarRef | AppExp | Error;

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
export const isError = (x: any): x is Error => x instanceof Error;
export const hasError = (x: any[]): boolean => filter(isError, x).length > 0;

// Type predicates for type unions
export const isExp = (x: any): x is Exp => isDefineExp(x) || isCExp(x);
export const isCExp = (x: any): x is CExp => isNumExp(x) || isBoolExp(x) || isPrimOp(x) ||
    isVarRef(x) || isAppExp(x) || isError(x);

// ========================================================
// Parsing

// Make sure to run "npm install ramda s-expression --save"
import parseSexp, { StringTree } from "s-expression";

export const parseL1 = (x: string): Program | DefineExp | CExp | Error =>
    parseL1Sexp(parseSexp(x));

export const parseL1Sexp = (sexp: StringTree): Program | DefineExp | CExp | Error =>
    isEmpty(sexp) ? Error("Unexpected empty") :
    isArray(sexp) ? parseL1Compound(sexp) :
    isString(sexp) ? parseL1Atomic(sexp) :
    Error("Unexpected type" + sexp);

// There are type errors which we will address in L3 with more precise
// type definitions for the AST.
const parseL1Compound = (sexps: StringTree[]): Program | DefineExp | CExp | Error =>
    // @ts-ignore: type error
    first(sexps) === "L1" ? makeProgram(map(parseL1Sexp, rest(sexps))) :
    first(sexps) === "define" && isString(sexps[1]) ? makeDefineExp(makeVarDecl(sexps[1]), parseL1CExp(sexps[2])) :
    parseL1CExp(sexps);

const parseL1Atomic = (sexp: string): CExp =>
    sexp === "#t" ? makeBoolExp(true) :
    sexp === "#f" ? makeBoolExp(false) :
    isNumericString(sexp) ? makeNumExp(+sexp) :
    isPrimitiveOp(sexp) ? makePrimOp(sexp) :
    makeVarRef(sexp);

const isPrimitiveOp = (x: string): boolean =>
    ["+", "-", "*", "/", ">", "<", "=", "not"].includes(x)

const parseL1CExp = (sexp: any): CExp | Error =>
    isArray(sexp) ? makeAppExp(parseL1CExp(first(sexp)),
                               map(parseL1CExp, rest(sexp))) :
    isString(sexp) ? parseL1Atomic(sexp) :
    Error("Unexpected type" + sexp);
