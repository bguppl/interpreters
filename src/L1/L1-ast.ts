// L1 Parser
// =========

// A parser provides 2 components to the clients:
// - Type definitions for the AST of the language (with type predicates, constructors, getters)
// - A parser function which constructs AST values from strings.
import { pipe } from "fp-ts/function";
import * as E from "fp-ts/Either";
import { isString, isArray, isNumericString, isIdentifier } from '../shared/type-predicates';
import { first, rest, second, isEmpty } from '../shared/list';

// ===============
// AST type models

// A toplevel expression in L1 - can appear in a program
export type Exp = DefineExp | CExp;
// An expression which can be embedded inside other expressions (constituent expression)
export type CExp = NumExp | BoolExp | PrimOp | VarRef | AppExp;

export interface Program {tag: "Program"; exps: readonly Exp[]; }

export interface DefineExp {tag: "DefineExp"; var: VarDecl; val: CExp; }
export interface NumExp {tag: "NumExp"; val: number; }
export interface BoolExp {tag: "BoolExp"; val: boolean; }
export interface PrimOp {tag: "PrimOp", op: string; }
export interface VarRef {tag: "VarRef", var: string; }
export interface VarDecl {tag: "VarDecl", var: string; }
export interface AppExp {tag: "AppExp", rator: CExp, rands: readonly CExp[]; }

// Type value constructors for disjoint types
export const makeProgram = (exps: readonly Exp[]): Program => ({tag: "Program", exps: exps});
export const makeDefineExp = (v: VarDecl, val: CExp): DefineExp =>
    ({tag: "DefineExp", var: v, val: val});
export const makeNumExp = (n: number): NumExp => ({tag: "NumExp", val: n});
export const makeBoolExp = (b: boolean): BoolExp => ({tag: "BoolExp", val: b});
export const makePrimOp = (op: string): PrimOp => ({tag: "PrimOp", op: op});
export const makeVarRef = (v: string): VarRef => ({tag: "VarRef", var: v});
export const makeVarDecl = (v: string): VarDecl => ({tag: "VarDecl", var: v});
export const makeAppExp = (rator: CExp, rands: readonly CExp[]): AppExp =>
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
import { parse as parseSexp, isToken } from "../shared/parser";

// combine Sexp parsing with the L1 parsing
export const parseL1 = (x: string): E.Either<string, Program> =>
    pipe(x, parseSexp, E.chain(parseL1Program));

// L1 concrete syntax
// <Program> -> (L1 <Exp>+)
// <Exp> -> <DefineExp> | <CExp>
// <DefineExp> -> (define <varDecl> <CExp>)
// <CExp> -> <AtomicExp> | <AppExp>
// <AtomicExp> -> <number> | <boolean> | <primOp>
// <AppExp> -> (<CExp>+)

// <Program> -> (L1 <Exp>+)
export const parseL1Program = (sexp: Sexp): E.Either<string, Program> =>
    sexp === "" || isEmpty(sexp) ? E.left("Unexpected empty program") :
    isToken(sexp) ? E.left("Program cannot be a single token") :
    isArray(sexp) ? parseL1GoodProgram(first(sexp), rest(sexp)) :
    sexp;

const parseL1GoodProgram = (keyword: Sexp, body: readonly Sexp[]): E.Either<string, Program> =>
    keyword === "L1" && !isEmpty(body) ? pipe(body, E.traverseArray(parseL1Exp), E.map(makeProgram)) :
    E.left("Program must be of the form (L1 <exp>+)");

// Exp -> <DefineExp> | <Cexp>
export const parseL1Exp = (sexp: Sexp): E.Either<string, Exp> =>
    isEmpty(sexp) ? E.left("Exp cannot be an empty list") :
    isArray(sexp) ? parseL1CompoundExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL1Atomic(sexp) :
    sexp;
    
// Compound -> DefineExp | CompoundCExp
export const parseL1CompoundExp = (op: Sexp, params: readonly Sexp[]): E.Either<string, Exp> => 
    op === "define"? parseDefine(params) :
    parseL1CompoundCExp(op, params);

// CompoundCExp -> AppExp
export const parseL1CompoundCExp = (op: Sexp, params: readonly Sexp[]): E.Either<string, CExp> =>
    parseAppExp(op, params);

// DefineExp -> (define <varDecl> <CExp>)
export const parseDefine = (params: readonly Sexp[]): E.Either<string, DefineExp> =>
    isEmpty(params) ? E.left("define missing 2 arguments") :
    isEmpty(rest(params)) ? E.left("define missing 1 arguments") :
    ! isEmpty(rest(rest(params))) ? E.left("define has too many arguments") :
    parseGoodDefine(first(params), second(params));

const parseGoodDefine = (variable: Sexp, val: Sexp): E.Either<string, DefineExp> =>
    ! isIdentifier(variable) ? E.left("First arg of define must be an identifier") :
    pipe(val, parseL1CExp, E.map(value => makeDefineExp(makeVarDecl(variable), value)));

// CExp -> AtomicExp | CompondCExp
export const parseL1CExp = (sexp: Sexp): E.Either<string, CExp> =>
    isEmpty(sexp) ? E.left("CExp cannot be an empty list") :
    isArray(sexp) ? parseL1CompoundCExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL1Atomic(sexp) :
    sexp;

// Atomic -> number | boolean | primitiveOp
export const parseL1Atomic = (token: Token): E.Either<string, CExp> =>
    token === "#t" ? E.of(makeBoolExp(true)) :
    token === "#f" ? E.of(makeBoolExp(false)) :
    isString(token) && isNumericString(token) ? E.of(makeNumExp(+token)) :
    isString(token) && isPrimitiveOp(token) ? E.of(makePrimOp(token)) :
    isString(token) ? E.of(makeVarRef(token)) :
    E.left("Invalid atomic token: " + token);

export const isPrimitiveOp = (x: string): boolean =>
    ["+", "-", "*", "/", ">", "<", "=", "not"].includes(x)

// AppExp -> ( <cexp>+ )
export const parseAppExp = (op: Sexp, params: readonly Sexp[]): E.Either<string, CExp> =>
    pipe(parseL1CExp(op),
         E.chain(rator => pipe(params,
                               E.traverseArray(parseL1CExp),
                               E.map(rands => makeAppExp(rator, rands)))));
