// ===========================================================
// AST type models
import { map, zipWith } from "ramda";
import { Sexp, Token } from "s-expression";
import { allT, first, second, rest, isEmpty } from "../shared/list";
import { isArray, isString, isNumericString, isIdentifier } from "../shared/type-predicates";
import { parse as p, isSexpString, isToken } from "../shared/parser";
import { Result, makeOk, makeFailure, bind, mapResult, safe2 } from "../shared/result";
import { isSymbolSExp, isEmptySExp, isCompoundSExp } from './L4-value';
import { makeEmptySExp, makeSymbolSExp, SExpValue, makeCompoundSExp, valueToString } from './L4-value'

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

export interface Program {tag: "Program"; exps: Exp[]; }
export interface DefineExp {tag: "DefineExp"; var: VarDecl; val: CExp; }
export interface NumExp {tag: "NumExp"; val: number; }
export interface BoolExp {tag: "BoolExp"; val: boolean; }
export interface StrExp {tag: "StrExp"; val: string; }
export interface PrimOp {tag: "PrimOp"; op: string; }
export interface VarRef {tag: "VarRef"; var: string; }
export interface VarDecl {tag: "VarDecl"; var: string; }
export interface AppExp {tag: "AppExp"; rator: CExp; rands: CExp[]; }
// L2
export interface IfExp {tag: "IfExp"; test: CExp; then: CExp; alt: CExp; }
export interface ProcExp {tag: "ProcExp"; args: VarDecl[], body: CExp[]; }
export interface Binding {tag: "Binding"; var: VarDecl; val: CExp; }
export interface LetExp {tag: "LetExp"; bindings: Binding[]; body: CExp[]; }
// L3
export interface LitExp {tag: "LitExp"; val: SExpValue; }
// L4
export interface LetrecExp {tag: "LetrecExp"; bindings: Binding[]; body: CExp[]; }
export interface SetExp {tag: "SetExp", var: VarRef; val: CExp; }

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
export const makeLitExp = (val: SExpValue): LitExp =>
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

export const parseL4 = (x: string): Result<Program> =>
    bind(p(x), parseL4Program);

export const parseL4Program = (sexp: Sexp): Result<Program> =>
    sexp === "" || isEmpty(sexp) ? makeFailure("Unexpected empty program") :
    isToken(sexp) ? makeFailure("Program cannot be a single token") :
    isArray(sexp) ? parseL4GoodProgram(first(sexp), rest(sexp)) :
    makeFailure("Unexpected type " + sexp);

const parseL4GoodProgram = (keyword: Sexp, body: Sexp[]): Result<Program> =>
    keyword === "L4" && !isEmpty(body) ? bind(mapResult(parseL4Exp, body),
                                              (exps: Exp[]) => makeOk(makeProgram(exps))) :
    makeFailure("Program must be of the form (L4 <exp>+)");

export const parseL4Exp = (sexp: Sexp): Result<Exp> =>
    isEmpty(sexp) ? makeFailure("Exp cannot be an empty list") :
    isArray(sexp) ? parseL4CompoundExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL4Atomic(sexp) :
    makeFailure("Unexpected type " + sexp);

export const parseL4CompoundExp = (op: Sexp, params: Sexp[]): Result<Exp> => 
    op === "define" ? parseDefine(params) :
    parseL4CompoundCExp(op, params);

export const parseL4CompoundCExp = (op: Sexp, params: Sexp[]): Result<CExp> =>
    isString(op) && isSpecialForm(op) ? parseL4SpecialForm(op, params) :
    parseAppExp(op, params);

export const parseL4SpecialForm = (op: Sexp, params: Sexp[]): Result<CExp> =>
    isEmpty(params) ? makeFailure("Empty args for special form") :
    op === "if" ? parseIfExp(params) :
    op === "lambda" ? parseProcExp(first(params), rest(params)) :
    op === "let" ? parseLetExp(first(params), rest(params)) :
    op === "quote" ? parseLitExp(first(params)) :
    op === "letrec" ? parseLetrecExp(first(params), rest(params)) :
    op === "set!" ? parseSetExp(params) :
    makeFailure("Never");

export const parseDefine = (params: Sexp[]): Result<DefineExp> =>
    isEmpty(params) ? makeFailure("define missing 2 arguments") :
    isEmpty(rest(params)) ? makeFailure("define missing 1 arguments") :
    ! isEmpty(rest(rest(params))) ? makeFailure("define has too many arguments") :
    parseGoodDefine(first(params), second(params));

const parseGoodDefine = (variable: Sexp, val: Sexp): Result<DefineExp> =>
    ! isIdentifier(variable) ? makeFailure("First arg of define must be an identifier") :
    bind(parseL4CExp(val),
         (value: CExp) => makeOk(makeDefineExp(makeVarDecl(variable), value)));

export const parseL4Atomic = (token: Token): Result<CExp> =>
    token === "#t" ? makeOk(makeBoolExp(true)) :
    token === "#f" ? makeOk(makeBoolExp(false)) :
    isString(token) && isNumericString(token) ? makeOk(makeNumExp(+token)) :
    isString(token) && isPrimitiveOp(token) ? makeOk(makePrimOp(token)) :
    isString(token) ? makeOk(makeVarRef(token)) :
    makeOk(makeStrExp(token.toString()));

export const parseL4CExp = (sexp: Sexp): Result<CExp> =>
    isEmpty(sexp) ? makeFailure("CExp cannot be an empty list") :
    isArray(sexp) ? parseL4CompoundCExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL4Atomic(sexp) :
    makeFailure("Unexpected type " + sexp);

/*
    ;; <prim-op>  ::= + | - | * | / | < | > | = | not | and | or | eq? | string=?
    ;;                  | cons | car | cdr | pair? | number? | list
    ;;                  | boolean? | symbol? | string?      ##### L3
*/
const isPrimitiveOp = (x: string): boolean =>
    ["+", "-", "*", "/", ">", "<", "=", "not", "and", "or", 
     "eq?", "string=?", "cons", "car", "cdr", "list", "pair?",
     "list?", "number?", "boolean?", "symbol?", "string?"].includes(x);

const isSpecialForm = (x: string): boolean =>
    ["if", "lambda", "let", "quote", "letrec", "set!"].includes(x);

const parseAppExp = (op: Sexp, params: Sexp[]): Result<AppExp> =>
    safe2((rator: CExp, rands: CExp[]) => makeOk(makeAppExp(rator, rands)))
        (parseL4CExp(op), mapResult(parseL4CExp, params));

const parseIfExp = (params: Sexp[]): Result<IfExp> =>
    params.length !== 3 ? makeFailure("Expression not of the form (if <cexp> <cexp> <cexp>)") :
    bind(mapResult(parseL4CExp, params),
         (cexps: CExp[]) => makeOk(makeIfExp(cexps[0], cexps[1], cexps[2])));

const parseProcExp = (vars: Sexp, body: Sexp[]): Result<ProcExp> =>
    isArray(vars) && allT(isString, vars) ? bind(mapResult(parseL4CExp, body),
                                                 (cexps: CExp[]) => makeOk(makeProcExp(map(makeVarDecl, vars), cexps))) :
    makeFailure(`Invalid vars for ProcExp`);

const isGoodBindings = (bindings: Sexp): bindings is [string, Sexp][] =>
    isArray(bindings) &&
    allT(isArray, bindings) &&
    allT(isIdentifier, map(first, bindings));

const parseBindings = (bindings: Sexp): Result<Binding[]> => {
    if (!isGoodBindings(bindings)) {
        return makeFailure(`Invalid bindings: ${bindings}`);
    }
    const vars = map(b => b[0], bindings);
    const valsResult = mapResult(binding => parseL4CExp(second(binding)), bindings);
    return bind(valsResult,
                (vals: CExp[]) => makeOk(zipWith(makeBinding, vars, vals)));
}

const parseLetExp = (bindings: Sexp, body: Sexp[]): Result<LetExp> =>
    safe2((bindings: Binding[], body: CExp[]) => makeOk(makeLetExp(bindings, body)))
        (parseBindings(bindings), mapResult(parseL4CExp, body));

const parseLetrecExp = (bindings: Sexp, body: Sexp[]): Result<LetrecExp> =>
    safe2((bindings: Binding[], body: CExp[]) => makeOk(makeLetrecExp(bindings, body)))
        (parseBindings(bindings), mapResult(parseL4CExp, body));

const parseSetExp = (params: Sexp[]): Result<SetExp> =>
    isEmpty(params) ? makeFailure("set! missing 2 arguments") :
    isEmpty(rest(params)) ? makeFailure("set! missing 1 argument") :
    ! isEmpty(rest(rest(params))) ? makeFailure("set! has too many arguments") :
    parseGoodSetExp(first(params), second(params));

const parseGoodSetExp = (variable: Sexp, val: Sexp): Result<SetExp> =>
    ! isIdentifier(variable) ? makeFailure("First arg of set! must be an identifier") :
    bind(parseL4CExp(val), (val: CExp) => makeOk(makeSetExp(makeVarRef(variable), val)));

// LitExp has the shape (quote <sexp>)
export const parseLitExp = (param: Sexp): Result<LitExp> =>
    bind(parseSExp(param), (sexp: SExpValue) => makeOk(makeLitExp(sexp)));

export const isDottedPair = (sexps: Sexp[]): boolean =>
    sexps.length === 3 && 
    sexps[1] === "."

export const makeDottedPair = (sexps : Sexp[]): Result<SExpValue> =>
    safe2((val1: SExpValue, val2: SExpValue) => makeOk(makeCompoundSExp(val1, val2)))
        (parseSExp(sexps[0]), parseSExp(sexps[2]));

// x is the output of p (sexp parser)
export const parseSExp = (sexp: Sexp): Result<SExpValue> =>
    sexp === "#t" ? makeOk(true) :
    sexp === "#f" ? makeOk(false) :
    isString(sexp) && isNumericString(sexp) ? makeOk(+sexp) :
    isSexpString(sexp) ? makeOk(sexp.toString()) :
    isString(sexp) ? makeOk(makeSymbolSExp(sexp)) :
    sexp.length === 0 ? makeOk(makeEmptySExp()) :
    isDottedPair(sexp) ? makeDottedPair(sexp) :
    isArray(sexp) ? (
        // fail on (x . y z)
        sexp[0] === '.' ? makeFailure("Bad dotted sexp: " + sexp) : 
        safe2((val1: SExpValue, val2: SExpValue) => makeOk(makeCompoundSExp(val1, val2)))
            (parseSExp(first(sexp)), parseSExp(rest(sexp)))) :
    makeFailure(`Bad literal expression: ${sexp}`);


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

export const unparse = (exp: Parsed): string =>
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
