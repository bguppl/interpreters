// ===========================================================
// AST type models for L5
// L5 extends L4 with:
// optional type annotations
import * as E from "fp-ts/Either";
import { map, zipWith } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { Sexp, Token } from 's-expression';
import { isCompoundSExp, isEmptySExp, isSymbolSExp, makeCompoundSExp, makeEmptySExp, makeSymbolSExp, SExpValue, valueToString } from './L5-value';
import { isTVar, makeFreshTVar, parseTExp, unparseTExp, TExp } from './TExp';
import { allT, first, rest, second, isEmpty } from '../shared/list';
import { parse as p, isToken, isSexpString } from "../shared/parser";
import { isArray, isString, isNumericString, isIdentifier } from "../shared/type-predicates";

/*
// =============================================================================
// Examples of type annotated programs
// (define [x : number] 5)
// (define [f : [number -> number]] (lambda ([x : number]) : number (* x x))
// (define [f : [number * number -> number]] (lambda ([x : number] [y : number]) : number (* x x))
// (define f (lambda ([x : number]) (* x x)))  // no type annotation on f and on return value of lambda
// (let (([a : number] 1)
//       ([b : boolean] #t))
//   (if b a (+ a 1)))
// (define [id : (T1 -> T1)] (lambda ([x : T1]) : T1 x))
;;
// The only changes in the syntax of L5 are optional type annotations in var-decl and proc-exp
;;
// <program> ::= (L5 <exp>+)                  / Program(exps:List(exp))
// <exp> ::= <define> | <cexp>                / DefExp | CExp
// <define> ::= ( define <var-decl> <cexp> )  / DefExp(var:VarDecl, val:CExp)
// <var> ::= <identifier>                     / VarRef(var:string)
// <cexp> ::= <number>                        / NumExp(val:number)
//         |  <boolean>                       / BoolExp(val:boolean)
//         |  <string>                        / StrExp(val:string)
//         |  <var-ref>
//         |  ( lambda ( <var-decl>* ) <TExp>? <cexp>+ ) / ProcExp(args:VarDecl[], body:CExp[], returnTE: TExp))
//         |  ( if <cexp> <cexp> <cexp> )     / IfExp(test: CExp, then: CExp, alt: CExp)
//         |  ( quote <sexp> )                / LitExp(val:SExp)
//         |  ( <cexp> <cexp>* )              / AppExp(operator:CExp, operands:CExp[]))
//         |  ( let ( <binding>* ) <cexp>+ )  / LetExp(bindings:Binding[], body:CExp[]))
//         |  ( letrec ( binding*) <cexp>+ )  / LetrecExp(bindings:Bindings[], body: CExp)
//         |  ( set! <var> <cexp>)            / SetExp(var: varRef, val: CExp)
// <binding>  ::= ( <var> <cexp> )            / Binding(var:VarDecl, val:Cexp)
// <prim-op>  ::= + | - | * | / | < | > | = | not |  eq? | string=?
//                  | cons | car | cdr | list? | number?
//                  | boolean? | symbol? | string?
//                  | display | newline
// <num-exp>  ::= a number token
// <bool-exp> ::= #t | #f
// <var-ref>  ::= an identifier token         / VarRef(var)
// <var-decl> ::= an identifier token | (var : TExp) / VarRef(var, TE: TExp) ##### L5
// <sexp>     ::= symbol | number | bool | string | ( <sexp>* )              ##### L3
*/

// A value returned by parseL5
export type Parsed = Exp | Program;

export type Exp = DefineExp | CExp;
export const isExp = (x: any): x is Exp => isDefineExp(x) || isCExp(x);

export type CExp =  AtomicExp | CompoundExp;
export const isCExp = (x: any): x is CExp => isAtomicExp(x) || isCompoundExp(x);

export type AtomicExp = NumExp | BoolExp | StrExp | PrimOp | VarRef;
export const isAtomicExp = (x: any): x is AtomicExp =>
    isNumExp(x) || isBoolExp(x) || isStrExp(x) ||
    isPrimOp(x) || isVarRef(x);

export type CompoundExp = AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp;
export const isCompoundExp = (x: any): x is CompoundExp =>
    isAppExp(x) || isIfExp(x) || isProcExp(x) || isLitExp(x) || isLetExp(x) || isLetrecExp(x) || isSetExp(x);
export const expComponents = (e: Exp): readonly CExp[] =>
    isIfExp(e) ? [e.test, e.then, e.alt] :
    isProcExp(e) ? e.body :
    isLetExp(e) ? [...e.body, ...pipe(e.bindings, map(b => b.val))] :
    isLetrecExp(e) ? [...e.body, ...pipe(e.bindings, map(b => b.val))] :
    isAppExp(e) ? [e.rator, ...e.rands] :
    isSetExp(e) ? [e.val] :
    isDefineExp(e) ? [e.val] :
    []; // Atomic expressions have no components

// Type definitions
export interface Program {tag: "Program"; exps: readonly Exp[]; }
export const makeProgram = (exps: readonly Exp[]): Program => ({tag: "Program", exps: exps});
export const isProgram = (x: any): x is Program => x.tag === "Program";

export interface DefineExp {tag: "DefineExp"; var: VarDecl; val: CExp; }
export const makeDefineExp = (v: VarDecl, val: CExp): DefineExp =>
    ({tag: "DefineExp", var: v, val: val});
export const isDefineExp = (x: any): x is DefineExp => x.tag === "DefineExp";

export interface NumExp {tag: "NumExp"; val: number; }
export const makeNumExp = (n: number): NumExp => ({tag: "NumExp", val: n});
export const isNumExp = (x: any): x is NumExp => x.tag === "NumExp";

export interface BoolExp {tag: "BoolExp"; val: boolean; }
export const makeBoolExp = (b: boolean): BoolExp => ({tag: "BoolExp", val: b});
export const isBoolExp = (x: any): x is BoolExp => x.tag === "BoolExp";

export interface StrExp {tag: "StrExp"; val: string; }
export const makeStrExp = (s: string): StrExp => ({tag: "StrExp", val: s});
export const isStrExp = (x: any): x is StrExp => x.tag === "StrExp";

export interface PrimOp {tag: "PrimOp"; op: PrimOpKeyword; }
export const makePrimOp = (op: PrimOpKeyword): PrimOp => ({tag: "PrimOp", op: op});
export const isPrimOp = (x: any): x is PrimOp => x.tag === "PrimOp";

export interface VarRef {tag: "VarRef"; var: string; }
export const makeVarRef = (v: string): VarRef => ({tag: "VarRef", var: v});
export const isVarRef = (x: any): x is VarRef => x.tag === "VarRef";

export interface VarDecl {tag: "VarDecl"; var: string; texp: TExp}
export const makeVarDecl = (v: string, te: TExp): VarDecl => ({tag: "VarDecl", var: v, texp: te});
export const isVarDecl = (x: any): x is VarDecl => x.tag === "VarDecl";

export interface AppExp {tag: "AppExp"; rator: CExp; rands: readonly CExp[]; }
export const makeAppExp = (rator: CExp, rands: readonly CExp[]): AppExp =>
    ({tag: "AppExp", rator: rator, rands: rands});
export const isAppExp = (x: any): x is AppExp => x.tag === "AppExp";

export interface IfExp {tag: "IfExp"; test: CExp; then: CExp; alt: CExp; }
export const makeIfExp = (test: CExp, then: CExp, alt: CExp): IfExp =>
    ({tag: "IfExp", test: test, then: then, alt: alt});
export const isIfExp = (x: any): x is IfExp => x.tag === "IfExp";

export interface ProcExp {tag: "ProcExp"; args: readonly VarDecl[], body: readonly CExp[]; returnTE: TExp }
export const makeProcExp = (args: readonly VarDecl[], body: readonly CExp[], returnTE: TExp): ProcExp =>
    ({tag: "ProcExp", args: args, body: body, returnTE: returnTE});
export const isProcExp = (x: any): x is ProcExp => x.tag === "ProcExp";

export interface Binding {tag: "Binding"; var: VarDecl; val: CExp; }
export const makeBinding = (v: VarDecl, val: CExp): Binding =>
    ({tag: "Binding", var: v, val: val});
export const isBinding = (x: any): x is Binding => x.tag === "Binding";

export interface LetExp {tag: "LetExp"; bindings: readonly Binding[]; body: readonly CExp[]; }
export const makeLetExp = (bindings: readonly Binding[], body: readonly CExp[]): LetExp =>
    ({tag: "LetExp", bindings: bindings, body: body});
export const isLetExp = (x: any): x is LetExp => x.tag === "LetExp";

export interface LitExp {tag: "LitExp"; val: SExpValue; }
export const makeLitExp = (val: SExpValue): LitExp => ({tag: "LitExp", val: val});
export const isLitExp = (x: any): x is LitExp => x.tag === "LitExp";

export interface LetrecExp {tag: "LetrecExp"; bindings: readonly Binding[]; body: readonly CExp[]; }
export const makeLetrecExp = (bindings: readonly Binding[], body: readonly CExp[]): LetrecExp =>
    ({tag: "LetrecExp", bindings: bindings, body: body});
export const isLetrecExp = (x: any): x is LetrecExp => x.tag === "LetrecExp";

export interface SetExp {tag: "SetExp"; var: VarRef; val: CExp; }
export const makeSetExp = (v: VarRef, val: CExp): SetExp =>
    ({tag: "SetExp", var: v, val: val});
export const isSetExp = (x: any): x is SetExp => x.tag === "SetExp";

// To help parser - define a type for reserved key words.
export type SpecialFormKeyword = "lambda" | "let" | "letrec" | "if" | "set!" | "quote";
const isSpecialFormKeyword = (x: string): x is SpecialFormKeyword =>
    ["if", "lambda", "let", "quote", "letrec", "set!"].includes(x);

/*
    ;; <prim-op>  ::= + | - | * | / | < | > | = | not | and | or | eq? | string=?
    ;;                  | cons | car | cdr | pair? | number? | list
    ;;                  | boolean? | symbol? | string?      ##### L3
*/
export type PrimOpKeyword = "+" | "-" | "*" | "/" | ">" | "<" | "=" | "not" | "and" | "or" | "eq?" | "string=?" | 
        "cons" | "car" | "cdr" | "list" | "pair?" | "list?" | "number?" | "boolean?" | "symbol?" | "string?" |
        "display" | "newline";
const isPrimOpKeyword = (x: string): x is PrimOpKeyword =>
    ["+", "-", "*", "/", ">", "<", "=", "not", "and", "or", 
     "eq?", "string=?", "cons", "car", "cdr", "list", "pair?",
     "list?", "number?", "boolean?", "symbol?", "string?", "display", "newline"].includes(x);

// ========================================================
// Parsing

export const parseL5 = (x: string): E.Either<string, Program> =>
    pipe(p(x), E.chain(parseL5Program));

export const parseL5Program = (sexp: Sexp): E.Either<string, Program> =>
    sexp === "" || isEmpty(sexp) ? E.left("Unexpected empty program") :
    isToken(sexp) ? E.left("Program cannot be a single token") :
    isArray(sexp) ? parseL5GoodProgram(first(sexp), rest(sexp)) :
    sexp;

const parseL5GoodProgram = (keyword: Sexp, body: readonly Sexp[]): E.Either<string, Program> =>
    keyword === "L5" && !isEmpty(body) ? pipe(body, E.traverseArray(parseL5Exp), E.map(makeProgram)) :
    E.left("Program must be of the form (L5 <exp>+)");

export const parseL5Exp = (sexp: Sexp): E.Either<string, Exp> =>
    isEmpty(sexp) ? E.left("Exp cannot be an empty list") :
    isArray(sexp) ? parseL5CompoundExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL5Atomic(sexp) :
    sexp;

export const parseL5CompoundExp = (op: Sexp, params: readonly Sexp[]): E.Either<string, Exp> =>
    op === "define" ? parseDefine(params) :
    parseL5CompoundCExp(op, params);

export const parseL5CompoundCExp = (op: Sexp, params: readonly Sexp[]): E.Either<string, CExp> =>
    isString(op) && isSpecialFormKeyword(op) ? parseL5SpecialForm(op, params) :
    parseAppExp(op, params);

export const parseL5SpecialForm = (op: SpecialFormKeyword, params: readonly Sexp[]): E.Either<string, CExp> =>
    isEmpty(params) ? E.left("Empty args for special form") :
    op === "if" ? parseIfExp(params) :
    op === "lambda" ? parseProcExp(first(params), rest(params)) :
    op === "let" ? parseLetExp(first(params), rest(params)) :
    op === "quote" ? parseLitExp(first(params)) :
    op === "letrec" ? parseLetrecExp(first(params), rest(params)) :
    op === "set!" ? parseSetExp(params) :
    op;

export const parseDefine = (params: readonly Sexp[]): E.Either<string, DefineExp> =>
    isEmpty(params) ? E.left("define missing 2 arguments") :
    isEmpty(rest(params)) ? E.left("define missing 1 arguments") :
    ! isEmpty(rest(rest(params))) ? E.left("define has too many arguments") :
    parseGoodDefine(first(params), second(params));

const parseGoodDefine = (variable: Sexp, val: Sexp): E.Either<string, DefineExp> =>
    ! isConcreteVarDecl(variable) ? E.left("First arg of define must be an identifier") :
    pipe(
        parseVarDecl(variable),
        E.chain(varDecl => pipe(
            parseL5CExp(val),
            E.map(val => makeDefineExp(varDecl, val))
        ))
    );

export const parseL5Atomic = (token: Token): E.Either<string, AtomicExp> =>
    token === "#t" ? E.of(makeBoolExp(true)) :
    token === "#f" ? E.of(makeBoolExp(false)) :
    isString(token) && isNumericString(token) ? E.of(makeNumExp(+token)) :
    isString(token) && isPrimOpKeyword(token) ? E.of(makePrimOp(token)) :
    isString(token) ? E.of(makeVarRef(token)) :
    E.of(makeStrExp(token.toString()));

export const parseL5CExp = (sexp: Sexp): E.Either<string, CExp> =>
    isEmpty(sexp) ? E.left("CExp cannot be an empty list") :
    isArray(sexp) ? parseL5CompoundCExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL5Atomic(sexp) :
    sexp;

const parseAppExp = (op: Sexp, params: readonly Sexp[]): E.Either<string, AppExp> =>
    pipe(
        parseL5CExp(op),
        E.chain(rator => pipe(
            params,
            E.traverseArray(parseL5CExp),
            E.map(rands => makeAppExp(rator, rands))
        ))
    );

const parseIfExp = (params: readonly Sexp[]): E.Either<string, IfExp> =>
    params.length !== 3 ? E.left("Expression not of the form (if <cexp> <cexp> <cexp>)") :
    pipe(params, E.traverseArray(parseL5CExp), E.map(([test, then, alt]) => makeIfExp(test, then, alt)));

// (lambda (<vardecl>*) [: returnTE]? <CExp>+)
const parseProcExp = (vars: Sexp, rest: readonly Sexp[]): E.Either<string, ProcExp> => {
    if (isArray(vars)) {
        const args = pipe(vars, E.traverseArray(parseVarDecl));
        const body = pipe(rest[0] === ":" ? rest.slice(2) : rest, E.traverseArray(parseL5CExp));
        const returnTE = rest[0] === ":" ? parseTExp(rest[1]) : E.of(makeFreshTVar());
        return pipe(
            args,
            E.chain(args => pipe(
                body,
                E.chain(body => pipe(
                    returnTE,
                    E.map(returnTE => makeProcExp(args, body, returnTE))
                ))
            ))
        );
    } else {
        return E.left(`Invalid args ${JSON.stringify(vars)}`)
    }
}

const isGoodBindings = (bindings: Sexp): bindings is [Sexp, Sexp][] =>
    isArray(bindings) && allT(isArray, bindings);

const parseLetExp = (bindings: Sexp, body: readonly Sexp[]): E.Either<string, LetExp> =>
    isEmpty(body) ? E.left('Body of "let" cannot be empty') :
    ! isGoodBindings(bindings) ? E.left(`Invalid bindings: ${JSON.stringify(bindings)}`) :
    pipe(
        parseBindings(bindings),
        E.chain(bdgs => pipe(
            body,
            E.traverseArray(parseL5CExp),
            E.map(body => makeLetExp(bdgs, body))
        ))
    );

const isConcreteVarDecl = (sexp: Sexp): boolean =>
    isIdentifier(sexp) ||
    (isArray(sexp) && sexp.length > 2 && isIdentifier(sexp[0]) && (sexp[1] === ':'));

export const parseVarDecl = (sexp: Sexp): E.Either<string, VarDecl> => {
    if (isString(sexp)) {
        return E.of(makeVarDecl(sexp, makeFreshTVar()));
    } else if (isArray(sexp)) {
        const v = sexp[0];
        if (isString(v)) {
            return pipe(parseTExp(sexp[2]), E.map(te => makeVarDecl(v, te)));
        } else {
            return E.left(`Invalid var ${sexp[0]}`);
        }
    } else {
        return E.left("Var cannot be a SexpString");
    }
}

const parseBindings = (bindings: [Sexp, Sexp][]): E.Either<string, readonly Binding[]> =>
    pipe(
        bindings,
        map(b => b[0]),
        E.traverseArray(parseVarDecl),
        E.chain(vds => pipe(
            bindings,
            map(b => b[1]),
            E.traverseArray(parseL5CExp),
            E.map(vals => zipWith(vds, vals, makeBinding))
        ))
    );

const parseLetrecExp = (bindings: Sexp, body: readonly Sexp[]): E.Either<string, LetrecExp> =>
    isEmpty(body) ? E.left('Body of "letrec" cannot be empty') :
    ! isGoodBindings(bindings) ? E.left(`Invalid bindings: ${JSON.stringify(bindings)}`) :
    pipe(
        parseBindings(bindings),
        E.chain(bdgs => pipe(
            body,
            E.traverseArray(parseL5CExp),
            E.map(body => makeLetrecExp(bdgs, body))
        ))
    );

const parseSetExp = (params: readonly Sexp[]): E.Either<string, SetExp> =>
    isEmpty(params) ? E.left("set! missing 2 arguments") :
    isEmpty(rest(params)) ? E.left("set! missing 1 argument") :
    ! isEmpty(rest(rest(params))) ? E.left("set! has too many arguments") :
    parseGoodSetExp(first(params), second(params));
    
const parseGoodSetExp = (variable: Sexp, val: Sexp): E.Either<string, SetExp> =>
    ! isIdentifier(variable) ? E.left("First arg of set! must be an identifier") :
    pipe(
        parseL5CExp(val),
        E.map(val => makeSetExp(makeVarRef(variable), val))
    );

// sexps has the shape (quote <sexp>)
export const parseLitExp = (param: Sexp): E.Either<string, LitExp> =>
    pipe(parseSExp(param), E.map(makeLitExp));

export const isDottedPair = (sexps: readonly Sexp[]): boolean =>
    sexps.length === 3 && 
    sexps[1] === "."

export const makeDottedPair = (sexps : readonly Sexp[]): E.Either<string, SExpValue> =>
    pipe(
        parseSExp(sexps[0]),
        E.chain(val1 => pipe(
            parseSExp(sexps[2]),
            E.map(val2 => makeCompoundSExp(val1, val2))
        ))
    );

// sexp is the output of p (sexp parser)
export const parseSExp = (sexp: Sexp): E.Either<string, SExpValue> =>
    sexp === "#t" ? E.of(true) :
    sexp === "#f" ? E.of(false) :
    isString(sexp) && isNumericString(sexp) ? E.of(+sexp) :
    isSexpString(sexp) ? E.of(sexp.toString()) :
    isString(sexp) ? E.of(makeSymbolSExp(sexp)) :
    sexp.length === 0 ? E.of(makeEmptySExp()) :
    isArray(sexp) && isDottedPair(sexp) ? makeDottedPair(sexp) :
    isArray(sexp) ? (
        // fail on (x . y z)
        sexp[0] === '.' ? E.left("Bad dotted sexp: " + sexp) : 
        pipe(
            parseSExp(first(sexp)),
            E.chain(val1 => pipe(
                parseSExp(rest(sexp)),
                E.map(val2 => makeCompoundSExp(val1, val2))
            ))
        )) :
    sexp;

// ==========================================================================
// Unparse: Map an AST to a concrete syntax string.

export const unparse = (e: Parsed): E.Either<string, string> =>
    // NumExp | StrExp | BoolExp | PrimOp | VarRef
    isNumExp(e) ? E.of(`${e.val}`) :
    isStrExp(e) ? E.of(`"${e.val}"`) :
    isBoolExp(e) ? E.of(e.val ? "#t" : "#f") :
    isPrimOp(e) ? E.of(e.op) :
    isVarRef(e) ? E.of(e.var) :
    // AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp
    isAppExp(e) ? pipe(
        unparse(e.rator),
        E.chain(rator => pipe(
            e.rands,
            E.traverseArray(unparse),
            E.map(rands => `(${rator} ${rands.join(" ")})`)
        ))
    ) :
    isIfExp(e) ? pipe(
        unparse(e.test),
        E.chain(test => pipe(
            unparse(e.then),
            E.chain(then => pipe(
                unparse(e.alt),
                E.map(alt => `(if ${test} ${then} ${alt})`)
            ))
        ))
    ) :
    isLetExp(e) ? unparseLetExp(e) :
    isLetrecExp(e) ? unparseLetrecExp(e) :
    isProcExp(e) ? unparseProcExp(e) :
    isLitExp(e) ? E.of(unparseLitExp(e)) :
    isSetExp(e) ? unparseSetExp(e) :
    // DefineExp | Program
    isDefineExp(e) ? pipe(
        unparseVarDecl(e.var),
        E.chain(vd => pipe(
            unparse(e.val),
            E.map(val => `(define ${vd} ${val})`)
        ))
    ) :
    isProgram(e) ? pipe(unparseLExps(e.exps), E.map(exps => `(L5 ${exps})`)) :
    e;

const unparseReturn = (te: TExp): E.Either<string, string> =>
    isTVar(te) ? E.of("") :
    pipe(unparseTExp(te), E.map(te => ` : ${te}`));

const unparseBindings = (bindings: readonly Binding[]): E.Either<string, string> =>
    pipe(
        bindings,
        E.traverseArray(bdg => pipe(
            unparseVarDecl(bdg.var),
            E.chain(vd => pipe(
                unparse(bdg.val),
                E.map(val => `(${vd} ${val})`)
            ))
        )),
        E.map(strs => strs.join(" "))
    );

const unparseVarDecl = (vd: VarDecl): E.Either<string, string> =>
    isTVar(vd.texp) ? E.of(vd.var) :
    pipe(unparseTExp(vd.texp), E.map(te => `(${vd.var} : ${te})`));

// Add a quote for symbols, empty and compound sexp - strings and numbers are not quoted.
const unparseLitExp = (le: LitExp): string =>
    isEmptySExp(le.val) ? `'()` :
    isSymbolSExp(le.val) ? `'${valueToString(le.val)}` :
    isCompoundSExp(le.val) ? `'${valueToString(le.val)}` :
    `${le.val}`;

const unparseLExps = (les: readonly Exp[]): E.Either<string, string> =>
    pipe(
        les,
        E.traverseArray(unparse),
        E.map(les => les.join(" "))
    );

const unparseProcExp = (pe: ProcExp): E.Either<string, string> =>
    pipe(
        pe.args,
        E.traverseArray(unparseVarDecl),
        E.chain(vds => pipe(
            unparseReturn(pe.returnTE),
            E.chain(ret => pipe(
                unparseLExps(pe.body),
                E.map(body => `(lambda (${vds.join(" ")})${ret} ${body})`)
            ))
        ))
    );

const unparseLetExp = (le: LetExp) : E.Either<string, string> =>
    pipe(
        unparseBindings(le.bindings),
        E.chain(bdgs => pipe(
            unparseLExps(le.body),
            E.map(body => `(let (${bdgs}) ${body})`)
        ))
    );

const unparseLetrecExp = (le: LetrecExp): E.Either<string, string> =>
    pipe(
        unparseBindings(le.bindings),
        E.chain(bdgs => pipe(
            unparseLExps(le.body),
            E.map(body => `(letrec (${bdgs}) ${body})`)
        ))
    );

const unparseSetExp = (se: SetExp): E.Either<string, string> =>
    pipe(unparse(se.val), E.map(val => `(set! ${se.var.var} ${val})`));
