// =============================================================================
// lexical-address

// Extend exp AST to distinguish variable declarations and variable references.
// Variable references are replaced by lexical addresses which indicate unambiguously
// to which declaration each variable reference is bound.

// We skip define and programs for simplicity
// We skip let-exp because they can be rewritten to lambda-app.
// We consider primOp as VarRef to simplify the parser.
/*
<cexpLA> ::= <number>                           / NumExp(val:number)
        |  <boolean>                            / BoolExp(val:boolean)
        |  <string>                             / StrExp(val:string)
        |  <var-ref>                            / VarRef(var:symbol)
        |  ( lambda ( <var-decl>* ) <cexpLA>+ ) / ProcExpLA(params:List(var-decl), body:List(cexp))
        |  ( if <cexpLA> <cexpLA> <cexpLA> )    / IfExpLA(test: cexpLA, then: cexpLA, else: cexpLA)
        |  ( <cexpLA> <cexpLA>* )               / AppExpLA(rator:cexpLA, rands:List(cexpLA))
        |  ( quote <sexp> )                     / LitExp(val:sexp)
*/
import { concat, map } from 'ramda';
import { BoolExp, LitExp, NumExp, StrExp, VarDecl, VarRef } from './L3-ast';
import { isBoolExp, isLitExp, isNumExp, isStrExp, isVarRef } from './L3-ast';
import { makeBoolExp, makeNumExp, makeStrExp, makeVarDecl, makeVarRef } from './L3-ast';
import { first, rest, isEmpty, allT, second, cons } from '../shared/list';
import { isArray, isNumericString, isString } from "../shared/type-predicates";
import { parseLitExp } from './L3-ast';
import { Result, makeFailure, makeOk, bind, mapResult, safe3, safe2 } from '../shared/result';
import { isToken } from "../shared/parser";

export type CExpLA = NumExp | BoolExp | StrExp | LitExp | VarRef | LexAddress | ProcExpLA | IfExpLA | AppExpLA;
export const isCExpLA = (x: any): x is CExpLA =>
    isNumExp(x) || isBoolExp(x) || isStrExp(x) || isLitExp(x) || isVarRef(x) ||
    isLexAddress(x) || isProcExpLA(x) || isIfExpLA(x) || isAppExpLA(x);

export interface ProcExpLA {
    tag: "ProcExpLA";
    params: VarDecl[];
    body: CExpLA[];
}
export const isProcExpLA = (x: any): x is ProcExpLA =>
    (typeof(x) === 'object') && (x.tag === 'ProcExpLA');
export const makeProcExpLA = (params: VarDecl[], body: CExpLA[]): ProcExpLA =>
    ({tag: "ProcExpLA", params: params, body: body});

export interface IfExpLA {
    tag: "IfExpLA";
    test: CExpLA;
    then: CExpLA;
    alt: CExpLA;
}
export const isIfExpLA = (x: any): x is IfExpLA =>
    (typeof(x) === 'object') && (x.tag === 'IfExpLA');
export const makeIfExpLA = (test: CExpLA, then: CExpLA, alt: CExpLA): IfExpLA =>
    ({tag: "IfExpLA", test: test, then: then, alt: alt});

export interface AppExpLA {
    tag: "AppExpLA";
    rator: CExpLA;
    rands: CExpLA[];
}
export const isAppExpLA = (x: any): x is AppExpLA =>
    (typeof(x) === 'object') && (x.tag === 'AppExpLA');
export const makeAppExpLA = (rator: CExpLA, rands: CExpLA[]): AppExpLA =>
    ({tag: "AppExpLA", rator: rator, rands: rands});

/*
AST extension for lexical-address annotations
<address> ::= <free-var> | <lexical-address>
<free-var> ::= [<identifier> free]                       / free-var(var)
<lexical-address> ::= [<identifier> : <number> <number>] / lexical-address(var:Symbol, depth:Number, pos:Number]
*/
export type LexAddress = FreeVar | LexicalAddress;
export const isLexAddress = (x: any): x is LexAddress => isFreeVar(x) || isLexicalAddress(x);

export interface FreeVar {
    tag: "FreeVar";
    var: string;
}
export const isFreeVar = (x: any): x is FreeVar => (typeof(x) === 'object') && (x.tag === "FreeVar");
export const makeFreeVar = (v: string): FreeVar => ({tag: "FreeVar", var: v});

export interface LexicalAddress {
    tag: "LexicalAddress";
    var: string;
    depth: number;
    pos: number;
}
export const isLexicalAddress = (x: any): x is LexicalAddress =>
    (typeof(x) === "object") && (x.tag === "LexicalAddress");
export const makeLexicalAddress = (v: string, depth: number, pos: number): LexicalAddress =>
    ({tag: "LexicalAddress", var: v, depth: depth, pos: pos});
export const makeDeeperLexicalAddress = (la: LexicalAddress): LexicalAddress =>
    makeLexicalAddress(la.var, la.depth + 1, la.pos);

/*
Purpose: parse a sexp into a ExpLA AST value.
Type: [Sexp -> ExpLA]
Signature: parseLA(sexp)
Examples:
parseLA("1") -> '(num-exp 1)
parseLA("(if #t (+ 1 2) 'ok)") -> '(IfExpLA (BoolExp true) (AppExpLA (VarRef +) ((num-exp 1) (num-exp 2))) (literal-exp ok))
*/
import { Sexp, Token } from "s-expression";
import { parse as p } from "../shared/parser";

export const parseLA = (x: string): Result<CExpLA> =>
    bind(p(x), parseLASExp);

export const parseLASExp = (sexp: Sexp): Result<CExpLA> =>
    isEmpty(sexp) ? makeFailure("Parse: Unexpected empty") :
    isArray(sexp) ? parseLACompound(sexp) :
    isToken(sexp) ? makeOk(parseLAAtomic(sexp)) :
    sexp;

const parseLAAtomic = (sexp: Token): CExpLA =>
    sexp === "#t" ? makeBoolExp(true) :
    sexp === "#f" ? makeBoolExp(false) :
    isString(sexp) && isNumericString(sexp) ? makeNumExp(+sexp) :
    isString(sexp) ? makeVarRef(sexp) :
    makeStrExp(sexp.toString());

const parseLACompound = (sexps: Sexp[]): Result<CExpLA> =>
    first(sexps) === "if" ? parseIfExpLA(sexps) :
    first(sexps) === "lambda" ? parseProcExpLA(sexps) :
    first(sexps) === "quote" ? parseLitExp(second(sexps)) :
    parseAppExpLA(sexps);

const parseAppExpLA = (sexps: Sexp[]): Result<AppExpLA> =>
    bind(mapResult(parseLASExp, sexps),
         (cexps: CExpLA[]) => makeOk(makeAppExpLA(first(cexps), rest(cexps))));

const parseIfExpLA = (sexps: Sexp[]): Result<IfExpLA> =>
    bind(mapResult(parseLASExp, rest(sexps)),
        (cexps: CExpLA[]) => makeOk(makeIfExpLA(cexps[0], cexps[1], cexps[2])));

const parseProcExpLA = (sexps: Sexp[]): Result<ProcExpLA> => {
    const vars = sexps[1];
    if (isArray(vars) && allT(isString, vars)) {
        return bind(mapResult(parseLASExp, rest(rest(sexps))),
                    (body: CExpLA[]) => makeOk(makeProcExpLA(map(makeVarDecl, vars), body)));
    } else {
        return makeFailure("Invalid vars for ProcExp");
    }
}

// ========================================================
// Unparse

import { isCompoundSExp, isEmptySExp, isSymbolSExp, valueToString } from './L3-value';

const unparseLitExp = (le: LitExp): Sexp =>
    isEmptySExp(le.val) ? ["quote", valueToString(le.val)] :
    isSymbolSExp(le.val) ? ["quote", valueToString(le.val)] :
    isCompoundSExp(le.val) ? ["quote", valueToString(le.val)] :
    valueToString(le.val);

export const unparseLA = (exp: CExpLA): Sexp =>
    isBoolExp(exp) ? valueToString(exp.val) :
    isNumExp(exp) ? valueToString(exp.val) :
    isStrExp(exp) ? exp.val :
    isLitExp(exp) ? unparseLitExp(exp) :
    isVarRef(exp) ? exp.var :
    isProcExpLA(exp) ? cons("lambda", cons(map((p) => p.var, exp.params), map(unparseLA, exp.body))) :
    isIfExpLA(exp) ? ["if", unparseLA(exp.test), unparseLA(exp.then), unparseLA(exp.alt)] :
    isAppExpLA(exp) ? cons(unparseLA(exp.rator), map(unparseLA, exp.rands)) :
    isFreeVar(exp) ? [exp.var, "free"] :
    isLexicalAddress(exp) ? [exp.var, ":", `${exp.depth}`, `${exp.pos}`] :
    exp;

/*
Annotate an exp AST so that all variable references are marked with their lexical address.
The lexical address links a variable reference to its corresponding variable declaration.
It can be of two forms:
- If the variable is free - it is noted [var free]
- else [var : depth var-index]
  where depth is the 0-based distance ot the enclosing lambda declaration
        var-index is the 0-based index of the variable in the lambda declaration
Example:
unparseLA(addLexicalAddresses(parseLA(`
   (lambda (a b c)
     (if (eq? b c)
         ((lambda (c)
            (cons a c))
          a)
         b))`)))
=>
(lambda (a b c)
 (if ([eq? free] [b : 0 1] [c : 0 2])
   ((lambda (c) ([cons free] [a : 1 0] [c : 0 0]))
    [a : 0 0])
   [b : 0 1]))
*/

/*
Purpose: get the closest enclosing lexical address given a variable name.
Signature: getLexicalAddress(var, lexicalAddresses)
Pre-conditions: Lexical-addresses are sorted by depth
Examples:
getLexicalAddress((var-ref b), [[lex-addr a 0 0], [lex-addr b 0 1]])
=> [LexAddr b 0 1]
getLexicalAddress((var-ref c), [[lex-addr a 0 0], [lex-addr b 0 1]])
=> [FreeVar c]
getLexicalAddress((var-ref a), [[lex-addr a 0 0], [lex-addr b 0 1], [lex-add a 1 1]])
=> [LexAddr a 0 0]
*/
export const getLexicalAddress = (v: VarRef, lexAddresses: LexicalAddress[]): LexAddress => {
    const loop = (addresses: LexicalAddress[]): LexAddress =>
        isEmpty(addresses) ? makeFreeVar(v.var) :
        v.var === first(addresses).var ? first(addresses) :
        loop(rest(addresses));
    return loop(lexAddresses);
}

/*
Purpose: get the pos of a variable in a declaration list (parameters from a lambda-exp)
Signature: indexOfVar(var, parameters)
Type: [VarDecl * VarDecl[]) => number ]
Examples:
indexOfVar((VarDecl b), [[VarDecl a], [VarDecl b]]) => 1
indexOfVar((VarDecl c), [[VarDecl a], [VarDecl b]]) => -1
*/
export const indexOfVar = (v: VarDecl, decls: VarDecl[]): number => {
    const loop = (decls: VarDecl[], index: number): number =>
        isEmpty(decls) ? -1 :
        first(decls).var === v.var ? index :
        loop(rest(decls), index + 1);
    return loop(decls, 0);
}

/*
Purpose: create a new view of the accessible variables when a declaration
         contour is crossed - that is, when we enter a (lambda (declarations) ...)
         variables in declarations are now visible at depth 0
         variables previously visible are now a depth + 1
         the new variables appear first in the new addresses
Signature: crossContour(decls, addresses)
Type: [VarDecl[] * LexicalAddress[]) => LexicalAddress[]
Example:
crossContour([[VarDecl a], [VarDecl b]], [[LexAddr a 0 0], [LexAddr c 0 1]]) =>
[[LexAddr a 0 0], [LexAddr b 0 1], [LexAddr a 1 0], [LexAddr c 1 1]]
This corresponds to the visible variables from the body of the inner lambda in:
'(lambda (a c) (lambda (a b) <here>))
*/
export const crossContour = (decls: VarDecl[], addresses: LexicalAddress[]): LexicalAddress[] =>
    concat(makeBoundAddresses(decls), map(makeDeeperLexicalAddress, addresses));
/*
Signature: makeBoundAddresses(decls)
Type: VarDecl[] => LexicalAddress[]
Example:
makeBoundAddresses([[VarDecl a], [VarDecl b]]) => [[LexAddr a 0 0], [lexAddr c 0 1]]
*/
const makeBoundAddresses = (decls: VarDecl[]): LexicalAddress[] =>
    map((decl) => makeLexicalAddress(decl.var, 0, indexOfVar(decl, decls)),
        decls);

/*
Purpose: Main function - map all variable reference expressions to their lexical address inside exp.
Signature: addLexicalAddresses(exp)
Type: [ExpLA -> ExpLA]
Example:
unparseLA(addLexicalAddresses(parseLA(`
    (lambda (a b c)
      (if (eq? b c)
          ((lambda (c)
             (cons a c))
           a)
          b))`)))
=>
(lambda (a b c)
  (if ((eq? free) (b : 0 1) (c : 0 2))
*/
export const addLexicalAddresses = (exp: CExpLA): Result<CExpLA> => {
    const visitProc = (proc: ProcExpLA, addresses: LexicalAddress[]): Result<ProcExpLA> => {
        const newAddresses = crossContour(proc.params, addresses);
        return bind(mapResult(b => visit(b, newAddresses), proc.body),
                    (bs: CExpLA[]) => makeOk(makeProcExpLA(proc.params, bs)));
    };
    const visit = (exp: CExpLA, addresses: LexicalAddress[]): Result<CExpLA> =>
        isBoolExp(exp) ? makeOk(exp) :
        isNumExp(exp) ? makeOk(exp) :
        isStrExp(exp) ? makeOk(exp) :
        isVarRef(exp) ? makeOk(getLexicalAddress(exp, addresses)) :
        isFreeVar(exp) ? makeFailure(`unexpected LA ${exp}`) :
        isLexicalAddress(exp) ? makeFailure(`unexpected LA ${exp}`) :
        isLitExp(exp) ? makeOk(exp) :
        isIfExpLA(exp) ? safe3((test: CExpLA, then: CExpLA, alt: CExpLA) => makeOk(makeIfExpLA(test, then, alt)))
                            (visit(exp.test, addresses), visit(exp.then, addresses), visit(exp.alt, addresses)) :
        isProcExpLA(exp) ? visitProc(exp, addresses) :
        isAppExpLA(exp) ? safe2((rator: CExpLA, rands: CExpLA[]) => makeOk(makeAppExpLA(rator, rands)))
                            (visit(exp.rator, addresses), mapResult(rand => visit(rand, addresses), exp.rands)) :
        exp;
    return visit(exp, []);
};
