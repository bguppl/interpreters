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
import { isArray, isEmpty, isNumericString, isSexpString, isString, allT } from '../shared/list';
import { parseLitExp } from './L3-ast';
import { isError, safeFL } from '../shared/error';
import { first, rest} from '../shared/list';

export type CExpLA = NumExp | BoolExp | StrExp | LitExp | VarRef | LexAddress | ProcExpLA | IfExpLA | AppExpLA;
export const isCExpLA = (x: any): x is CExpLA =>
    isNumExp(x) || isBoolExp(x) || isStrExp(x) || isLitExp(x) || isVarRef(x) ||
    isLexAddress(x) || isProcExpLA(x) || isIfExpLA(x) || isAppExpLA(x);

export interface ProcExpLA {
    tag: "ProcExpLA";
    params: VarDecl[];
    body: CExpLA[];
};
export const isProcExpLA = (x: any): x is ProcExpLA =>
    (typeof(x) === 'object') && (x.tag === 'ProcExpLA');
export const makeProcExpLA = (params: VarDecl[], body: CExpLA[]): ProcExpLA =>
    ({tag: "ProcExpLA", params: params, body: body});

export interface IfExpLA {
    tag: "IfExpLA";
    test: CExpLA;
    then: CExpLA;
    alt: CExpLA;
};
export const isIfExpLA = (x: any): x is IfExpLA =>
    (typeof(x) === 'object') && (x.tag === 'IfExpLA');
export const makeIfExpLA = (test: CExpLA, then: CExpLA, alt: CExpLA): IfExpLA =>
    ({tag: "IfExpLA", test: test, then: then, alt: alt});

export interface AppExpLA {
    tag: "AppExpLA";
    rator: CExpLA;
    rands: ReadonlyArray<CExpLA>;
};
export const isAppExpLA = (x: any): x is AppExpLA =>
    (typeof(x) === 'object') && (x.tag === 'AppExpLA');
export const makeAppExpLA = (rator: CExpLA, rands: ReadonlyArray<CExpLA>): AppExpLA =>
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
};
export const isFreeVar = (x: any): x is FreeVar => (typeof(x) === 'object') && (x.tag === "FreeVar");
export const makeFreeVar = (v: string): FreeVar => ({tag: "FreeVar", var: v});

export interface LexicalAddress {
    tag: "LexicalAddress";
    var: string;
    depth: number;
    pos: number;
};
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
import parseSexp, { StringTree } from "s-expression";

export const parseLA = (x: string): CExpLA | Error =>
    parseLASExp(parseSexp(x));

export const parseLASExp = (sexp: StringTree): CExpLA | Error =>
    isEmpty(sexp) ? Error("Parse: Unexpected empty") :
    isArray(sexp) ? parseLACompound(sexp) :
    isString(sexp) ? parseLAAtomic(sexp) :
    isSexpString(sexp) ? parseLAAtomic(sexp) :
    Error(`Parse: Unexpected type ${sexp}`);

const parseLAAtomic = (sexp: string | String): CExpLA =>
    sexp === "#t" ? makeBoolExp(true) :
    sexp === "#f" ? makeBoolExp(false) :
    isString(sexp) && isNumericString(sexp) ? makeNumExp(+sexp) :
    isSexpString(sexp) ? makeStrExp(sexp.toString()) :
    makeVarRef(sexp);

const parseLACompound = (sexps: StringTree[]): CExpLA | Error =>
    first(sexps) === "if" ? parseIfExpLA(sexps) :
    first(sexps) === "lambda" ? parseProcExpLA(sexps) :
    first(sexps) === "quote" ? parseLitExp(sexps) :
    parseAppExpLA(sexps);

const parseAppExpLA = (sexps: StringTree[]): AppExpLA | Error =>
    safeFL((cexps: CExpLA[]) => makeAppExpLA(first(cexps), rest(cexps)))(map(parseLASExp, sexps));

const parseIfExpLA = (sexps: StringTree[]): IfExpLA | Error =>
    safeFL((cexps: CExpLA[]) => makeIfExpLA(cexps[0], cexps[1], cexps[2]))(map(parseLASExp, rest(sexps)));

const parseProcExpLA = (sexps: StringTree[]): ProcExpLA | Error => {
    const vars = sexps[1];
    if (isArray(vars) && allT(isString, vars)) {
        return safeFL((body: CExpLA[]) => makeProcExpLA(map(makeVarDecl, vars), body))(map(parseLASExp, rest(rest(sexps))));
    } else {
        return Error("Invalid vars for ProcExp");
    }
}

// ========================================================
// Unparse

import { isCompoundSExp, isEmptySExp, isSymbolSExp, valueToString } from './L3-value';

const unparseLitExp = (le: LitExp): any =>
    isEmptySExp(le.val) ? ["quote", valueToString(le.val)] :
    isSymbolSExp(le.val) ? ["quote", valueToString(le.val)] :
    isCompoundSExp(le.val) ? ["quote", valueToString(le.val)] :
    le.val;

export const unparseLA = (exp: CExpLA | Error): any =>
    isError(exp) ? exp :
    isBoolExp(exp) ? exp.val :
    isNumExp(exp) ? exp.val :
    isStrExp(exp) ? exp.val :
    isLitExp(exp) ? unparseLitExp(exp) :
    isVarRef(exp) ? exp.var :
    isProcExpLA(exp) ? ["lambda", map((p) => p.var, exp.params)].concat(map(unparseLA, exp.body)) :
    isIfExpLA(exp) ? ["if", unparseLA(exp.test), unparseLA(exp.then), unparseLA(exp.alt)] :
    isAppExpLA(exp) ? [unparseLA(exp.rator)].concat(map(unparseLA, exp.rands)) :
    isFreeVar(exp) ? [exp.var, "free"] :
    isLexicalAddress(exp) ? [exp.var, ":", exp.depth, exp.pos] :
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
export const addLexicalAddresses = (exp: CExpLA | Error): CExpLA | Error => {
    const visitProc = (proc: ProcExpLA, addresses: LexicalAddress[]): ProcExpLA | Error => {
        let newAddresses = crossContour(proc.params, addresses);
        return safeFL((bs: CExpLA[]) => makeProcExpLA(proc.params, bs))(map((b) => visit(b, newAddresses), proc.body));
    };
    const visit = (exp: CExpLA | Error, addresses: LexicalAddress[]): CExpLA | Error =>
        isBoolExp(exp) ? exp :
        isNumExp(exp) ? exp :
        isStrExp(exp) ? exp :
        isVarRef(exp) ? getLexicalAddress(exp, addresses) :
        isFreeVar(exp) ? Error("unexpected LA ${exp}") :
        isLexicalAddress(exp) ? Error("unexpected LA ${exp}") :
        isLitExp(exp) ? exp :
        isIfExpLA(exp) ? safeFL((tta: CExpLA[]) =>
                                makeIfExpLA(tta[0], tta[1], tta[2]))([
                                    visit(exp.test, addresses),
                                    visit(exp.then, addresses),
                                    visit(exp.alt, addresses)]) :
        isProcExpLA(exp) ? visitProc(exp, addresses) :
        isAppExpLA(exp) ? safeFL((app: CExpLA[]) => makeAppExpLA(first(app), rest(app)))
                            ([visit(exp.rator, addresses)].concat(
                                map((r) => visit(r, addresses), exp.rands))) :
        exp;
    return isError(exp) ? exp : visit(exp, []);
};
