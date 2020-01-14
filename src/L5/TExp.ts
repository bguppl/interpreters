/*
;; TExp AST
;; ========
;; Type checking language
;; Syntax with optional type annotations for var declarations and function return types.

;; Type language
;; <texp>         ::= <atomic-te> | <compound-te> | <tvar>
;; <atomic-te>    ::= <num-te> | <bool-te> | <void-te>
;; <num-te>       ::= number   // num-te()
;; <bool-te>      ::= boolean  // bool-te()
;; <str-te>       ::= string   // str-te()
;; <void-te>      ::= void     // void-te()
;; <compound-te>  ::= <proc-te> | <tuple-te>
;; <non-tuple-te> ::= <atomic-te> | <proc-te> | <tvar>
;; <proc-te>      ::= [ <tuple-te> -> <non-tuple-te> ] // proc-te(param-tes: list(te), return-te: te)
;; <tuple-te>     ::= <non-empty-tuple-te> | <empty-te>
;; <non-empty-tuple-te> ::= ( <non-tuple-te> *)* <non-tuple-te> // tuple-te(tes: list(te))
;; <empty-te>     ::= Empty
;; <tvar>         ::= a symbol starting with T // tvar(id: Symbol, contents; Box(string|boolean))

;; Examples of type expressions
;; number
;; boolean
;; void
;; [number -> boolean]
;; [number * number -> boolean]
;; [number -> [number -> boolean]]
;; [Empty -> number]
;; [Empty -> void]
*/
import { chain, concat, filter, map, uniq } from "ramda";
// @ts-ignore
import p = require("s-expression");
import { isArray, isBoolean, isEmpty, isString } from './L5-ast';
import { makeBox, setBox, unbox, Box } from '../shared/box';
import { getErrorMessages, hasNoError, isError, safeF, safeFL } from '../shared/error';
import { first, rest } from '../shared/list';

export type TExp =  AtomicTExp | CompoundTExp | TVar;
export const isTExp = (x: any): x is TExp => isAtomicTExp(x) || isCompoundTExp(x) || isTVar(x);

export type AtomicTExp = NumTExp | BoolTExp | StrTExp | VoidTExp;
export const isAtomicTExp = (x: any): x is AtomicTExp =>
    isNumTExp(x) || isBoolTExp(x) || isStrTExp(x) || isVoidTExp(x);

export type CompoundTExp = ProcTExp | TupleTExp;
export const isCompoundTExp = (x: any): x is CompoundTExp => isProcTExp(x) || isTupleTExp(x);

export type NonTupleTExp = AtomicTExp | ProcTExp | TVar;
export const isNonTupleTExp = (x: any): x is NonTupleTExp =>
    isAtomicTExp(x) || isProcTExp(x) || isTVar(x);

export type NumTExp = { tag: "NumTExp" };
export const makeNumTExp = (): NumTExp => ({tag: "NumTExp"});
export const isNumTExp = (x: any): x is NumTExp => x.tag === "NumTExp";

export type BoolTExp = { tag: "BoolTExp" };
export const makeBoolTExp = (): BoolTExp => ({tag: "BoolTExp"});
export const isBoolTExp = (x: any): x is BoolTExp => x.tag === "BoolTExp";

export type StrTExp = { tag: "StrTExp" };
export const makeStrTExp = (): StrTExp => ({tag: "StrTExp"});
export const isStrTExp = (x: any): x is StrTExp => x.tag === "StrTExp";

export type VoidTExp = { tag: "VoidTExp" };
export const makeVoidTExp = (): VoidTExp => ({tag: "VoidTExp"});
export const isVoidTExp = (x: any): x is VoidTExp => x.tag === "VoidTExp";

// proc-te(param-tes: list(te), return-te: te)
export type ProcTExp = { tag: "ProcTExp"; paramTEs: TExp[]; returnTE: TExp; };
export const makeProcTExp = (paramTEs: TExp[], returnTE: TExp): ProcTExp =>
    ({tag: "ProcTExp", paramTEs: paramTEs, returnTE: returnTE});
export const isProcTExp = (x: any): x is ProcTExp => x.tag === "ProcTExp";
// Uniform access to all components of a ProcTExp
export const procTExpComponents = (pt: ProcTExp): TExp[] =>
    [...pt.paramTEs, pt.returnTE];

export type TupleTExp = NonEmptyTupleTExp | EmptyTupleTExp;
export const isTupleTExp = (x: any): x is TupleTExp =>
    isNonEmptyTupleTExp(x) || isEmptyTupleTExp(x);

export interface EmptyTupleTExp { tag: "EmptyTupleTExp" };
export const makeEmptyTupleTExp = (): EmptyTupleTExp => ({tag: "EmptyTupleTExp"});
export const isEmptyTupleTExp = (x: any): x is EmptyTupleTExp => x.tag === "EmptyTupleTExp";

// NonEmptyTupleTExp(TEs: NonTupleTExp[])
export interface NonEmptyTupleTExp { tag: "NonEmptyTupleTExp"; TEs: NonTupleTExp[]; };
export const makeNonEmptyTupleTExp = (tes: NonTupleTExp[]): NonEmptyTupleTExp =>
    ({tag: "NonEmptyTupleTExp", TEs: tes});
export const isNonEmptyTupleTExp = (x: any): x is NonEmptyTupleTExp => x.tag === "NonEmptyTupleTExp";

// TVar: Type Variable with support for dereferencing (TVar -> TVar)
export type TVar = { tag: "TVar"; var: string; contents: Box<undefined | TExp>; };
export const isEmptyTVar = (x: any): x is TVar =>
    (x.tag === "TVar") && unbox(x.contents) === undefined;
export const makeTVar = (v: string): TVar =>
    ({tag: "TVar", var: v, contents: makeBox(undefined)});
const makeTVarGen = (): () => TVar => {
    let count: number = 0;
    return () => {
        count++;
        return makeTVar(`T_${count}`);
    }
}
export const makeFreshTVar = makeTVarGen();
export const isTVar = (x: any): x is TVar => x.tag === "TVar";
export const eqTVar = (tv1: TVar, tv2: TVar): boolean => tv1.var === tv2.var;
export const tvarContents = (tv: TVar): undefined | TExp => unbox(tv.contents);
export const tvarSetContents = (tv: TVar, val: TExp): void =>
    setBox(tv.contents, val);
export const tvarIsNonEmpty = (tv: TVar): boolean => tvarContents(tv) !== undefined;
export const tvarDeref = (te: TExp): TExp => {
    if (! isTVar(te)) return te;
    const contents = tvarContents(te);
    if (contents === undefined)
        return te;
    else if (isTVar(contents))
        return tvarDeref(contents);
    else
        return contents;
}

// ========================================================
// TExp Utilities

// Purpose: uniform access to atomic types
export const atomicTExpName = (te: AtomicTExp): string => te.tag;

export const eqAtomicTExp = (te1: AtomicTExp, te2: AtomicTExp): boolean =>
    atomicTExpName(te1) === atomicTExpName(te2);


// ========================================================
// TExp parser

export const parseTE = (t: string): TExp | Error =>
    parseTExp(p(t));

/*
;; Purpose: Parse a type expression
;; Type: [SExp -> TEx[]]
;; Example:
;; parseTExp("number") => 'num-te
;; parseTExp('boolean') => 'bool-te
;; parseTExp('T1') => '(tvar T1)
;; parseTExp('(T * T -> boolean)') => '(proc-te ((tvar T) (tvar T)) bool-te)
;; parseTExp('(number -> (number -> number)') => '(proc-te (num-te) (proc-te (num-te) num-te))
*/
export const parseTExp = (texp: any): TExp | Error =>
    (texp === "number") ? makeNumTExp() :
    (texp === "boolean") ? makeBoolTExp() :
    (texp === "void") ? makeVoidTExp() :
    (texp === "string") ? makeStrTExp() :
    isString(texp) ? makeTVar(texp) :
    isArray(texp) ? parseCompoundTExp(texp) :
    Error(`Unexpected TExp - ${texp}`);

/*
;; expected structure: (<params> -> <returnte>)
;; expected exactly one -> in the list
;; We do not accept (a -> b -> c) - must parenthesize
*/
const parseCompoundTExp = (texps: any[]): ProcTExp | Error => {
    const pos = texps.indexOf('->');
    return (pos === -1)  ? Error(`Procedure type expression without -> - ${texps}`) :
    (pos === 0) ? Error(`No param types in proc texp - ${texps}`) :
    (pos === texps.length - 1) ? Error(`No return type in proc texp - ${texps}`) :
    (texps.slice(pos + 1).indexOf('->') > -1) ? Error(`Only one -> allowed in a procexp - ${texps}`) :
    safeMakeProcTExp(parseTupleTExp(texps.slice(0, pos)),
                     parseTExp(texps[pos + 1]));
};

const safeMakeProcTExp = (args: Array<TExp | Error>, returnTE: Error | TExp): Error | ProcTExp =>
    isError(returnTE) ? returnTE :
    hasNoError(args) ? makeProcTExp(args, returnTE) :
    Error(getErrorMessages(args));

/*
;; Expected structure: <te1> [* <te2> ... * <ten>]?
;; Or: Empty
*/
const parseTupleTExp = (texps: any[]): Array<TExp | Error> => {
    const isEmptyTuple = (x: any[]): boolean =>
        (x.length === 1) && (x[0] === 'Empty');
    // [x1 * x2 * ... * xn] => [x1,...,xn]
    const splitEvenOdds = (x: any[]): any[] =>
        isEmpty(x) ? [] :
        isEmpty(rest(x)) ? x :
        (x[1] !== '*') ? [Error(`Parameters of procedure type must be separated by '*': ${texps}`)] :
        [x[0], ...splitEvenOdds(x.splice(2))];

    if (isEmptyTuple(texps))
        return [];
    else {
        const argTEs = splitEvenOdds(texps);
        if (hasNoError(argTEs))
            return map(parseTExp, argTEs);
        else
            return filter(isError, argTEs);
    }
}

/*
;; Purpose: Unparse a type expression Texp into its concrete form
*/
export const unparseTExp = (te: TExp | Error): string | Error => {
    const unparseTuple = (paramTes: TExp[]): any =>
        isEmpty(paramTes) ? ["Empty"] :
        [unparseTExp(paramTes[0]), ...chain((te) => ['*', unparseTExp(te)], rest(paramTes))];
    const up = (x: TExp | Error | undefined): string | string[] | Error =>
        isError(x) ? x :
        isNumTExp(x) ? 'number' :
        isBoolTExp(x) ? 'boolean' :
        isStrTExp(x) ? 'string' :
        isVoidTExp(x) ? 'void' :
        isEmptyTVar(x) ? x.var :
        isTVar(x) ? up(tvarContents(x)) :
        isProcTExp(x) ? [...unparseTuple(x.paramTEs), '->', unparseTExp(x.returnTE)] :
        ["never"];
    const unparsed = up(te);
    return isString(unparsed) ? unparsed :
           isError(unparsed) ? unparsed :
           isArray(unparsed) ? `(${unparsed.join(' ')})` :
           `Error ${unparsed}`;
}

// ============================================================
// equivalentTEs: 2 TEs are equivalent up to variable renaming.
// For example:
// equivalentTEs(parseTExp('(T1 -> T2)'), parseTExp('(T3 -> T4)'))


// Signature: matchTVarsInTE(te1, te2, succ, fail)
// Type: [Texp * Texp * [List(Pair(Tvar, Tvar)) -> T1] * [Empty -> T2]] |
//       [List(Texp) * List(Texp) * ...]
// Purpose:   Receives two type expressions or list(texps) plus continuation procedures
//            and, in case they are equivalent, pass a mapping between
//            type variable they include to succ. Otherwise, invoke fail.
// Examples:
// matchTVarsInTE(parseTExp('(Number * T1 -> T1)',
//                parseTExp('(Number * T7 -> T5)'),
//                (x) => x,
//                () => false) ==> [[T1, T7], [T1, T5]]
// matchTVarsInTE(parseTExp('(Boolean * T1 -> T1)'),
//                parseTExp('(Number * T7 -> T5)'),
//                (x) => x,
//                () => false)) ==> false

type Pair<T1, T2> = {left: T1; right: T2};

const matchTVarsInTE = <T1, T2>(te1: TExp, te2: TExp,
                                succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
                                fail: () => T2): T1 | T2 =>
    (isTVar(te1) || isTVar(te2)) ? matchTVarsinTVars(tvarDeref(te1), tvarDeref(te2), succ, fail) :
    (isAtomicTExp(te1) || isAtomicTExp(te2)) ?
        ((isAtomicTExp(te1) && isAtomicTExp(te2) && eqAtomicTExp(te1, te2)) ? succ([]) : fail()) :
    matchTVarsInTProcs(te1, te2, succ, fail);

// te1 and te2 are the result of tvarDeref
const matchTVarsinTVars = <T1, T2>(te1: TExp, te2: TExp,
                                    succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
                                    fail: () => T2): T1 | T2 =>
    (isTVar(te1) && isTVar(te2)) ? (eqTVar(te1, te2) ? succ([]) : succ([{left: te1, right: te2}])) :
    (isTVar(te1) || isTVar(te2)) ? fail() :
    matchTVarsInTE(te1, te2, succ, fail);

const matchTVarsInTProcs = <T1, T2>(te1: TExp, te2: TExp,
        succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
        fail: () => T2): T1 | T2 =>
    (isProcTExp(te1) && isProcTExp(te2)) ? matchTVarsInTEs(procTExpComponents(te1), procTExpComponents(te2), succ, fail) :
    fail();

const matchTVarsInTEs = <T1, T2>(te1: TExp[], te2: TExp[],
                                    succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
                                    fail: () => T2): T1 | T2 =>
    (isEmpty(te1) && isEmpty(te2)) ? succ([]) :
    (isEmpty(te1) || isEmpty(te2)) ? fail() :
    // Match first then continue on rest
    matchTVarsInTE(first(te1), first(te2),
                    (subFirst) => matchTVarsInTEs(rest(te1), rest(te2), (subRest) => succ(concat(subFirst, subRest)), fail),
                    fail);

// Signature: equivalent-tes?(te1, te2)
// Purpose:   Check whether 2 type expressions are equivalent up to
//            type variable renaming.
// Example:  equivalentTEs(parseTExp('(T1 * (Number -> T2) -> T3))',
//                         parseTExp('(T4 * (Number -> T5) -> T6))') => #t
export const equivalentTEs = (te1: TExp, te2: TExp): boolean => {
    // console.log(`EqTEs ${JSON.stringify(te1)} - ${JSON.stringify(te2)}`);
    const tvarsPairs = matchTVarsInTE(te1, te2, (x) => x, () => false);
    // console.log(`EqTEs pairs = ${map(JSON.stringify, tvarsPairs)}`)
    if (isBoolean(tvarsPairs))
        return false;
    else {
        const uniquePairs = uniq(tvarsPairs);
        return (uniq(map((p) => p.left.var, tvarsPairs)).length === uniq(map((p) => p.right.var, tvarsPairs)).length);
    }
};
