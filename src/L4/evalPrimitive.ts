// ========================================================
// Primitives
import * as E from "fp-ts/Either";
import { reduce } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { allT, first, rest } from "../shared/list";
import { isNumber, isBoolean, isString } from "../shared/type-predicates";
import { PrimOp } from "./L4-ast";
import { Value, isSymbolSExp, isEmptySExp, isCompoundSExp, CompoundSExp, makeCompoundSExp, makeEmptySExp, EmptySExp } from "./L4-value";

export const applyPrimitive = (proc: PrimOp, args: readonly Value[]): E.Either<string, Value> =>
    proc.op === "+" ? (allT(isNumber, args) ? pipe(args, reduce(0, (x, y) => x + y), E.of) : E.left("+ expects numbers only")) :
    proc.op === "-" ? minusPrim(args) :
    proc.op === "*" ? (allT(isNumber, args) ? pipe(args, reduce(1, (x, y) => x * y), E.of) : E.left("* expects numbers only")) :
    proc.op === "/" ? divPrim(args) :
    proc.op === ">" ? E.of(args[0] > args[1]) :
    proc.op === "<" ? E.of(args[0] < args[1]) :
    proc.op === "=" ? E.of(args[0] === args[1]) :
    proc.op === "not" ? E.of(! args[0]) :
    proc.op === "and" ? isBoolean(args[0]) && isBoolean(args[1]) ? E.of(args[0] && args[1]) : E.left('Arguments to "and" not booleans') :
    proc.op === "or" ? isBoolean(args[0]) && isBoolean(args[1]) ? E.of(args[0] || args[1]) : E.left('Arguments to "or" not booleans') :
    proc.op === "eq?" ? E.of(eqPrim(args)) :
    proc.op === "string=?" ? E.of(args[0] === args[1]) :
    proc.op === "cons" ? E.of(consPrim(args[0], args[1])) :
    proc.op === "car" ? carPrim(args[0]) :
    proc.op === "cdr" ? cdrPrim(args[0]) :
    proc.op === "list" ? E.of(listPrim(args)) :
    proc.op === "list?" ? E.of(isListPrim(args[0])) :
    proc.op === "pair?" ? E.of(isPairPrim(args[0])) :
    proc.op === "number?" ? E.of(typeof(args[0]) === 'number') :
    proc.op === "boolean?" ? E.of(typeof(args[0]) === 'boolean') :
    proc.op === "symbol?" ? E.of(isSymbolSExp(args[0])) :
    proc.op === "string?" ? E.of(isString(args[0])) :
    E.left("Bad primitive op " + proc.op);

const minusPrim = (args: readonly Value[]): E.Either<string, number> => {
    // TODO complete
    const x = args[0], y = args[1];
    if (isNumber(x) && isNumber(y)) {
        return E.of(x - y);
    } else {
        return E.left(`Type error: - expects numbers ${args}`)
    }
}

const divPrim = (args: readonly Value[]): E.Either<string, number> => {
    // TODO complete
    const x = args[0], y = args[1];
    if (isNumber(x) && isNumber(y)) {
        return E.of(x / y);
    } else {
        return E.left(`Type error: / expects numbers ${args}`)
    }
}

const eqPrim = (args: readonly Value[]): boolean => {
    const x = args[0], y = args[1];
    if (isSymbolSExp(x) && isSymbolSExp(y)) {
        return x.val === y.val;
    } else if (isEmptySExp(x) && isEmptySExp(y)) {
        return true;
    } else if (isNumber(x) && isNumber(y)) {
        return x === y;
    } else if (isString(x) && isString(y)) {
        return x === y;
    } else if (isBoolean(x) && isBoolean(y)) {
        return x === y;
    } else {
        return false;
    }
}

const carPrim = (v: Value): E.Either<string, Value> =>
    isCompoundSExp(v) ? E.of(v.val1) :
    E.left(`Car: param is not compound ${v}`);

const cdrPrim = (v: Value): E.Either<string, Value> =>
    isCompoundSExp(v) ? E.of(v.val2) :
    E.left(`Cdr: param is not compound ${v}`);

const consPrim = (v1: Value, v2: Value): CompoundSExp =>
    makeCompoundSExp(v1, v2);

export const listPrim = (vals: readonly Value[]): EmptySExp | CompoundSExp =>
    vals.length === 0 ? makeEmptySExp() :
    makeCompoundSExp(first(vals), listPrim(rest(vals)))

const isListPrim = (v: Value): boolean =>
    isEmptySExp(v) || isCompoundSExp(v);

const isPairPrim = (v: Value): boolean =>
    isCompoundSExp(v);
