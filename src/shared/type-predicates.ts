// ========================================================
// Type utilities

import { SexpString, Token, CompoundSexp, Sexp } from 's-expression';
import { allT } from './list';

export const isArray = Array.isArray;
export const isString = (x: any): x is string => typeof x === "string";
export const isNumber = (x: any): x is number => typeof x === "number";
export const isBoolean = (x: any): x is boolean => typeof x === "boolean";

// s-expression returns strings quoted as "a" as [String: 'a'] objects
// to distinguish them from symbols - which are encoded as 'a'
// These are constructed using the new String("a") constructor
// and can be distinguished from regular strings based on the constructor.
export const isSexpString = (x: any): x is SexpString =>
    ! isString(x) && x.constructor && x.constructor.name === "String";

export const isSexp = (x: any): x is Sexp => isToken(x) || isCompoundSexp(x);
export const isToken = (x: any): x is Token => isString(x) || isSexpString(x);
export const isCompoundSexp = (x: any): x is CompoundSexp => isArray(x) && allT(isSexp, x);

// A weird method to check that a string is a string encoding of a number
export const isNumericString = (x: string): boolean => JSON.stringify(+x) === x;

// A predicate for a valid identifier
export const isVar = (x: any): x is string => isString(x) && !isNumericString(x);
