// List operations similar to car/cdr/cadr in Scheme

import { all } from 'ramda';

export const first = <T>(x: T[]): T => x[0];
export const second = <T>(x: T[]): T => x[1];
export const rest = <T>(x: T[]): T[] => x.slice(1);

// A useful type predicate for homegeneous lists
export const allT = <T>(isT: (x: any) => x is T, x: any[]): x is T[] => all(isT, x);

// ========================================================
// Type utilities

export const isEmpty = (x: any): boolean => x.length === 0;
export const isArray = (x: any): x is any[] => x instanceof Array;
export const isString = (x: any): x is string => typeof x === "string";
export const isNumber = (x: any): x is number => typeof x === "number";
export const isBoolean = (x: any): x is boolean => typeof x === "boolean";

// s-expression returns strings quoted as "a" as [String: 'a'] objects
// to distinguish them from symbols - which are encoded as 'a'
// These are constructed using the new String("a") constructor
// and can be distinguished from regular strings based on the constructor.
export const isSexpString = (x: any): x is String =>
    ! isString(x) && x.constructor && x.constructor.name === "String";
// A weird method to check that a string is a string encoding of a number
export const isNumericString = (x: string): boolean => JSON.stringify(+x) === x;
