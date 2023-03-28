// ========================================================
// Type utilities
export const isArray = Array.isArray;
export const isString = (x: any): x is string => typeof x === "string";
export const isNumber = (x: any): x is number => typeof x === "number";
export const isBoolean = (x: any): x is boolean => typeof x === "boolean";
export const isError = (x: any): x is Error => x instanceof Error;

// Check that a string encodes a number (also works for -3.0)
// Uses the same conventions as JavaScript - covers octal, hexadecimal, decimal, float
// '0xAB', '0o77' '-1.0e-12' are all valid numbers
export const isNumericString = (x: string): boolean => 
    ((x != null) &&
     (x !== '') &&
     !isNaN(Number(x)));


// A predicate for a valid identifier
export type Identifier = string;
export const isIdentifier = (x: any): x is Identifier =>
    /^[A-Za-z][A-Za-z0-9]*/i.test(x);