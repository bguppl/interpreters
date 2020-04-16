// ========================================================
// Type utilities
export const isArray = Array.isArray;
export const isString = (x: any): x is string => typeof x === "string";
export const isNumber = (x: any): x is number => typeof x === "number";
export const isBoolean = (x: any): x is boolean => typeof x === "boolean";
export const isError = (x: any): x is Error => x instanceof Error;

// A weird method to check that a string is a string encoding of a number
export const isNumericString = (x: string): boolean => JSON.stringify(+x) === x;

// A predicate for a valid identifier
export type Identifier = string;
export const isIdentifier = (x: any): x is Identifier =>
    /[A-Za-z][A-Za-z0-9]*/i.test(x);