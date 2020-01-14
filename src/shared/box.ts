// ========================================================
// Box datatype
// Encapsulate mutation in a single type.
export type Box<T> = T[];
export const makeBox = <T>(x: T): Box<T> => ([x]);
export const unbox = <T>(b: Box<T>): T => b[0];
export const setBox = <T>(b: Box<T>, v: T): void => { b[0] = v; return; }
