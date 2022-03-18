// List operations similar to car/cdr/cadr in Scheme
import { every } from "fp-ts/ReadonlyArray";

export const cons = <T>(x: T, xs: readonly T[]): readonly T[] => [x].concat(xs);
export const first = <T>(x: readonly T[]): T => x[0];
export const second = <T>(x: readonly T[]): T => x[1];
export const rest = <T>(x: readonly T[]): readonly T[] => x.slice(1);
export const isEmpty = (x: any): boolean => Array.isArray(x) && x.length === 0;

// A useful type predicate for homogeneous lists
export const allT = <T>(isT: (x: any) => x is T, x: readonly any[]): x is T[] => every(isT)(x);
