// List operations similar to car/cdr/cadr in Scheme
import { all } from 'ramda';

export const cons = <T>(x: T, xs: T[]): T[] => [x].concat(xs);
export const first = <T>(x: T[]): T => x[0];
export const second = <T>(x: T[]): T => x[1];
export const rest = <T>(x: T[]): T[] => x.slice(1);
export const isEmpty = (x: any): boolean => Array.isArray(x) && x.length === 0;

// A useful type predicate for homogeneous lists
export const allT = <T>(isT: (x: any) => x is T, x: any[]): x is T[] => all(isT, x);
