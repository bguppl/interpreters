// List operations similar to car/cdr/cadr in Scheme
import { all } from "ramda";

export type List<T> = T[];
export type Empty = [];
export type NonEmptyList<T> = [T, ...T[]];

export const cons = <T>(x: T, xs: List<T>): List<T> => [x, ...xs];
export const first = <T>([x, ..._xs]: NonEmptyList<T>): T => x;
export const second = <T>([_x0, x1, ..._xs]: NonEmptyList<T>): T => x1;
export const rest = <T>([_, ...xs]: NonEmptyList<T>): List<T> => xs;
export const isNonEmptyList = <T>(xs: any): xs is NonEmptyList<T> =>
  Array.isArray(xs) && xs.length > 0;
export const isEmpty = <T>(xs: any): xs is Empty =>
  Array.isArray(xs) && ! isNonEmptyList<T>(xs);

// A useful type predicate for homogeneous lists
export const allT = <T>(isT: (x: any) => x is T, x: any[]): x is T[] =>
  all(isT, x);
