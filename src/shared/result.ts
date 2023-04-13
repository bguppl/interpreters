import { List, isEmpty, isNonEmptyList, first, rest, cons } from "./list";
import { Optional, makeSome, makeNone } from "./optional";

export type Result<T> = Ok<T> | Failure;

type Ok<T> = {
  tag: "Ok";
  value: T;
};

type Failure = {
  tag: "Failure";
  message: string;
};

export const makeOk = <T>(value: T): Result<T> => ({ tag: "Ok", value: value });

export const makeFailure = <T>(message: string): Result<T> => ({
  tag: "Failure",
  message: message,
});

export const isOk = <T>(r: Result<T>): r is Ok<T> => r.tag === "Ok";

export const isFailure = <T>(r: Result<T>): r is Failure => r.tag === "Failure";

// bind a result value into a happy path function that could fail (f is a diagonal operator)
export const bind = <T, U>(r: Result<T>, f: (x: T) => Result<U>): Result<U> =>
  isOk(r) ? f(r.value) : r;

// bind a result value into a happy path function that does not fail (f is a horizontal operator)
export const mapv = <T, U>(r: Result<T>, f: (x: T) => U): Result<U> =>
  isOk(r) ? makeOk(f(r.value)) : r;

// Traditional Result.bind(f) from diagonal T->R<U> to lifted R<T>->R<U>
// Also known as flatmap
export const rbind =
  <T, U>(f: (x: T) => Result<U>): ((r: Result<T>) => Result<U>) =>
  (r) =>
    isOk(r) ? f(r.value) : r;

// Traditional Result.map(f) from horizontal T->U to lifted R<T>->R<U>
export const rmap =
  <T, U>(f: (x: T) => U): ((r: Result<T>) => Result<U>) =>
  (r) =>
    isOk(r) ? makeOk(f(r.value)) : r;

// Traditionally called Result.fold(result, onOk, onFailure)
export const either = <T, U>(
  r: Result<T>,
  ifOk: (value: T) => U,
  ifFailure: (message: string) => U
): U => (isOk(r) ? ifOk(r.value) : ifFailure(r.message));

// Purpose: Test whether a result is Ok and of a
//          specified type (using a given type predicate)
// Example:
//     const r: Result<Exp> = bind(p("(+ x 1)"), parseL3Exp);
//     isOkT(isAppExp)(r) ? [here "r" is Ok<AppExp>]
export const isOkT =
  <T>(pred: (x: any) => x is T) =>
  (r: any): r is Ok<T> =>
    isOk(r) && pred(r.value);

// Purpose: Like map on an array - but with a diagonal transformer operator (returns a Result<T>)
//          With f: T=>Result<U> and list: List<T> return a Result<List<U>>
//          If one of the items of the list fails on f - returns the Failure on the first item that fails.
// Example:
// mapResult((x) => x === 0 ? makeFailure("div by 0") : makeOk(1/x), [1,2]) ==> {tag:"Ok", value:[1, 0.5]}
// mapResult((x) => x === 0 ? makeFailure("div by 0") : makeOk(1/x), [1,0,2]) ==> {tag:"Failure", message:"div by 0"}
export const mapResult = <T, U>(
  f: (x: T) => Result<U>,
  list: List<T>
): Result<List<U>> =>
  isNonEmptyList<T>(list) ? bind(f(first(list)), 
                                 (fa: U) => bind(mapResult(f, rest(list)), 
                                                 (fas: U[]) => makeOk(cons(fa, fas))))
    : makeOk([]);

export const zipWithResult = <T1, T2, T3>(
  f: (x: T1, y: T2) => Result<T3>,
  xs: List<T1>,
  ys: List<T2>
): Result<List<T3>> =>
  isNonEmptyList<T1>(xs) && 
  isNonEmptyList<T2>(ys) ? bind(f(first(xs), first(ys)), 
                                (fxy: T3) => bind(zipWithResult(f, rest(xs), rest(ys)), 
                                                  (fxys: T3[]) => makeOk(cons(fxy, fxys))))
  : makeOk([]);

export const safe2 =
  <T1, T2, T3>(
    f: (x: T1, y: T2) => Result<T3>
  ): ((xr: Result<T1>, yr: Result<T2>) => Result<T3>) =>
  (xr: Result<T1>, yr: Result<T2>) =>
    bind(xr, (x: T1) => bind(yr, (y: T2) => f(x, y)));

export const resultToOptional = <T>(r: Result<T>): Optional<T> =>
  either(r, makeSome, (_) => makeNone());
