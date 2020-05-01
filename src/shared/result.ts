import { isEmpty, first, rest, cons } from "./list";
import { Optional, makeSome, makeNone } from "./optional";

export type Result<T> = Ok<T> | Failure;

interface Ok<T> {
    tag: "Ok";
    value: T;
}

interface Failure {
    tag: "Failure";
    message: string;
}

export const makeOk = <T>(value: T): Result<T> =>
    ({ tag: "Ok", value: value });

export const makeFailure = <T>(message: string): Result<T> =>
    ({ tag: "Failure", message: message });

export const isOk = <T>(r: Result<T>): r is Ok<T> =>
    r.tag === "Ok";

export const isFailure = <T>(r: Result<T>): r is Failure =>
    r.tag === "Failure";

export const bind = <T, U>(r: Result<T>, f: (x: T) => Result<U>): Result<U> =>
    isOk(r) ? f(r.value) : r;

export const either = <T, U>(r: Result<T>, ifOk: (value: T) => U, ifFailure: (message: string) => U): U =>
    isOk(r) ? ifOk(r.value) : ifFailure(r.message);

// Purpose: Test whether a result is Ok and of a
//          specified type (using a given type predicate)
// Example:
//     const r: Result<Exp> = bind(p("(+ x 1)"), parseL3Exp);
//     isOkT(isAppExp)(r) ? [here "r" is Ok<AppExp>]
export const isOkT = <T>(pred: (x: any) => x is T) => (r: any): r is Ok<T> =>
    isOk(r) && pred(r.value);

// Purpose: Like map on an array - but when the transformer function applied returns a Result<T>
//          With f: T=>Result<U> and list: T[] return a Result<U[]> 
//          If one of the items of the list fails on f - returns the Failure on the first item that fails.
// Example: 
// mapResult((x) => x === 0 ? makeFailure("div by 0") : makeOk(1/x), [1,2]) ==> {tag:"Ok", value:[1, 0.5]}
// mapResult((x) => x === 0 ? makeFailure("div by 0") : makeOk(1/x), [1,0,2]) ==> {tag:"Failure", message:"div by 0"}
export const mapResult = <T, U>(f: (x: T) => Result<U>, list: T[]): Result<U[]> =>
    isEmpty(list) ? makeOk([]) :
    bind(f(first(list)), 
         (fa: U) => bind(mapResult(f, rest(list)), 
                         (fas: U[]) => makeOk(cons(fa, fas))));

export const zipWithResult = <T1, T2, T3>(f: (x: T1, y: T2) => Result<T3>, xs: T1[], ys: T2[]): Result<T3[]> =>
    xs.length === 0 || ys.length === 0 ? makeOk([]) :
    bind(f(first(xs), first(ys)),
         (fxy: T3) => bind(zipWithResult(f, rest(xs), rest(ys)),
                           (fxys: T3[]) => makeOk(cons(fxy, fxys))));

export const safe2 = <T1, T2, T3>(f: (x: T1, y: T2) => Result<T3>): (xr: Result<T1>, yr: Result<T2>) => Result<T3> =>
    (xr: Result<T1>, yr: Result<T2>) =>
        bind(xr, (x: T1) => bind(yr, (y: T2) => f(x, y)));

export const safe3 = <T1, T2, T3, T4>(f: (x: T1, y: T2, z: T3) => Result<T4>): (xr: Result<T1>, yr: Result<T2>, zr: Result<T3>) => Result<T4> =>
    (xr: Result<T1>, yr: Result<T2>, zr: Result<T3>) =>
        bind(xr, (x: T1) => bind(yr, (y: T2) => bind(zr, (z: T3) => f(x, y, z))));

export const resultToOptional = <T>(r: Result<T>): Optional<T> =>
    either(r, makeSome, _ => makeNone());