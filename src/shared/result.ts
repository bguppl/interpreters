import { isEmpty, first, rest } from "./list";

export type Result<T> = Ok<T> | Failure
​
interface Ok<T> {
    tag: "Ok";
    value: T;
}
​
interface Failure {
    tag: "Failure";
    message: string;
}
​
export const makeOk = <T>(value: T): Result<T> =>
    ({ tag: "Ok", value: value });
​
export const makeFailure = <T>(message: string): Result<T> =>
    ({ tag: "Failure", message: message });
​
export const isOk = <T>(r: Result<T>): r is Ok<T> =>
    r.tag === "Ok";
​
export const isFailure = <T>(r: Result<T>): r is Failure =>
    r.tag === "Failure";
​
export const map = <T, U>(f: (x: T) => U, r: Result<T>): Result<U> =>
    isOk(r) ? makeOk(f(r.value)) : r;
​
export const bind = <T, U>(r: Result<T>, f: (x: T) => Result<U>): Result<U> =>
    isOk(r) ? f(r.value) : r;
​
// Purpose: Like map on an array - but when the transformer function applied returns a Result<T>
//          With f: T=>Result<U> and list: T[] return a Result<U[]> 
//          If one of the items of the list fails on f - returns the Failure on the first item that fails.
// Example: 
// mapResult((x)=>x == 0 ? makeFailure("div by 0") : makeOk(1/x), [1,2]) ==> {tag:"Ok", value:[1, 0.5]}
// mapResult((x)=>x == 0 ? makeFailure("div by 0") : makeOk(1/x), [1,0,2]) ==> {tag:"Failure", message:"div by 0"}
export const mapResult = <T, U>(f: (x: T) => Result<U>, list: T[]): Result<U[]> =>
    isEmpty(list) ? makeOk([]) :
    bind(f(first(list)), 
         (fa: U) => bind(mapResult(f, rest(list)), 
                         (fas: U[]) => makeOk([fa].concat(fas))));
