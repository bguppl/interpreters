import { cons, isEmpty, first, rest } from "./list";
import { makeOk, makeFailure, Result } from "./result";

export type Optional<T> = Some<T> | None;

interface Some<T> {
    tag: "Some";
    value: T;
}

interface None {
    tag: "None";
}

export const makeSome = <T>(value: T): Optional<T> =>
    ({ tag: "Some", value: value });

export const makeNone = <T>(): Optional<T> =>
    ({ tag: "None" });

export const isSome = <T>(o: Optional<T>): o is Some<T> =>
    o.tag === "Some";

export const isNone = <T>(o: Optional<T>): o is None =>
    o.tag === "None";

export const bind = <T, U>(o: Optional<T>, f: (x: T) => Optional<U>): Optional<U> =>
    isSome(o) ? f(o.value) : o;

export const maybe = <T, U>(o: Optional<T>, ifSome: (value: T) => U, ifNone: () => U): U =>
    isSome(o) ? ifSome(o.value) : ifNone();

export const mapOptional = <T, U>(f: (x: T) => Optional<U>, list: T[]): Optional<U[]> =>
    isEmpty(list) ? makeSome([]) :
    bind(f(first(list)),
         (fa: U) => bind(mapOptional(f, rest(list)),
                         (fas: U[]) => makeSome(cons(fa, fas))));

export const safe2 = <T1, T2, T3>(f: (x: T1, y: T2) => Optional<T3>): (xr: Optional<T1>, yr: Optional<T2>) => Optional<T3> =>
    (xr: Optional<T1>, yr: Optional<T2>) =>
        bind(xr, (x: T1) => bind(yr, (y: T2) => f(x, y)));

export const safe3 = <T1, T2, T3, T4>(f: (x: T1, y: T2, z: T3) => Optional<T4>): (xr: Optional<T1>, yr: Optional<T2>, zr: Optional<T3>) => Optional<T4> =>
    (xr: Optional<T1>, yr: Optional<T2>, zr: Optional<T3>) =>
        bind(xr, (x: T1) => bind(yr, (y: T2) => bind(zr, (z: T3) => f(x, y, z))));

export const optionalToResult = <T>(o: Optional<T>, message: string): Result<T> =>
    maybe(o, (value: T) => makeOk(value), () => makeFailure(message));