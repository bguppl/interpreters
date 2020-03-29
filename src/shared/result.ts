export type Result<T> = Ok<T> | Failure

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

export const map = <T, U>(f: (x: T) => U, r: Result<T>): Result<U> =>
    isOk(r) ? makeOk(f(r.value)) : r;

export const bind = <T, U>(r: Result<T>, f: (x: T) => Result<U>): Result<U> =>
    isOk(r) ? f(r.value) : r;

export const mapResult = <T, U>(f: (x: T) => Result<U>, list: T[]): Result<U[]> =>
    list.length === 0 ? makeOk([]) :
    bind(f(list[0]), fa => bind(mapResult(f, list.slice(1)), fas => makeOk([fa].concat(fas))));