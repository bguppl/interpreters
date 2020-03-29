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

export const sequence = <T>(results: Result<T>[]): Result<T[]> => {
    if (results.length === 0) {
        return makeOk([]);
    } else {
        const [r, ...rs] = results;
        return bind(r, r => bind(sequence(rs), rs => makeOk([r].concat(rs))));
    }
}
