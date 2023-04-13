import { List, cons, isEmpty, isNonEmptyList, first, rest } from "./list";
import { makeOk, makeFailure, Result } from "./result";

export type Optional<T> = Some<T> | None;

type Some<T> = {
  tag: "Some";
  value: T;
};

type None = {
  tag: "None";
};

export const makeSome = <T>(value: T): Optional<T> => ({
  tag: "Some",
  value: value,
});

export const makeNone = <T>(): Optional<T> => ({ tag: "None" });

export const isSome = <T>(o: Optional<T>): o is Some<T> => o.tag === "Some";

export const isNone = <T>(o: Optional<T>): o is None => o.tag === "None";

export const bind = <T, U>(
  o: Optional<T>,
  f: (x: T) => Optional<U>
): Optional<U> => (isSome(o) ? f(o.value) : o);

export const mapv = <T, U>(o: Optional<T>, f: (x: T) => U): Optional<U> =>
  isSome(o) ? makeSome(f(o.value)) : o;

export const maybe = <T, U>(
  o: Optional<T>,
  ifSome: (value: T) => U,
  ifNone: () => U
): U => (isSome(o) ? ifSome(o.value) : ifNone());

export const mapOptional = <T, U>(
  f: (x: T) => Optional<U>,
  list: List<T>
): Optional<List<U>> =>
  isNonEmptyList<T>(list) ? bind(f(first(list)), (fa: U) =>
        bind(mapOptional(f, rest(list)), (fas: List<U>) =>
          makeSome(cons(fa, fas))))
  : makeSome([]);

export const optionalToResult = <T>(
  o: Optional<T>,
  message: string
): Result<T> =>
  maybe(
    o,
    (value: T) => makeOk(value),
    () => makeFailure(message)
  );
