// ========================================================
// Option monad (aka Maybe)
// Michael Elhadad - April 2023

import { deepStrictEqual } from "assert";
import { Replace } from "./replace";
import { pipe } from 'ramda';

// Type definition
export type Option<T> = Some<T> | None;

export type None = { tag: "none" };
export const isNone = (x: any): x is None => x.tag === "none";
export const none = (): None => ({ tag: "none" });

export type Some<T> = { tag: "some"; value: T };
export const isSome = <T>(x: any): x is Some<T> => x.tag === "some";
export const some = <T>(x: T): Option<T> => ({ value: x, tag: "some" });

// Override application operator for Option values
// transform is a "diagonal operator" - from T1 => Option<T2>
// bind adapts diagonal operators to work on Option<T1> values.
export const bindOption = <T1, T2>(
  input: Option<T1>,
  transform: (x: T1) => Option<T2>
): Option<T2> => (isSome(input) ? transform(input.value) : input);

// ==================================
// Lift from values to monadic values

// wrapOption: lift a value
export const wrapOption = <T>(x: T | undefined): Option<T> =>
  x === undefined ? none() : some(x);

// mapOption: flat [T1=>T2] to lifted [Option<T1>=>Option<T2>]
export const mapOption =
  <T1, T2>(f: (x: T1) => T2): ((y: Option<T1>) => Option<T2>) =>
  (y) =>
    bindOption(y, (x: T1) => wrapOption(f(x)));

// chainOption: diagonal [T1=>Option<T2>] to lifted [Option<T1>=>Option<T2>]
// When composing a mixture of diagonal and flat functions - use map and chain as params to pipe
// When composing only diagonal functions - use pipeOption
export const chainOption =
  <T1, T2>(f: (x: T1) => Option<T2>): ((y: Option<T1>) => Option<T2>) =>
  (y) =>
    bindOption(y, f);

pipe(
	wrapOption, 
	mapOption((x: number) => x * x), 
	chainOption((x) => x === 0 ? none() : some(1/x)), 
	mapOption((x) => x * x)
)(5);

// =======================================================================================
// pipeOption composes a variadic chain of diagonal operators into a single diagonal result.
// The good typing solution is a bit complex :-) but it can be achieved with TypeScript type system.

// For pipeOption, all the functions in the chain must be of type fi: [Ti => Option<Ti+1>] fi+1: [Ti+1 => Option<Ti+2>] ...
// pipeOption overrides function application with the bind operator: compose fi(x) and fi+1 as in bind(fi(x), fi+1)
// as opposed to fi+1(fi(x)) in regular pipe.

// This is a general diagonal operator
type OptionComp = (arg: any) => Option<any>;

// This type checks that a list of diagonal operators form a valid composition chain
// that is Ti -> Option<Ti+1>
// If the chain is valid, this type is the same as the chain (an array of diagonal operators)
// else it returns the offset from the end of the first
// offending function in the chain (as a singleton number type)
// This will allow us to replace the type of the offending function with an invalid type
// which will be helpful when understanding the type checker feedback.
type IsValidOptionChain<T extends OptionComp[]> = T extends [
  infer $First extends OptionComp,
  infer $Second extends OptionComp,
  ...infer $Rest extends OptionComp[]
]
  ? [ReturnType<$First>] extends [Option<Parameters<$Second>[0]>]
    ? IsValidOptionChain<[$Second, ...$Rest]>
    : T extends [any, ...infer $Rest]
    ? $Rest["length"]
    : never
  : true;

// Put together IsValidOptionChain with Replace in case of failure
type PipeOptionParamType<Composables extends OptionComp[]> =
  IsValidOptionChain<Composables> extends infer $Offset extends number
    ? Replace<Composables, $Offset, "INVALID_COMPOSABLE">
    : Composables;

// This computes the return type (without Option<>) of the chain
// Extracts the type of the last function in the chain.
// In the case of an empty chain - just return a free Type Variable T
type OptionChainReturnType<Chain extends OptionComp[], T> = Chain extends [
  ...any[],
  (arg: never) => Option<infer Last>
]
  ? Last
  : Chain["length"] extends 0
  ? T
  : never;

// This computes the type of the parameter of the chain
// It is the first parameter of the first function in the chain.
// In the case of an empty chain - just return a free type variable T.
type OptionChainParamType<
  Chain extends OptionComp[],
  T
> = Chain["length"] extends 0
  ? T
  : Parameters<Chain[0]>["length"] extends 0
  ? void
  : Parameters<Chain[0]>[0];

// ---------------------------
// We can now implement a type safe pipeOption
export const pipeOption =
  <Composables extends OptionComp[]>(
    ...composables: PipeOptionParamType<Composables>
  ) =>
  // return a function
  <T>(
    firstData: OptionChainParamType<Composables, T>
  ): Option<OptionChainReturnType<Composables, T>> => {
    let data: any = firstData;
    // Override application operator for Option
    // inline the bind logic - enable shortcut return on None
    for (const composable of composables) {
      data = composable(data);
      if (isNone(data)) return data;
      else data = data.value;
    }
    return wrapOption<OptionChainReturnType<Composables, T>>(data);
  };

// Methods to unlift from the Option
// -------------------------------
// getOrElseOption
// Example:
// getOrElse(some(1), undefined) -> 1
// getOrElse(none(), undefined) -> undefined
export const getOrElseOption = <T1, T2>(o: Option<T1>, whenNone: T2): T1 | T2 =>
  isNone(o) ? whenNone : o.value;

// foldOption(o:Option<T1>, ()=>T2, (val:T1)=>T3)
// Example:
// foldOption(none(), () => undefined, (val: number) => val) -> undefined
// foldOption(some(1), () => undefined, (val: number) => val) -> 1
export const foldOption = <T1, T2, T3>(
  o: Option<T1>,
  handleNone: () => T2,
  handleSome: (val: T1) => T3
) => (isSome(o) ? handleSome(o.value) : handleNone());

// ===========================================
// Examples

// Unlift
const o1 = none();
const o2 = some(1);

deepStrictEqual(getOrElseOption(o1, undefined), undefined);
deepStrictEqual(getOrElseOption(o2, undefined), 1);

deepStrictEqual(
  foldOption(
    o1,
    () => undefined,
    (val) => val
  ),
  undefined
);

deepStrictEqual(
  foldOption(
    o2,
    () => undefined,
    (val) => val
  ),
  1
);

// Compose and apply

deepStrictEqual(
  pipeOption(
    () => o1,
    (x: number) => some(x * 2),
    (x: number) => some(1 / x)
  )(),
  none()
);

deepStrictEqual(
  pipeOption(
    () => o2,
    (x: number) => some(x * 2),
    (x: number) => some(1 / x)
  )(),
  some(0.5)
);

deepStrictEqual(
  bindOption(o1, (x: number) => some(x)),
  none()
);

deepStrictEqual(
  bindOption(o2, (x: number) => some(x)),
  o2
);

// map
deepStrictEqual(mapOption((x: number) => x * 2)(o1), none());
deepStrictEqual(mapOption((x: number) => x * 2)(o2), some(2));
