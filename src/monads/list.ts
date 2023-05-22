// list.ts
// Michael Elhadad - Apr 2023
// Observe the similarity of the functions between two monads: Option and List

import { Replace } from "./replace";
import { deepStrictEqual } from "assert";

// Recursive Type definition
export type List<T> = Empty | NonEmptyList<T>;
export type Empty = [];
export type NonEmptyList<T> = [T, ...Array<T>];

// Type predicates
export const isEmpty = <T>(x: Array<T>): x is Empty => x.length === 0;
export const isNonEmpty = <T>(x: Array<T>): x is NonEmptyList<T> =>
  x.length > 0;

// Type accessors
export const first = <T>(x: NonEmptyList<T>): T => x[0];
export const rest = <T>(x: NonEmptyList<T>): List<T> => <List<T>>x.slice(1);

export const wrapList = <T>(x: T): NonEmptyList<T> => [x];

// Known as flatMap
// Since List is a recursive type, bind is a recursive function
export const bindList = <T1, T2>(
  l: List<T1>,
  f: (x: T1) => List<T2>
): List<T2> => (isEmpty(l) ? l : [...f(first(l)), ...bindList(rest(l), f)]);

// Known as map
export const mapList =
  <T1, T2>(f: (x: T1) => T2): ((y: List<T1>) => List<T2>) =>
  (l: List<T1>) =>
    bindList(l, (x: T1) => wrapList(f(x)));

// chainList: diagonal [T1=>List<T2>] to lifted [List<T1>=>List<T2>]
// When composing a mixture of diagonal and flat functions - use map and chain as params to pipe
// When composing only diagonal functions - use pipeList
export const chainList =
  <T1, T2>(f: (x: T1) => List<T2>): ((y: List<T1>) => List<T2>) =>
  (y: List<T1>) => bindList(y, f);

// pipeList
// =======================================================================================
// For pipeList, all the functions in the chain must be of type fi: [Ti => List<Ti+1>]

// This is a general diagonal operator
type ListComp = (arg: any | void) => List<any>;

type IsValidListChain<T extends ListComp[]> = T extends [
  infer $First extends ListComp,
  infer $Second extends ListComp,
  ...infer $Rest extends ListComp[]
]
  ? [ReturnType<$First>] extends [List<Parameters<$Second>[0]>]
    ? IsValidListChain<[$Second, ...$Rest]>
    : T extends [any, ...infer $Rest]
    ? $Rest["length"]
    : never
  : true;

// Put together IsValidOptionChain with Replace in case of failure
type PipeListParamType<Composables extends ListComp[]> =
  IsValidListChain<Composables> extends infer $Offset extends number
    ? Replace<Composables, $Offset, "INVALID_COMPOSABLE">
    : Composables;

// This computes the return type (without Option<>) of the chain
// Extracts the type of the last function in the chain.
// In the case of an empty chain - just return a free Type Variable T
type ListChainReturnType<Chain extends ListComp[], T> = Chain extends [
  ...any[],
  (arg: never) => List<infer Last>
]
  ? Last
  : Chain["length"] extends 0
  ? T
  : never;

type ListChainParamType<Chain extends ListComp[], T> = Chain["length"] extends 0
  ? T
  : Parameters<Chain[0]>["length"] extends 0
  ? void
  : Parameters<Chain[0]>[0];

// Expand all results into a tree of results - each composable returns a list of results
// Flatten all results.
export const pipeList =
  <Composables extends ListComp[]>(
    ...composables: PipeListParamType<Composables>
  ) =>
  // return a function
  <T>(
    firstData: ListChainParamType<Composables, T>
  ): List<ListChainReturnType<Composables, T>> => {
    let data: any = wrapList(firstData);
    // Override application operator for list
    for (const composable of composables) {
      data = bindList(data, composable);
    }
    return data;
  };

// Methods to unlift from the List
// -------------------------------
// getOrElseList
// Example:
// getOrElse([1], undefined) -> 1
// getOrElse([], undefined) -> undefined
export const getOrElseList = <T1, T2>(l: List<T1>, whenEmpty: T2): T1 | T2 =>
  isEmpty(l) ? whenEmpty : first(l);

// Aka reduce
// Like bindList (flatMap), foldList is recursive to fold the recursive type.
// foldList(l:List<T1>, ()=>T2, (firstVal:T1, restFolded: T2)=>T2)
// Example:
// foldList([], () => 0, (firstVal: number, restFolded: number) => firstVal + restFolded) -> 0
// foldList([1,2], () => 0, (firstVal: number, restFolded: number) => firstVal + restFolded) -> 3
export const foldList = <T1, T2>(
  l: List<T1>,
  handleEmpty: () => T2,
  handleNonEmpty: (firstVal: T1, restFolded: T2) => T2
): T2 =>
  isEmpty(l)
    ? handleEmpty()
    : handleNonEmpty(first(l), foldList(rest(l), handleEmpty, handleNonEmpty));

// =================================================
// Examples

// Unlift
const l1: Empty = [];

// Literal type inference from [1] to NonEmptyList<number> fails
// it widens to List<number>
const l2: NonEmptyList<number> = [1];
const l3: NonEmptyList<number> = [1, 2];

// But it works in context
deepStrictEqual(first([1]), 1);
deepStrictEqual(first([1, 2]), 1);
deepStrictEqual(rest([1]), []);
deepStrictEqual(rest([1, 2]), [2]);

// When variables are declared with type - all ok
deepStrictEqual(first(l2), 1);
deepStrictEqual(first(l3), 1);

deepStrictEqual(rest(l2), []);
deepStrictEqual(rest(l3), [2]);

deepStrictEqual(
  foldList(
    l1,
    () => 0,
    (val: number, acc) => val + acc
  ),
  0
);

deepStrictEqual(
  foldList(
    l2,
    () => 0,
    (val, acc) => val + acc
  ),
  1
);

deepStrictEqual(
  foldList(
    l3,
    () => 0,
    (val, acc) => val + acc
  ),
  3
);

// Compose and apply

deepStrictEqual(
  pipeList(
    () => [],
    (x: number) => wrapList(x * 2),
    (x: number) => wrapList(1 / x)
  )(),
  []
);

deepStrictEqual(
  pipeList(
    () => [1],
    (x: number) => [x * 2],
    (x: number) => [1 / x]
  )(),
  [0.5]
);

deepStrictEqual(
  pipeList(
    () => [1, 2],                           // [1, 2]
    (x: number) => [x - 1, x],              // [0, 1, 1, 2]
    (x: number) => (x === 0 ? [] : [1 / x]) // [1, 1, 1/2]
  )(),
  [1, 1, 0.5]
);

deepStrictEqual(
  pipeList(
    () => [1, 2],                  // [1,               2]
    (x: number) => [x * 2, x * 4], // [2,      4,       4,       8]
    (x: number) => [1 / x, x * x]  // [1/2, 4, 1/4, 16, 1/4, 16, 1/8, 64]
  )(),
  [0.5, 4, 0.25, 16, 0.25, 16, 0.125, 64]
);

deepStrictEqual(
  bindList(l1, (x: number) => wrapList(x)),
  []
);

deepStrictEqual(
  bindList(l2, (x: number) => wrapList(x)),
  l2
);

deepStrictEqual(
  bindList(l3, (x: number) => wrapList(x)),
  l3
);

// map
deepStrictEqual(mapList((x: number) => x * 2)(l1), l1);
deepStrictEqual(mapList((x: number) => x * 2)(l2), [2]);
deepStrictEqual(mapList((x: number) => x * 2)(l3), [2, 4]);
