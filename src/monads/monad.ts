import { pipe } from "ramda";

// Monad.ts
// https://www.youtube.com/watch?v=C2w45qRc3aU
// Absolute Best Intro to Monads for Software Engineers
// 00:00 Intro
// 00:29 Basic Code
// 01:45 Issue #1
// 02:38 Issue #2
// 04:11 Putting It All Together
// 05:15 Properties of Monads
// 06:05 The Option Monad
// 09:14 Monads Hide Work Behind The Scenes
// 11:21 Common Monads
// 12:10 The List Monad
// 13:56 Recap
//
// Inspired by:
// Bartosz Mikewski - Category Theory 3.2: Kleisli category https://www.youtube.com/watch?v=i9CU4CuHADQ
// This is a more mathematical description of the same material.
//
// Scott Waschlin - The Power of Composition - https://www.youtube.com/watch?v=vDe-4o8Uwl8
// - Functions are "things"
// = Composition everywhere
// = Types are not classes (they are sets)
//
// Pipe example
// x |> f1 |> f2 |> f3
// In Typescript:
// bind(bind(bind(x, f1), f2), f3)
// Or using Ramda:
// pipe(f1, f2, f3)(x)

// Algebraic Type Systems:
// - Composable type systems - compose types with OR and AND
// - AND: tuple
// - OR: union (discriminated union) usually product types
// Can compose types
//
// Obstacles to composition
// - Multiple parameters
//   Example: replace(oldValue, newValue, inputStr) -> string
// - Solution: currying / partial functions
// replace(old)(new)(inputStr)
// - One input - multiple possible outputs
// - Different types of inputs - multiple possible outputs
// - Multiple inputs - One output
// - Multiple inputs - Multiple outputs

// Solutions:
// wrap unions and products of values into algebraic types
// override composition operator for each type of types

// ===========================
// Motivating example: logging

// Consider simple full functions [number => number]
const square = (x: number): number => x * x;
const inc = (x: number): number => x + 1;

// It is easy to compose them anyway we want

inc(square(2));

pipe(square, inc)(2);

// Consider a new requirement: We now want to trace the logs
// This may be needed for debugging a system in production
// or for auditing reasons

// We do not want to introduce the I/O operation as part
// of the computation to avoid breaking separation of concerns
// and introducing a global dependency on a logger
// which would make it difficult to run these functions in
// concurrent runtime environment (would require serialization of the
// logging operations).

// We settle for this protocol instead:
// We wrap the result in a map and add a logs field
// The logs field accumulates the logs as operations are performed.
/*
inc(square(2)) --> {
	result: 5,
	logs: [
		"Squared 2 to get 4",
		"Added 1 to 4 to get 5"
	]
}
*/

// To support this we introduce a wrapper type
// which keeps track of the logs list.
type NumberWithLogs = {
  result: number;
  logs: string[];
};

// First attempt:
const square1 = (x: number): NumberWithLogs => ({
  result: x * x,
  logs: [`Squared ${x} to get ${x * x}`],
});

// the input param is a wrapper which "remembers" what has already been performed.
// the operation concatenates a new line to the logs field
const inc1 = (x: NumberWithLogs): NumberWithLogs => ({
  result: x.result + 1,
  logs: [...x.logs, `Added 1 to ${x.result} to get ${x.result + 1}`],
});

// { result: 5, logs: [ 'Squared 2 to get 4', 'Added 1 to 4 to get 5' ] }
console.log(inc1(square1(2)));

// Problems: these functions are not composable!
// inc1(5) --> bad type
// square1(square1(2)) --> bad type
// square1(inc1(2)) --> bad type

// In general - we observe that "uniform" function types [N => N]
// became "non-uniform" [N => NwL] and [NwL => NwL]

// To resolve this - let us:
// 1. disentangle the construction and maintenance of the logs list from the computation.
// 2. align all operations to the same type

// Let's first create a "constructor" for the new type (we will call this a wrapper)
// The wrapper moves the initial parameter into the "NumberWithLogs" domain
const wrapNumberWithLogs = (x: number): NumberWithLogs => ({
  result: x,
  logs: [],
});

// Align all the functions to the same shape [NwL => NwL]
// We then combine NumberWithLogs values
const square2 = (x: NumberWithLogs): NumberWithLogs => ({
  result: x.result * x.result,
  logs: [...x.logs, `Squared ${x.result} to get ${x.result * x.result}`],
});

// This mechanism is now composable when we use the wrapper where needed
console.log(square2(square2(wrapNumberWithLogs(2))));
console.log(inc1(wrapNumberWithLogs(5)));

pipe(wrapNumberWithLogs, inc1, square2)(5);

// Graphically - we think of two planes:
// - the "plain type" (numbers in our example)
// - the "embellished type" (NumberWithLogs in our example)
// The last pipe can be described as this route:
// NumberWithLogs       NwL[5] -inc1-> NwL[6] --square2--> NwL[36]
//                      /
//                   wrap
//                   /
// Number          5
// In this diagram:
// - wrap is a "diagonal" operator (from normal type to embellished type)
// - inc1 and square2 are "lifted" operators (instead of N=>N - they are NwL=>NwL)

// =================================================
// Remove the duplicated code:
// We see in all functions of type [NwL => NwL] the same code will appear:
// logs: [...x.logs, `...abc`]
// This repetition is a bad smell of something wrong - we want to abstract it away.
// Also we want to fix the violation of "separation of concern":
// The new functions (inc1, square2) must "know about log concatenation" - we want
// the function to only know about how to compute (increment, square).

// Just an intermediary step towards a solution:
// Separate log concatenation logic from core of function
const square3 = (x: NumberWithLogs): NumberWithLogs => {
  // Code that is specific to 'square'
  const result = {
    result: x.result * x.result,
    logs: [`Squared ${x.result} to get ${x.result * x.result}`],
  };
  // Code is always the same for all [NwL => NwL] functions
  return {
    result: result.result,
    logs: [...x.logs, ...result.logs],
  };
};

// Let us "abstract away" the repeated code in a separate function.
// We will write the application of functions in the "NumberWithlogs domain" as:
// runWithLogs(wrapWithLogs(5), inc)
// Instead of inc(5) in the "number domain".
// In our intermediary version, we used inc1(wrapWithLogs(5))
// Now, runWithLogs will deal with log concatenation and the function
// will only do the part that is specific to the transformation it computes.

// Infer the type of runWithLogs:
const runWithLogs = (
  x: NumberWithLogs,
  transform: (y: number) => NumberWithLogs
) => {
  // transform is the "parametric" transformation
  // different for each function
  const newNumberWithLogs = transform(x.result);
  // The constant part is kept here
  return {
    result: newNumberWithLogs.result,
    logs: [...x.logs, ...newNumberWithLogs.logs],
  };
};

// Now the signature of the "transform" functions is simplified:
// - Take a simple number as parameter
// - Return a NumberWithLogs with a single log message
//
// These functions all have the following structure:
// - From "Normal type" (number) to "Embellished Type" (NumberWithLogs)
// - They do not "know" about how to "compute" the embellished type (concatenate logs)
// These transformers are "diagonal" operators,

const square4 = (x: number): NumberWithLogs => ({
  result: x * x,
  logs: [`Squared ${x} to get ${x * x}`],
});

const inc4 = (x: number): NumberWithLogs => ({
  result: x + 1,
  logs: [`Added 1 to ${x} to get ${x + 1}`],
});

// Usage: we can now combine the calls in any combination
//        we only need to start with a "wrapped" value in the embellished type
const a = wrapNumberWithLogs(5);
const b = runWithLogs(a, inc4);
const c = runWithLogs(b, square4);
const d = runWithLogs(c, square4);
console.log(d);
/*
{
  result: 1296,
  logs: [
    'Added 1 to 5 to get 6',
    'Squared 6 to get 36',
    'Squared 36 to get 1296'
  ]
}
*/

// If we want to use pipe - a new version of pipe is needed - that knows about the logic
// of this type with its running protocol.
// pipeWithLogs combines a sequence of diagonal operators
// and "overrides" the composition operator by using runWithLogs

const isEmpty = <T>(l: T[]): boolean => l.length === 0;
// @Precondition: l is non-empty
const first = <T>(l: T[]): T => l[0];
const rest = <T>(l: T[]): T[] => l.slice(1);

const pipeWithLogs = (
  ...funcs: ((x: number) => NumberWithLogs)[]
): ((x: number) => NumberWithLogs) =>
  isEmpty(funcs)
    ? wrapNumberWithLogs
    : (x: number) => runWithLogs(first(funcs)(x), pipeWithLogs(...rest(funcs)));

const e = pipeWithLogs(inc4, square4, square4)(5);
console.log(e);

// ========================================================
// That design pattern is a Monad
// Its aim is to facilitate composition of complex functions.

// Monads have 3 components:
// - Wrapper Type (in our example NumberWithLogs)
// - Wrap Function (in our example wrapNumberWithLogs)
// - Run function: runs a transformation on a monadic value (runWithLogs)

// ========================================================
// Option monad (aka Maybe)

// number
// Option<number> = a number OR nothing
// Option<User> = a User OR nothing
// T: raw type
// Option<T>: wrapped type

type Option<T> = Some<T> | None;
type None = { tag: "none" };
type Some<T> = { tag: "some"; value: T };
// Type predicate
const isSome = <T>(x: any): x is Some<T> => x.tag === "some";
const some = <T>(x: T): Option<T> => ({ value: x, tag: "some" });
const none = (): None => ({ tag: "none" });
const isNone = <T>(x: any): x is None => x.tag === "none";
const wrap = <T>(x: T | undefined): Option<T> =>
  x === undefined ? none() : some(x);

// Override application operator for Option
const bind = <T1, T2>(
  input: Option<T1>,
  transform: (x: T1) => Option<T2>
): Option<T2> => (isSome(input) ? transform(input.value) : input);

// Override composition operator for Option - composition uses bind
const pipeOption2 =
  <T1, T2, T3>(f1: (x: T1) => Option<T2>, f2: (x: T2) => Option<T3>) =>
  (x: T1) =>
    bind(f1(x), f2);

const pipeOption3 =
  <T1, T2, T3, T4>(
    f1: (x: T1) => Option<T2>,
    f2: (x: T2) => Option<T3>,
    f3: (x: T3) => Option<T4>
  ) =>
  (x: T1) =>
    bind(bind(f1(x), f2), f3);

// Can we generalize pipeOption to a variadic function that takes any number of parameters
// and type checks that the chain is valid?
// We could use function overriding in TypeScript, but this is verbose and limited to a fixed number of parameters.
//
// The following is not a good typing solution - it does not type check the chain:
/*
const pipeOption = <T1, T2>(...funcs: ((x: T1) => Option<T2>)[]) => 
	isEmpty(funcs) ? wrap : 
	((x: T1) => bind(first(funcs)(x), pipeOption(...rest(funcs))));
*/
// We want a compatible chain T1, T2, T3...
// This type must have the same types T1/T2 for all functions - not good
// It would only work for a homogeneous chain T->T->T....

// =======================================================================================
// The good typing solution is a bit complex :-) but it can be achieved with TypeScript type system.

// This declares a unique symbol for the type checker only
declare const INVALID_COMPOSABLE_CHAIN: unique symbol;

// Let us start with the simpler task of regular function composition.

// This a composable function with no constraint on types
type Comp = (arg: any) => any;

// This verifies that a list of function types are composable
// That is, for all i: [... fi, fi+1, ...] fi: Ti-1 -> Ti, fi+1: Ti -> Ti+1
// (arg: never) => any is the most general 1-arg function type (arg position is contravariant)
// The parameter to IsValidChain is an array of type expressions that must all extend the most general function type.
// The type definition is recursive - it binds the first and second args to $First and $Second
// and $Rest to the rest of the array.
// It checks that the return type of the first is compatible with the arg type of the second
// This is the "connecting" test - fi feeds values into fi+1
// If it fits - we continue testing the rest from [$Second, ...$Rest]
// Else we return the position of the offending function type in the chain.
// Observe how the type constructors ReturnType<functionType> and Parameters<functionType>
// are used to decompose the components of the function type.
// If fi and fi+1 do not match - then return the position of fi+1 - the offending function in the chain.
// If the array only has one element (which must be a function type) - then return true.
type IsValidChain<T extends ((arg: never) => any)[]> = T extends [
  infer $First extends Comp,
  infer $Second extends Comp,
  ...infer $Rest extends Comp[]
]
  ? [ReturnType<$First>] extends [Parameters<$Second>[0]]
    ? IsValidChain<[$Second, ...$Rest]>
    : T extends [any, ...infer $Rest]
    ? $Rest["length"]
    : never
  : true;

// Replace item at position Offset in an array of types with the type Item.
// This is done when we identify that a function type "breaks" the chain of composition.
// $Draft is a local variable used in the recursion to accumulate the elements until the replacement.
// We check that the length of $Draft === the desired position with "$Draft["length"] extends Offset"
// because we know Offset is a number - the only way to extend the type 3 for example is to be 3.
// Else we recurse with the last item $Item pushed into $Draft and $Before containing all but the last item.
type ReplaceFromBack<
  T extends unknown[],
  Offset extends number,
  Item,
  $Draft extends unknown[] = []
> = $Draft["length"] extends Offset
  ? $Draft extends [any, ...infer $After]
    ? [...T, Item, ...$After]
    : never
  : T extends [...infer $Before, infer $Item]
  ? ReplaceFromBack<$Before, Offset, Item, [$Item, ...$Draft]>
  : never;

// Example: the type asdf is [1, 2, 3, 4, 5, 6, "hey", 8, 9]
type asdf = ReplaceFromBack<[1, 2, 3, 4, 5, 6, 7, 8, 9], 3, "hey">;

// We can now define a function like compose which enforces proper "compsability" of its parameters
// myPipe is the usual function composition operator (as it is defined in Ramda)
// If the parameter composables (variadic number of arguments) does not match the composition constraint
// we type it as a chain with INVALID_COMPOSABLE in the position that does not fit - this will make type checking fail
// and provide detailed feedback to the programmer - which function is not ok in the chain.
// If the types match the constraint - we compute the type of the resulting function as input type of the first function in
// the chain and return type of the last function in the chain.
function myPipe<Composables extends [Comp, ...Comp[]]>(
  ...composables: IsValidChain<Composables> extends infer $Offset extends number
    ? ReplaceFromBack<Composables, $Offset, "INVALID_COMPOSABLE">
    : Composables
) {
  // myPipe returns a function
  return (
    firstData: Parameters<Composables[0]>[0]
  ): Composables extends [...any[], infer $Last extends (arg: never) => any]
    ? ReturnType<$Last>
    : never => {
    // Implement the pipe composition in a non-functional manner
    // Note that it is ok to use 'any' here because we know the types match
    // from the parameter type checking.
    // We could use instead a recursive functional implementation.
    let data: any = firstData;
    for (const composable of composables) {
      data = (composable as any)(data);
    }
    return data;
  };
}

// For pipeOption, the difference is that all the functions in the chain
// must be of type fi: [Ti => Option<Ti+1>] fi+1: [Ti+1 => Option<Ti+2>] ...
// The bind operator is used to compose fi(x) and fi+1 as in bind(fi(x), fi+1)
type OptionComp = (arg: never) => Option<any>;

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

type ChainReturnType<Chain extends OptionComp[], T> = Chain extends [
  ...any[],
  (arg: never) => Option<infer Last>
]
  ? Last
  : Chain["length"] extends 0
  ? T
  : never;

type ChainParamType<Chain extends OptionComp[], T> = Chain["length"] extends 0
  ? T
  : Parameters<Chain[0]>[0];

// pipeOption must return wrap for the case of 0 arguments
const pipeOption =
  <Composables extends OptionComp[]>(
    ...composables: IsValidOptionChain<Composables> extends infer $Offset extends number
      ? ReplaceFromBack<Composables, $Offset, "INVALID_COMPOSABLE">
      : Composables
  ) =>
  // return a function
  <T>(
    firstData: ChainParamType<Composables, T>
  ): Option<ChainReturnType<Composables, T>> => {
    let data: any = firstData;
    // Override application operator for Option
    // inline the bind logic - enable shortcut return on None
    for (const composable of composables) {
      data = (composable as any)(data);
      if (isNone(data)) return data;
      else data = data.value;
    }
    return wrap<ChainReturnType<Composables, T>>(data);
  };

// pipeOption() without parameters is like wrap()
// This is the reason wrap() is often called the "unit" element of the monad
// it is the "neutral element" for monad composition.
// This is similar to what happened in the discussion of reduce((acc,item)=>acc+item, 0, [1,2,3])
const f = pipeOption();
console.log(`wrap 1 `, f(1));

const g = pipeOption((_: number): Option<string> => some("a"));
const h = pipeOption(
  (_: number): Option<string> => some("a"),
  (_: string): Option<number> => some(1),
  (_: number): Option<number> => some(1)
);
console.log(`h(1) -> `, h(1));

// ==============================
// Code without Option

// Call an API, get current user, get user pet, get pet nickname
// All the functions could return an "undefined" value when the object is missing.
type User = { name: string; pet: Pet | undefined };
type Pet = { nickName: string | undefined };

const getCurrentUser1 = (): User | undefined => ({
  name: "Michael",
  pet: { nickName: "doggy" },
});

const getPet1 = (user: User): Pet | undefined => user.pet;

const getNickName1 = (pet: Pet): string | undefined => pet.nickName;

// These functions are difficult to compose because they return
// a union of two incompatible values (true value or undefined).
// We need to add "guards" before passing the return value to another function.
// Lots of if statements...
// We would like to write:
// getNickName1(getPet1(getCurrentUser1()))
// or pipe(getCurrentUser1, getPet1, getNickName1)()
// but instead we need to write:
const getPetNickName1 = (): string | undefined => {
  const user: User | undefined = getCurrentUser1();
  if (user === undefined) return undefined;

  const userPet: Pet | undefined = getPet1(user);
  if (userPet === undefined) return undefined;

  const userPetNickName: string | undefined = getNickName1(userPet);
  if (userPetNickName === undefined) return undefined;
  return userPetNickName;
};

console.log(getPetNickName1());

// ==============================
// With Option

const getCurrentUser2 = (): Option<User> =>
  some({ name: "Michael", pet: { nickName: "doggy" } });

// No mention of "undefined" - no if
const getPetNickName2 = (): Option<string> => {
  const user: Option<User> = getCurrentUser2();
  const userPet: Option<Pet> = bind(user, (user: User) => wrap(user.pet));
  const userPetNickName = bind(userPet, (pet: Pet) => wrap(pet.nickName));
  return userPetNickName;
};

console.log(getPetNickName2());

// With "functional composition" with embedded calls to bind
const getPetNickName3 = (): Option<string> =>
  bind(getCurrentUser2(), (user: User) =>
    bind(wrap(user.pet), (pet: Pet) => wrap(pet.nickName))
  );

console.log(getPetNickName3());

// ==============================
// With "pipe functional composition"

// Operators that can be composed in a bind chain
// must have type T1 => Option<T2>
// They are "diagonal" from "normal types" to "option types".
// The resulting composition is also "diagonal".

const getPet2 = (user: User): Option<Pet> => wrap(user.pet);

const getNickName2 = (pet: Pet): Option<string> => wrap(pet.nickName);

// Can be composed as:
const getPetNickName4 = pipeOption(getCurrentUser2, getPet2, getNickName2);

console.log(getPetNickName4(undefined));
// => { value: 'doggy', tag: 'some' }

// ============================================================
// Summary of monad interface
//
// bind: apply transform f: [T1 => M[T2]] to monadic value M[t1]
// bind: M[T1] => (T1 => M[T2]) => M[T2]
// wrap: T => M[T]
// map: lift horizontal transform from values to monadic values: f: T1 => T2, map(f): M[T1] => M[T2]
// map: (T1 => T2) => (M[T1] => M[T2])
// chain: lift diagonal transform to lifted
// chain: (T1 => M[T2]) => (M[T1] => M[T2])
// fold: (M[T1], handleA: () => T2, handleB: (item, acc) => T2) => T2
// pipeM: (T1 => M[T2], T2 => M[T3], ..., Tn-1 => M[Tn]): [T1 => M[Tn]] - compose a sequence of diagonal operators into a diagonal operator.
