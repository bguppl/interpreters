# TypeScript: Complex Data Types, JSON, Map, Filter, Reduce

## Solutions to Practical Session Exercises - Week #1

## Exercise 1: Implementing `map` with `reduce`

```typescript
const mapExercise = (f, elements) =>
    elements.reduce((acc, curr) => { // reducer
        acc.push(f(curr));
        return acc;
    }, []);

console.log(mapExercise(x => x * x, [0, 1, 2, 3, 4, 5, 6])); // ==> [ 0, 1, 4, 9, 16, 25, 36 ]
```

**Note**: this version uses mutation in the reducer (`acc.push(...)`).

Rewrite this to avoid mutation.

## Exercise 2: Implementing `filter` with `reduce`

```typescript
function filterExercise(pred, elements) {
    return elements.reduce((acc, curr) => {
        if (pred(curr)) {
            acc.push(curr);
        }
        return acc;
    }, []);
}

console.log(filterExercise(x => x % 2 === 0, [0, 1, 2, 3, 4, 5, 6])); // ==> [ 0, 2, 4, 6 ]

// Using the more compact => notation and the ternary ( test ? then : else ) expression
const filter2 = (pred, elements) =>
    elements.reduce((acc, cur) => (pred(cur) ? acc.concat(cur) : acc), []);

filter2(x => x % 2 === 0, [0, 1, 2, 3, 4]); // ==> [ 0, 2, 4 ]

// Using the Ramda reduce operator instead of the array method elements.reduce()
// With TypeScript type declarations - note the usage of Array<T>() instead of [] to denote an empty array of type T[]
import { reduce } from "ramda";
const filter3 = <T>(pred: (x: T) => boolean, elements: T[]) =>
    reduce(
        (acc, cur) => (pred(cur) ? acc.concat(cur) : acc),
        Array<T>(),
        elements
    );

console.log(filter3(x => x % 2 === 0, [0, 1, 2, 3, 4])); // ==> [ 0, 2, 4 ]
```

**Note**: Implement `filter` using `chain` and without mutation (no `push()`).

## Exercise 3: Implementing `some` and `every` with `map` and `reduce`

```typescript
const someExercise = (pred,arr) => arr.map(pred).reduce((acc,curr) => acc || curr, false);
const even = (x) => x % 2 === 0;
const arr1 = [0, 1, 2, 3];
const arr2 = [1, 3];

console.log(`arr1HasEvenNumbers = ${someExercise(even, arr1)}`); // ==> arr1HasEvenNumbers = true
console.log(`arr2HasEvenNumbers = ${someExercise(even, arr2)}`); // ==> arr2HasEvenNumbers = false

function everyExercise(pred, arr) {
    return arr.map(pred).reduce((acc, curr) => {
        return acc && curr;
    }, true);
}

console.log(`allInArr1AreEven = ${everyExercise(even, arr1)}`); // ==> allInArr1AreEven = false
console.log(`allInArr3AreEven = ${everyExercise(even, arr3)}`); // ==> allInArr3AreEven = true

// Every with ramda map and reduce and TypeScript types.
import { map, reduce } from "ramda";

const even = (x: number) => x % 2 === 0;
const arr1 = [0, 1, 2, 3];
const arr2 = [0, 2];

// Note the requirement to cast 'true' as boolean
const everyExercise2 = <T>(pred: (x: T) => boolean, arr: T[]) =>
    reduce(
        (acc: boolean, cur: boolean) => acc && cur,
        <boolean>true,
        map(pred, arr)
    );

console.log(`allInArr1AreEven = ${everyExercise2(even, arr1)}`); // ==> allInArr1AreEven = false
console.log(`allInArr2AreEven = ${everyExercise2(even, arr2)}`); // ==> allInArr2AreEven = true
```
