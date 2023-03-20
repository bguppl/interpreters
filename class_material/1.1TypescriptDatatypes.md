# Semantics of Programming Languages and Types
### PPL 2023 

This lecture introduces notion of **programming language semantics** and the role of **types** in programming languages in the context of Functional Programming.

It leads to the definition of the following concepts:

1. Operational semantics of programming languages
  * High-level definition of programming language semantics
  * Distinguish expressions and values in programming languages
  * Define expression types: atomic vs. compound expressions
  * Define the evaluation algorithm to map expressions to values
2. Types in programming languages
  * Define value types: atomic vs compound values
  * Define typing errors and type safety
  * Distinguish untyped and typed programming languages
  * Define static and dynamic type checking
3. Types with TypeScript 
  * Illustrate type checking in TypeScript
  * Illustrate the benefits of gradual typing with TypeScript
  * Review the type language of TypeScript:
    * Primitive types vs. Defined types
    * Atomic types vs. Compound types

In the next lecture, we will go deeper into complex data types in TypeScript and review:
* Compound values, literal constants and serialized values with JSON
* Recursive types
* Generic types
* Function types

## Operational Semantics of Programming Languages

This section introduces very briefly the notion of *operational semantics for programming languages*.
We will refine this definition in Chapter 2.  This definition will help us define more precisely what
is meant by *data types*.

### High-level definition of programming languages semantics

Semantics is the study of the meaning of languages. The word *meaning* is complex and we must specify what is meant by *meaning of a programming language*.  We adopt here the presentation offered in Chapter 1 of:

[**Semantics with Applications: A Formal Introduction**](http://www.cs.kun.nl/~hubbers/courses/sc_1718/materiaal/wiley.pdf)
_Hanne Riis Nielson, Flemming Nielson_
Wiley Professional Computing, Wiley, Second Edition 1999.


A computer language is specified by two main components:
* a (formal) syntax – describing the structure of programs as they are written by programmers 
* a semantics – which describes the intended meaning of programs.

Various tools manipulate programs (compilers, interpreters, debuggers, IDEs, verification tools).
All of these tools must agree upon a formal specification of what is expected when we execute the program.
The formal semantics of a programming language provides an unambiguous definition of what the execution should achieve.

This formal definition has several uses - among which:
* People learning the language can understand what each of the programming language construct is expected to achieve. 
* Implementers of tools for the language (parsers, compilers, interpreters, debuggers etc) have a formal reference for their tool and a formal definition of its correctness/completeness
* 2 different programs in the same language or in different languages can be proved formally as equivalent/non-equivalent

Proving that programs are equivalent is necessary for example to prove that a compiler generates assembly code which is equivalent to the source program.
Naturally, we want to be able to prove that this equivalence holds for any possible source program. 

#### Semantic Domain

Formal semantics gives rules for translation from one domain (usually the program’s syntax) to another formally defined domain.  

We adopt the **Operational Semantics** approach which determines that:
the meaning of an expression in the programming language is specified by the computation it induces when it is executed on a machine.  The operational semantics of a programming language tells us how to execute the program step by step.
When we follow this specification, we record the steps and keep track of the **state** of the computation as steps are executed.
The specification determines what is meant by the state and which parts of the state are updated when 

Take as example an imperative programming language with global variables and assignment to variables.
For this language, the semantics specifies that the state of an execution consists of the list of defined variables and their value at each step.

The result of the execution of a program according to the semantic specification is a **derivation sequence** which represents the computation history.  Each step in the derivation indicates which part of the program was executed, and the state that was reached as a consequence.

In this example, a program is a sequence of assignments:
```
Initial state: [x:5, y:3, z:7]
Program: "z = x; x = y; y = z;"

Step 1: execute "z=x;" 
    Remaining program: "x=y; y=z;"
    New state: [x:5; y:3, z:5]
Step 2: execute "x=y;"
    Remaining program: "y=z;"
    New state: [x:3, y:3, z:5]
Step 3: execute "y=z;"
    Remaining program: ""
    New state: [x:3, y:5, z:5]
```

This result can be understood as a representation of the *history of the computation* - it traces the steps of the execution into *primitive steps*
and the successive states of the computation. 

This view is an **abstraction** - it does not provide all the details of what should happen in a concrete computation on specific hardware. 
For example, it does not mention registers, translation to machine language, encoding of data types.
This computation history is a formal mathematical object which is in the **semantic domain**.

The rules of the semantics of the specific programming language indicate how to select a sub-expression to evaluate at each
step of the computation, and what are the effects on the state of the computation each time a primitive sub-expression is 
executed.

In summary, the operational semantics of the language maps a program and an initial state to a formal structure -
in this case, we informally refer to this structure as a *computation history*.

## Statements vs. Expressions vs. Values

In **imperative programs**, programs are constructed from **statements**.  A statement is a unit of program execution.
* **Atomic statements** such as variable assignment or print-statements have a single effect either on the world (screen is updated, disk is updated, information is sent in the network) or on the internal state of the program (the memory owned by the process is updated).
* **Compound statements** are built from multiple sub-statements.  For example, an `if-then-else` construct is used to
build a compound statement, or a `for-loop` construct.  
When a statement is executed, the state of the program is modified.

In contrast, in **Funcional Programming** (FP), programs are **expressions**.  Expressions can either be:
* **Atomic expressions** - for example, the number expression `-12` or the boolean expression `true` or the variable expression `x`.
* **Compound expressions** - which are made up of sub-expressions according to the syntax of the language. For example the expression `12 >= 7` is a compound expression made up of 3 sub-expressions. `(12 === 13) ? -1 : 2` is also a compound expression (called a conditional expression).  

In FP, we do not 'execute an expression', but instead we compute its value - a process we call **evaluation**.
The *evaluation function* maps expressions to **values**.  

The operational semantics of an FP language describes how the evaluation function operates over all possible expressions in the language.  It is defined inductively over the syntactic structure of expressions.

In the rest of the course, we will focus on specifying the operational semantics of a functional language.

### Types of Expressions

The JavaScript language mixes both expressions and statements - it is a multi-paradigm language.
This makes it difficult to describe completely its operational semantics in a concise manner.
We will completely define a smaller language in Chapter 2 - here, we only provide an informal description for JavaScript.

Intuitively, the types of expressions we consider are:
* **Atomic expressions**: number expressions (`1, -12`), string expressions (`"abc"`), boolean expressions (`true, false`).
* **Compound expressions**: arithmetic expressions `(10 + v) * (w / 5)`, relational expressions (`x === 5`), ternary conditional expression `<condition> ? <expr1> : <expr2>`. 
* **Variable bindings**: these are expressions of the form `let <var> = <expr1>; <expr2>` which define a block in which the variable `<var>` is defined and bound to the value of `<expr1>` and then evaluate the value of `<expr2>`.
* **Function invocation**: of the form `f(<expr1>,...,<exprn>)` 
* **Function definition**: of the form `(<var1>,...) => <expr>`.

The list of these expression types and their structure defines the *syntax of the programming language*.
We intentionally did not include in this partial description the syntax of *statements* in JavaScript.

Expressions are combined recursively to form trees of compound expressions with atomic expressions at the leaves. 
For example:
```
let v = 12;
  (v >= 7) ? (v * 3) : 9
```
is a compound expression of type `let`, with 2 sub-expressions - a binding expression `v=12` and a body expression which
is a conditional ternary expression; in turn, this sub-expression has 3 sub-expressions.

### Types of Values

Once an expression is evaluated by applying the *evaluation function*, it becomes a **value**.
The *evaluation function* maps expressions to values.

As we can distinguish types of expressions, we can also characterize the set of all possible values in an inductive manner - that is, start from **atomic values** and then build up **compound values** which are made up of smaller value parts.

This lecture focuses on the characterization of the **domain of values** in JavaScript.

## Evaluation of Expressions into Values

The operational semantics of an FP language defines the evaluation function.  It determines how a top-level expression (the program) is evaluated into a value.  This definition is inductive. For each syntactic construct, an evaluation rule indicates how the sub-expressions are evaluated - in which order.  At each step of the computation, a sub-expression is evaluated and then substituted by its value in the embedding expression.  

There may be a need to describe the state of the computation during this evaluation process - similar to the set of bindings `var:value` we observed above when describing the operational semantics of a very simple imperative language.  But the operations on this state will be quite different from what we observed, as in pure FP languages there is no need to perform variable assignment (because variables are immutable).  

The output of the operational semantics is a computation history - but in this chapter we will not describe the computation history - we will only describe the values produced by the evaluation function.

### Evaluation Algorithm

Consider the complex expression we presented above:
```
let v = 12;
  (v >= 7) ? (v * 3) : 9
```
The evaluation algorithm consists of the following steps: given an expression $$E$$
1. Identify the toplevel syntactic construct of $$E$$ - in our case a `let` construct.
2. Identify the immediate sub-expressions of $$E$$ - in our case - the binding construct `v=12` and the body (rest of $$E$$).
3. Perform the specific *evaluation rule* defined for the construct of $$E$$ - in this case - the *let-evaluation rule* is:
   1. Compute the value of the `<expr>` on the right-hand-side of the binding expression - call it `v1`.
   2. Add to the state of the computation the binding `{v:v1}`
   3. Compute the value of the body sub-expression - call it `v2`.
   4. Remove the binding `{v:v1}` from the state of the computation.
   5. The value of the overall `let` construct is `v2`.

This definition of the evaluation function is **recursive**: in steps 3.A and 3.C - we invoked the evaluation function recursively on sub-expressions of $$E$$.

The evaluation function refers to a *state* which describes the set of known variables at every single step of the computation, and the values to which they are bound.

This recursive definition must have a base case to terminate. The base case of the recursion consists of evaluating 
**atomic expressions** or to invoke **atomic primitive operators or functions**.  For example:
* The expression `v` is an atomic expression (it has no sub-expressions); it is an expression of type *variable*.
The computation rule for evaluating a variable is to lookup its value in the current state of the computation.
In our case, the value of the expression `v` is `12`.
* The expression `7` is an atomic expression of type `number`; The computation rule for evaluating a number is to convert the way numbers are written in the language into a number value.
* The expression `(v >= 7)` is a compound expression of type `relational operator` with a primitive atomic operator (`>=`); The computation rule for evaluating a relational operator is to (1) evaluate the left side sub-expression (we obtain `12`); (2) evaluate the right-side sub-expression (we obtain `7`); (3) pass the 2 values to the primitive comparison operator which must return a boolean value (in this case `true`).

#### Reduction of Expressions

A useful way to think about the evaluation of complex expressions is in terms of **reduction**.

Consider the following example (adopted from [PAPL Section 5.6](http://papl.cs.brown.edu/2020/basic-data-expr.html)):

We want to evaluate the following expression, which is a formula for computing the wage of a worker given the number of hours she worked:

```typescript
let hours=45;
  (hours <= 40) ? hours * 10 :
  (hours > 40) ? (40 * 10) + ((hours - 40) * 15) :
  0
```

This formula in a more explicit form states that:

```typescript
if (hours <= 40)
      hours * 10  // 10$ per hour for less than 40 hours a week
   else if (hours > 40)
      (40 * 10) + ((hours - 40) * 15) // 10$/hour up to 40hrs - 15$ above
   else
       0  // This should not happen.
```


```typescript
let hours=45;
  (hours <= 40) ? hours * 10 :
  (hours > 40) ? (40 * 10) + ((hours - 40) * 15) :
  0
```

Let’s now see how this expression is reduced to a value, using a step-by-step process:

The first step is to **substitute the `hours` variable** with 45:

```typescript
  (45 <= 40) ? 45 * 10 :
  (45 > 40) ? (40 * 10) + ((45 - 40) * 15) :
  0
```

Next, the conditional part of the `?` expression is evaluated, which in this case is false.

```typescript
=>  false ? 45 * 10 :
    (45 > 40): (40 * 10) + ((45 - 40) * 15) :
    0
```

Since the condition is false, the next branch is tried. Note that at each step, a sub-expression is evaluated and replaced by its value:

```typescript
=>  false ? 45 * 10 :
    true: (40 * 10) + ((45 - 40) * 15) :
    0
```

Since the condition is true, the expression reduces to the body of that branch. After that, it’s just arithmetic (but each step is done in a specific order):
```typescript
=>  (40 * 10) + ((45 - 40) * 15)
=>  400 + ((45 - 40) * 15)
=>  400 + (5 * 15)
=>  400 + 75
=>  475
```

This style of reduction is the best way to think about the evaluation of complex expressions.
It is called a process of **reduction** - because the complex expression is incrementally reduced into a value.

## Types in Programming Languages

In the same way as **expressions** can be atomic or compound and built-up into complex recursive programs,
the **values** that are generated by programs can be:
* **atomic values**: values which have no "sub-components" - such as numbers or booleans.
* **compound values**: values which are constructed from sub-values, such as arrays or structures.

Compound values can be created in the following manner:
* Specified as **constant literal values** in the program; for example the expression `[1,2,3]` denotes in JavaScript an array value containing 3 number components.
* Computed by **primitive constructor** functions - for example, the expression `[1].concat([12])` returns a new array value containing 2 number components `[1,12]`.
* **Deserialized from strings** that represent compound values according to a value syntax (we will see in the next lecture the example of JSON).

**NOTE 1**: the capability to specify compound values as literal constants is a characteristic of dynamic languages such as JavaScript, Scheme or Python.  
It is more difficult to *write down compound values* in languages such as Java or C++.
We will return to this important distinction later.

**NOTE 2**: the distinction that a value is atomic depends on a language design decision: for example, strings in JavaScript are defined to be immutable atomic values.  
This is because there is no accessor that can take apart a string into characters and mutate one of these parts in place.  
In contrast, in C and C++, strings are mutable compound (non-atomic) values (arrays of characters). 
There are functions in JavaScript that can compute a sub-string of a string, but these are string constructors - they compute a new string given another string as input, not accessors which give access to a part of an existing compound string.


### Typed vs. Untyped Languages

As you have noted, JavaScript is an **untyped language** - this means: when we declare variables (using the `let` construct)
or functions (with the `function` or `=>` constructs), we do not specify the type of the variables or parameters.
We also do not need to specify the type returned by the function.

This is in contrast to **typed languages** such as Java and C++ - which require the programmer to specify the type
of all variables before they can be defined or used.

Yet - even if **variables** are not declared with a type in Javascript, **values** in JavaScript **do have** a type -- they can be either `number` or `string` or `boolean` or compound values.

Why would we want or not want to declare the type of variables? 

What are the benefits of each of the approaches?


### Typing Errors at Runtime

In the description of the evaluation function above, we indicated that expressions are traversed recursively, til 
atomic expressions are evaluated (into numbers, booleans or strings) and primitive operators or functions are invoked
(for example `(v >= 7)`).  

When primitive operators or functions are invoked - we can obtain errors at runtime because the value which is passed to the operator does not fit the type of the primitive operator.  
For example, in most languages, the evaluation of `(7 >= "a")` or `(8 + "b")`would return an error at runtime because the 
operators `">="` and `+` do not know how to operate on a mixture of numbers and strings.
This will be the case in Scheme for example as we will see in Chapter 2.

#### JavaScript Primitives do not Fail

In JavaScript, the language designers took a different approach: they made the primitive operators extremely flexible and robust.  So that evaluations of *strange expressions* do not trigger a runtime error - but instead do either automatic conversions or return special values indicating an *impossible value*.

For example:


```typescript
"a" > 2
// => false
```

```typescript
2 + "ab"
// =>    '2ab'
```
```typescript
"a" * 2
// =>    NaN
```
`NaN` is a special value which means *"Not a Number"*.

```typescript
"a" && true
// =>     true
```

Non-boolean values are interpreted as boolean when they appear as arguments to boolean operators such as `&&` and `||`.
Arithmetic operations are defined over strings and numbers.

So that overall - primitive operations in JavaScript do not fail easily.

This is a dubious decision - as such automatic handling of unexpected variations is most often a sign of poorly written code
and produces *surprising* results.

#### Accessors to Compound Values in JavaScript do not Fail

Another decision of the JavaScript designers was to make access to compound values "robust" against runtime errors as well:
* Accessing an index out of bounds in an array returns the special value `undefined`
* Accessing an undefined key in a map returns the special value `undefined` as well.


```typescript
let arr=[1];
    arr[4]
// =>    undefined
```

```typescript
let map={a:1, b:2}
    map.c
// =>  undefined
```

#### Undefined Variables in JavaScript do Fail

Variables access, however, **can fail**: trying to access an undefined variable raises an error at runtime.
When variable expressions are evaluated, it may turn out that the variable is not defined in the current state of the program at the time its value is looked up.  

For example:

```typescript
let b=2;
    c; // Trigger a reference error
// => ReferenceError: c is not defined
```

Similarly, trying to access a key from an `undefined` value triggers an error at runtime:

```typescript
e.k 
// => ReferenceError: e is not defined
```

## Type Safety

Now that we have seen which errors can be triggered when executing a JavaScript program - and which *surprising* results may be returned when we combine unexpected value types in primitive operators invocations - we return to our question:
* Can we avoid such errors and surprises?
* What are the benefits of declaring the types of variables?

**Type safety** (see [Wikipedia Type Safety](https://en.wikipedia.org/wiki/Type_safety)) is *the extent to which a programming language discourages or prevents type errors. A type error is erroneous or undesirable program behaviour caused by a discrepancy between differing data types for the program's constants, variables, and methods (functions), e.g., treating an integer (int) as a floating-point number (float).*

Type enforcement can be either:
* **static**, catching potential errors at compile time, 
* or **dynamic**, associating type information with values at run-time and consulting them as needed to detect imminent errors,
* or a combination of both.

In the context of static (compile-time) type systems, type safety usually involves a **guarantee that the eventual value of any expression will be a legitimate member of that expression's static type**.

We will develop the notion of type safety incrementally, after we discuss what are types.

## Types of Values

A data type is a classification of data which indicates what the programmer intends to do with the data. 
Data types are defined along 2 aspects:
* Types correspond to **sets of values** - for example, the type `Boolean` is the set of values {`true`, `false`};
  the type `Number` is the (possibly infinite) set of numeric values.
* Types define the **operations that can be done on data values** in the set.  For example, values of type `Boolean` can be
  computed using logical operators such as `and`, `or`; `Number` values can be manipulated with operators `+`, `*` and
  compared with operators such as `<`, `>`.
  
Types also determine how the interpreter reads and writes the values, and how it stores the values in memory. 

When discussing types, we must distinguish the **type of a value** and the **type of a variable**:
* **Values** always have a type - that is, they belong to a specific set of values. The number `3` belongs to the set of `number` values; the value `true` belongs to the set of `boolean` values.
* **Variables** are parts of the programming language expressions. They are bound to values as the expressions are computed at runtime.  When a programmer declares that a *variable has a type* - the meaning is that we express the intent of the programmer that this variable can **only be bound to values of this type** during the lifecycle of the variable. It states a constraint on the variable usage.  

In most typed languages, this constraint can be checked at compile time: this is an extremely strong result.  It means that the compiler can at compile time guarantee that all possible executions of the program, with all possible input values will satisfy the stated constraint.  This static verification (that is, a verification performed at compile-time - without knowing the value of the variables) is called **type checking**.

## Value Types

As mentioned, a value always has a type - this simply states that a value belongs to a set of values.
For example, the value `1` belongs to the set of values `number` - which means that the value `1` has type `number`.

In most cases, a value will have more than one type: consider for example the case of *sub-types*
such as `Integer` and `Number`. `Integer` as a set of values is a proper subset of `Number` as a set of values.
This means in particular that a value like `6` belongs both to the type `Integer` and to the type `Number`.

A good type system helps programmers not only declare the intended types of variables to obtain type safety,
it also helps the programmer **design the range of values** the program will process and produce.
This is a constructive process which helps structure the set of all possible values in meaningful ways
and also document the domain upon which programs operate.  

As we write more complex programs over more complex data structures - we will feel more strongly the benefit 
of describing precise data types - which will also guide the structure of the code we write to operate over them.


### Types and Set Relations

Since types denote set of values, we can define relations among types that are similar to set relations:
* Type T1 can be a subset of type T2: this means any value of type T1 is also a value of type T2.  We also say that T1 is a subtype of T2.
* Types T1 and T2 can be disjoint - meaning there are no values that are both of type T1 and T2.  For example, 
`number` and `boolean` are disjoint. 
* One can define a universal type - which is the type of all possible values.  (This type is exists in TypeScript and is called `any`).
* Some types denote finite sets (`boolean` has 2 values), others are infinite (`number`, `string`). 
* One can construct new types on the basis of existing types: 
  * The union of T1 and T2 contains all the values of T1 and T2 together.
  * The intersection of T1 and T2.
  * The cartesian product of T1 and T2 would be the type that contains pairs of values (v1, v2) such that v1 is of type T1 and v2 is of type T2.
  
This last construction opens the door to compound data types - that is, types which contains values that have sub-components.

We will see later that most typed programming languages allow programmers to define new types (user-defined types) on the basis of primitive types (types provided by the language) by using set theory operations like Union, Intersection or Cartesian Product.

Different languages offer various levels of introspection (often called *reflection*) to enable the analysis of
the type of values *at runtime*, or *at interpretation/compile time*. 

In JavaScript (the underlying language into which TypeScript is translated), variables are not typed, values have a type (which is encoded in the binary representation of the values in memory) and the `typeof` primitive operator can be used to inspect the type of values at runtime.  Primitives in JavaScript use this introspection mechanism to decide how to operate on each combination of value types they receive as parameters.

In contrast, in Java and C/C++, primitive values (non-object values such as integers, booleans, characters, pointers and references) cannot be inspected at runtime: values of different types (for example a char, a boolean or an integer) are encoded in binary format in RAM without any distinctive sign that could be used at runtime to determine that the value belongs to different types.

## Types in TypeScript

We now describe the basic data types manipulated in TypeScript.
The online reference is available in the [TypeScript Tutorial](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html): 

TypeScript is a language built on top of JavaScript: it adds optional **type declarations** to variables (which do not exist 
in JavaScript). The TypeScript compiler (invoked by the command line tool `tsc`) translates TypeScript into JavaScript.
During the translation, the TypeScript compiler performs **type checking** - which we will explain later.


### Atomic Primitive Value Types

Simple values can have the following primitive data types in TypeScript:
* Boolean: `true`, `false`;
* Number: floating point numbers; `6, 6.1, -4, 0xf0a` (hexadecimal notation)
* String: immutable sequences of characters, `"this is a string"`.


```typescript
console.log(typeof(6))
console.log(typeof(true))
console.log(typeof("a"))
// =>     number
//        boolean
//        string
```

There are two *special* value types that are not useful on their own - but will play an important role when we start
building more complex types:
* **null**: this is a type that contains only the special value `null`
* **undefined**: this is a type that contains only the special value `undefined`

`undefined` plays a special role in the lifecycle of variables.  We will return to this when we discuss the types of
variables (as opposed to the type of values which are we now reviewing).


```typescript
let null1 = null;
let undef = undefined;

console.log(typeof(null1));
console.log(typeof(undef));

// =>     object
//        undefined
```

*NOTE*:
*the value of the expression `typeof null` returns `object` - this is a well-known error in JavaScript that
has not yet been cleaned up - `null` should still be understood as a distinct atomic primitive value type - not as a compound type.*

## Compound Value Types

In contrast to atomic values, one can also define *compound values* - which are values that are built from multiple parts.
There are two basic compound types in TypeScript:
* Arrays
* Maps

Values of arrays are written as literal constants of the form `[1, 2, 3]`.

Maps are written as literal constants of the form: `{ a : 1, b : 2 }`

Any value that is not an atomic value is a *compound value* - which is called in Javascript an *object* value.

*NOTE*:
The term of **object** is confusing in this context - it does not refer to *objects* as used in the more standard 
context of *object oriented programming*.  *Object* in this context means any value which is *not* an atomic value type.


```typescript
let arr1 = [1, 2, 3],
    arr2 = ['a', 'b', 'c'];
  console.log(typeof(arr1));
  console.log(typeof(arr2));

// =>     object
//       object
```

```typescript
let map1 = { a : 1, b : 2},
    map2 = { 'a' : 1, 'b' : 2};
  console.log(typeof(map1));
  console.log(typeof(map2));

// =>     object
//       object
```

*NOTE:*
Notice that the primitive `typeof` operator is limited in its capability to distinguish the types of values, even if they are 
very different.  We will see later that TypeScript improves on this operator tremendously.

More specific reflection operators allow finer distinction in the types of values, for example:


```typescript
let arrIns = [1, 2],
    mapIns = { a:1 }
  console.log(arrIns instanceof Array);
  console.log(mapIns instanceof Array);

// =>    true
//       false
```

Compound values can be entered as constant literals (as above) or can be constructed by invoking the appropriate constructors
and mutators (functions which incrementally change a value).

For example:


```typescript
let arrConstructed = Array(1,2,3)
  arrConstructed
// =>     [ 1, 2, 3 ]
```

```typescript
let mapConstructed = {}
  mapConstructed.a = 1
  mapConstructed.b = 2
  mapConstructed
// =>  { a: 1, b: 2 }
```

### Compound Value Getters

Compound values can be "taken apart" by using getters - for arrays, using indexes which refer to the positions of the items
within the array, and for maps using key values.  In addition, the `slice()` method of arrays can return a range of values from within the array:


```typescript
let arrInd = ["a", "b", "c", "d", "e"]
    console.log(arrInd[0])          // Array indexes start at 0
    console.log(arrInd[1])
    console.log(arrInd.slice(1))    // Extract from item 1 to the end.
    console.log(arrInd.slice(1,4))  // Extract from item 1 to 4 - 4 not included.

// =>     a
//    b
//    [ 'b', 'c', 'd', 'e' ]
//    [ 'b', 'c', 'd' ]
```

Maps are collections of pairs (key, value).

Map getters use the key to access the value.

Alternatively, the dot notation `m.k` can be used when the key is a string.


```typescript
let mapInd = { a : 1, b : 2 }   // Note that keys which are strings in key position can skip the quotes - this is the same as { "a" : 1, "b" : 2}
    console.log(mapInd['a'])
    console.log(mapInd.a)
    console.log(mapInd['b'])
    console.log(mapInd.b)

// =>     1
//    1
//    2
//    2
```

The method `Object.keys(x)` returns the list of the keys which are defined for any compound values.
It operates both on arrays (the keys are the indexes) and on maps.

```typescript
let mapKey = { a : 1, b : 2},
    arrKey = ['a', 'b', 'c'];
  console.log(Object.keys(mapKey));
  console.log(Object.keys(arrKey));

// =>    [ 'a', 'b' ]
//    [ '0', '1', '2' ]
```
Alternatively, one can use the generalized `for` loops over arrays and maps:


```typescript
// The backquote notation can be used to generate "interpolated strings"
// The notation ${expression} in such a string is replaced by the value of the expression converted to a string.
for (let k in mapKey) {
  console.log(`${k} has value ${mapKey[k]}`);  
}
for (let i in arrKey) {
  console.log(`${i} has value ${arrKey[i]}`);
}
// =>
//    a has value 1
//    b has value 2
//    0 has value a
//    1 has value b
//    2 has value c
```

## Variable Types in TypeScript: Gradual Typing

In *static languages* like C++ and Java, variables must be typed - that is, when a variable is defined, its type must be declared by the programmer. 

In *dynamically typed languages* like JavaScript and Scheme, variables are not typed.  
But when a variable is bound to a value, we can inspect its type at runtime (because values are always typed).

TypeScript extends JavaScript and introduces *optional variable types*.
TypeScript is compiled into JavaScript - and at compilation time, type checking is performed.
There is no additional type checking happening at runtime after compilation has completed.

Typing annotations can be introduced gradually as code matures.
(See [Wikipedia Gradual Typing](https://en.wikipedia.org/wiki/Gradual_typing)).

Typing a variable means that the programmer declares how she intends to use the variable - which values it can be bound to.
In TypeScript, this is performed by adding a type annotation to variable declarations:


```typescript
let typedVarNum  : number = 6,
    typedVarStr  : string = "blue",
    typedVarBool : boolean = true;
```


## Summary

### Concepts
* Programming Language *semantics* defines the requirements for tools that manipulate programs: interpreters, compilers, debuggers etc.  It helps determine whether 2 programs are equivalent.
* The *Operational Semantics* of programming language defines the meaning of programs by mapping programs to evaluation histories. 
* *Operational semantics* provide recursive *evaluation rules* for each specific syntactic construct in a programming language.

* *Expressions* in an FP language are computed into *values*. This process can be described as *reduction*.

* *Runtime errors* can be triggered when parameters of the wrong type are passed to primitives (in Scheme) or when undefined variables are accessed.
* *Type safety* guarantees at compile time that a well-typed program will not lead to type runtime errors. It requires that the compiler determines the type of variables - which means to which types of values the variable can ever be bound.

* *Data types* are defined by 2 aspects: they are *sets of values* and they determine which *operations can be used on values*.
* *Values* can be *atomic* (no sub-part) or *compound* (made up of multiple parts).

* TypeScript adds the option of *gradual typing* on top of JavaScript
* *Primitive atomic types* in TypeScript are *number*, *boolean*, *string*, *undefined* and *null*.
* *Primitive compound types* in TypeScript are *array* and *map*.

### Compound Types Operations
* Literal constants for arrays are written as `[1, 2, 3]`
* Literal constants for maps are written as `{ k1 : 1, k2 : 2}`
* Parts of an array are accessed using number indexes starting with 0: `arr[0], arr[1]`
* Parts of a map are accessed using key indexes: `map[k1]` or the dot notation `map.k1`
* The list of indexes that can be used to access parts of a compound values is returned by `Object.keys(value)`
  or using the construct `for (let k in object) { ... }`.


