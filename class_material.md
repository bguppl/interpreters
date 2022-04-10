# Class Material

## Chapter 1: Practical Functional Programming

### Introduction

[1. Introduction](./class_material/chap1.html)

### Functional Programming: Expressions, Values, Types using TypeScript

[2. Semantics of Programming Languages and Types](./class_material/1.1TypescriptDatatypes.html)

[3. TypeScript: Complex Data Types, JSON, FP Processing of JSON](./class_material/1.2TypescriptJSON.html)

[4. Type Checking](./class_material/1.3TypescriptTypeChecking.html)

[5. Data Types and Operations on Data](./class_material/1.4TypesAndOperations.html)



## Chapter 2: Syntax and Semantics with Scheme

### Defining a Programming Language Bottom-up: Elements of Programming

[1. Elements of Programming - Defining Scheme Bottom-up](./class_material/2.1Scheme-bottom-up.html)

[2.Higher-order Functions in Scheme and Local Variables](./class_material/2.2Higher-Order-Functions-Let.html)

### Syntax

[3.Syntax of Programming Languages: BNF, Abstract Syntax Tree](./class_material/2.3Syntax.html)

[4.Syntactic Operations: L1 Parsing, Type Guards, Scoping, Lexical Addresses, Syntactic Rewrites](./class_material/2.4SyntacticOperations.html)


### Operational Semantics: Substitution-based Interpreter

[5. Operational Semantics: L1 evaluation, environment](./class_material/2.5OperationalSemantics.html)

[6. Substitution Model: L2 (ProcExp, IfExp), Closures, L3 (SExp), Error Processing, Applicative Order vs. Normal Order evaluation](./class_material/2.6SubstitutionModel.html)

### Operational Semantics: Environment-based Interpreter

[7. Environment Model, L4 evaluation, let evaluation, visual notation of environments, using closures, closure-based compound data structures](./class_material/2.7EnvironmentModel.html)

[8. Recursion and Mutation](./class_material/2.8RecursionMutation.md)



## Chapter 3: Type Checking and Type Inference

[1. Type Checking with Full Type Annotations](./class_material/3.1TypeChecking.html)

[2. Type Inference](./class_material/3.2TypeInference.html)

[3. Type Inference System](./class_material/3.3TypeInferenceSystem.html)


### Code

#### Type Checking (Code and Tests)
* [TExp.ts](https://github.com/bguppl/interpreters/blob/master/src/L5/TExp.ts): AST for the type language (to write type annotations)
* [L5-ast.ts](https://github.com/bguppl/interpreters/blob/master/src/L5/L5-ast.ts): AST for L5 with Type Annotations (same as L4-ast with type annotations)
* [L5-env.ts](https://github.com/bguppl/interpreters/blob/master/src/L5/L5-env.ts): Environment for L5 (similar to L4-env-box.ts)
* [L5-value.ts](https://github.com/bguppl/interpreters/blob/master/src/L5/L5-value.ts): Values definition for L5 (similar to L4-value-box.ts)
* [L5-eval.ts](https://github.com/bguppl/interpreters/blob/master/src/L5/L5-eval.ts): Interpreter for L5 with Type Annotations (similar to L4-eval-box.ts)
* [evalPrimitive.ts](https://github.com/bguppl/interpreters/blob/master/src/L5/evalPrimitive.ts): Primitive handling for L5 (same as L4 with type checking).
* [TEnv.ts](https://github.com/bguppl/interpreters/blob/master/src/L5/TEnv.ts): Type Environments
* [L5-typecheck.ts](https://github.com/bguppl/interpreters/blob/master/src/L5/L5-typecheck.ts): Type Checker for fully type annotated L5 programs

* [L5-eval.test.ts](https://github.com/bguppl/interpreters/blob/master/test/L5/L5-eval.test.ts): Tests for L5 interpreter
* [L5-typecheck.test.ts](https://github.com/bguppl/interpreters/blob/master/test/L5/L5-typecheck.test.ts): Tests for type checker

#### Type Inference with Type Equations
* [Substitution ADT](https://github.com/bguppl/interpreters/blob/master/src/L5/L5-substitution-adt.ts): make-sub, sub-apply, sub-combine
* [Type Equations Algorithm](https://github.com/bguppl/interpreters/blob/master/src/L5/L5-type-equations.ts): L5-exp->pool, pool->equations, solve-equations, infer

* [Substitution ADT Tests](https://github.com/bguppl/interpreters/blob/master/test/L5/L5-substitution-adt.test.ts)
* [Type Equations Tests](https://github.com/bguppl/interpreters/blob/master/test/L5/L5-type-equations.test.ts)

#### Type Inference with Type Var Unification (Optimized Type Equations)
* [Type Inference](https://github.com/bguppl/interpreters/blob/master/src/L5/L5-typeinference.ts): checkEqualType with unification, check-no-occurrence, typeofExp

* [Type Inference Tests](https://github.com/bguppl/interpreters/blob/master/test/L5/L5-typeinference.test.ts)



## Chapter 4: Control Structures

* [Asynchronous Programming, Promises and Generators](./class_material/4.1AsyncProgramming.html)
* [Continuation Passing Style](./class_material/4.2CPS.html)
* [Lazy Lists and Generators in Scheme](./class_material/4.3Generators.html)
* [From Recursion to Iteration in TypeScript through CPS](./class_material/4.4RecursionToIteration.html)

### Code

* [Asynchronous code examples in Node.js](https://github.com/bguppl/interpreters/blob/master/src/async/coroutine1.rkt): Promises and Generators
* [coroutine1.rkt](https://github.com/bguppl/interpreters/blob/master/src/async/coroutine1.rkt) - Coroutines and Generators in Racket
* [lzl.rkt](https://github.com/bguppl/interpreters/blob/master/src/async/lzl.rkt) - Lazy-lists in Racket

#### Deriving an iterative interpreter for L5 using CPS and Registerization

The following code relies on the files of the L5 interpreter from the previous chapter. Add the files to the same folder.

* [sum.ts](https://github.com/bguppl/interpreters/blob/master/src/cps/sum.ts): Turning a recursive TypeScript function into iterative code in JavaScript - general method (CPS, concrete data structures for continuations, registerization, iteration).

* [L6-eval.ts](https://github.com/bguppl/interpreters/blob/master/src/cps/L6-eval.ts): A CPS version of the L5 interpreter (same AST as L5).
* [L6-eval.tests.ts](https://github.com/bguppl/interpreters/blob/master/test/cps/L6-eval.test.ts): Same tests as L5 using the L6 version.

* [L7a-eval.ts](https://github.com/bguppl/interpreters/blob/master/src/cps/L7a-eval.ts): L6 CPS interpreter with explicit continuation ADT
* [L7b-eval.ts](https://github.com/bguppl/interpreters/blob/master/src/cps/L7b-eval.ts): L6 CPS interpreter with concrete continuation data structures
* [L7c-eval.ts](https://github.com/bguppl/interpreters/blob/master/src/cps/L7c-eval.ts): L6 CPS interpreter with registers and iterative interpreter.
* [L7-tests.ts](https://github.com/bguppl/interpreters/blob/master/test/cps/L7.test.ts): Tests for L7c interpreter demonstrating that tail-recursive code is executed iteratively.



## Chapter 5: Logic Programming

* [Relational Logic Programming](./class_material/5.1RelationalLogicProgramming.html)
* [Logic Programming](./class_material/5.2LogicProgramming.html)
* [Logic Programming Interpreter in Scheme](./class_material/5.3LPInterpreter.html)

### Code

To install the full package of the Logic Programming system in Racket:

Clone the code from [github](https://github.com/bguppl/interpreters/tree/master/src/logic)
* Unzip it in a folder /ppl/src/logic
* Launch DrRacket
* Load the file "answer-query-test.rkt"
* Run the tests - it should print
```
6 tests passed.
5 tests passed.
9 tests passed.
```

* [LP-ast.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/LP-ast.rkt): AST for Logic Programming
* [LP-ast-tests.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/LP-ast-tests.rkt): AST tests and examples
* [LP-rename.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/LP-rename.rkt): variable renaming
* [substitution-ADT.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/substitution-ADT.rkt): Substitution ADT
* [substitution-adt-tests.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/substitution-ADT-tests.rkt): Substitution tests
* [lazy-tree-ADT.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/lazy-tree-ADT.rkt): Lazy tree ADT
* [lazy-tree-ADT-tests.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/lazy-tree-ADT-tests.rkt): lazy tree tests
* [auxiliary.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/auxiliary.rkt): auxiliary functions
* [utils.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/utils.rkt): Utility functions
* [unify.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/unify.rkt): unification algorithm
* [term-equation-adt.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/term-equation-adt.rkt): term equation ADT (pair of terms)
* [unify-tests.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/unify-tests.rkt): unification tests
* [answer-query.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/answer-query.rkt): answer-query algorithm
* [answer-query-test.rkt](https://github.com/bguppl/interpreters/blob/master/src/logic/answer-query-test.rkt): answer query tests



