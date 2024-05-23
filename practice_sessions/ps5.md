<h1><b>Operational Semantics</b></h1>

<h2><b>Practical Session - Week #5</b></h2>

The operational semantics of a programming language is specified by a set of formal evaluation rules that operate on the AST of an expression. 
The evaluation process can be specified as an algorithm ***eval(exp) : [AST -> Value]***.

In this session, we review:

* **What is an Environment**

* **Handling Primitives in the Operational Semantics**

* **Renaming and Substitute**

* **applicative-eval *vs* normal-eval**

* **letrec**



<h3><b>What is an Environment?</b></h3>

An environment represents a partial function (as opposed to a total function) from symbols (variable names) to values.
It supports the operation ***apply-env(env, var)*** which either returns the value of ***var*** in the environment ***env***, or else throws an error.

We define the environment data type inductively by the following definition:
```typescript
type Env = EmptyEnv | NonEmptyEnv;
type EmptyEnv = {tag: "EmptyEnv"}
type NonEmptyEnv = {tag: "Env"; var: string; val: Value; nextEnv: Env;}
```

The ***apply-env*** operation is defined recursively according to the structure of the data type:
```typescript
export const applyEnv = (env: Env, v: string): Result<Value> =>
    isEmptyEnv(env) ? makeFailure("var not found " + v) :
    env.var === v ? makeOk(env.val) :
    applyEnv(env.nextEnv, v);
```

Example of an environment and a corresponding look-up operation - using the environment implementation
used in class:

```typescript
applyEnv(makeEnv('x', 3,
           makeEnv('y', 2,
              makeEnv('x', 1, makeEmptyEnv())), 'x')
=> { tag: 'Ok', value: 3 }
```

Note how the external binding for ***x*** to 3 hides the internal binding of ***x*** to 1.

```typescript
applyEnv(makeEnv('x', 3,
           makeEnv('y', 2,
             makeEnv('x', 1, makeEmptyEnv())), 'y')
=> { tag: 'Ok', value: 2 }
```

```typescript
applyEnv(makeEnv('x', 3,
           makeEnv('y', 2,
             makeEnv('x', 1, makeEmptyEnv())), 'z')
=> { tag: 'Failure', message: 'var not found z' }
```


<h3><b>Handling Primitives in L1</b></h3>

Let us recall the syntax of L1 using the BNF + Abstract Syntax specification we have developed in the previous lectures:

```scheme
<program> ::= (L1 <exp>+) // program(exps:List(exp))
<exp> ::= <define-exp> | <cexp>
<define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
<cexp> ::= <num-exp> // num-exp(val:number)
       | <bool-exp>  // bool-exp(val:boolean)
       | <prim-op>   // prim-op(op:string)
       | <var-ref>   // var-ref(var:string)
       | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
<prim-op> ::= + | - | * | / | < | > | = | not
<num-exp> ::= a number token
<bool-exp> ::= #t | #f
<var-ref> ::= an identifier token
<var-decl> ::= an identifier token
```

The values that can be returned in L1 are:

Value = number \| boolean \| PrimOp

 **Note:**  In L1, the only side-effect that we can get is from a ***define*** expression.

<h4><b> Representing Primitive Operators </b></h4>

In Scheme, when we compute this expression, we get:
```scheme
> +
#<procedure:+>
```
That is, the value of the + expression (which is an expression of type Symbol) is a procedure in Scheme. 

On the other hand, in JavaScript, when we evaluate the expression "+" we get:
```javascript
+;
 ^
Syntax Error: Unexpected token ; ...
```

(In Node - the reader keeps waiting for the continuation of the expression starting with + or -).

That is, in the syntax of JavaScript - primitive operators alone are not expressions - they are only defined as part of larger expressions that contain them.

In the code reviewed in class, we implemented primitive operators as syntactic expressions of type prim-op.
We represented primitive operators as a specific expression type in the AST <br/>***PrimOp(op: string)***. 
The value of a prim-op expression is itself. 
When we apply a primitive operator to arguments, we explicitly dispatch to each known primitive operator in the language and apply the corresponding primitive operation in the meta-language.
This happens in the procedure <br/>***apply-primitive***.

Let us implement a different strategy which is the same as in Scheme: primitives are variable references which refer to primitive procedures which 
are pre-defined in the global environment.

The steps of the change are:

* Change the syntax to represent primitive operators as VarRef expressions 
* Change the type returned when we evaluate a primitive operator
* Change the evaluation rules to support the evaluation of primitive operators as VarRef expressions that refer to primitive procedures

The result of all the changes discussed in this section appears in accompanying zip file.

<h4><b> Change of AST for Primitives</b></h4>

```scheme
;; The main change is that we remove the expression type prim-op
<cexp> ::= <num-exp> // num-exp(val:number)
       | <bool-exp>  // bool-exp(val:boolean)
       | <var-ref>   // var-ref(var:string)
       | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
```
Now, when we parse an expression such as <code>(L1 (* 3 2))</code> we obtain an AST:

```json
{
    "tag": "Program",
    "exps": [
        {
            "tag": "AppExp",
            "rator": {
                "tag": "VarRef",
                "var": "*"
            },
            "rands": [
                {
                    "tag": "NumExp",
                    "val": 3
                },
                {
                    "tag": "NumExp",
                    "val": 2
                }
            ]
        }
    ]
}
```
instead of:

```json
{
    "tag": "Program",
    "exps": [
        {
            "tag": "AppExp",
            "rator": {
                "tag": "PrimOp",
                "op": "*"
            },
            "rands": [
                {
                    "tag": "NumExp",
                    "val": 3
                },
                {
                    "tag": "NumExp",
                    "val": 2
                }
            ]
        }
    ]
}
```
<h4><b>Change the Structure of the Returned Values</b></h4>

The second change is now how to evaluate the sub-expression ```{ "tag": "VarRef", "var": "*" }```: in this case, we expect to obtain a value of a different type - which is a primitive procedure mapped to the variable <b>*</b> in the global environment.

This has an impact on the structure of the values the interpreter can return:


* In the class implementation, for L1, Value = number \| boolean \| PrimOp
* In the new implementation, we have: Value = number \| boolean \| <b>PrimProc</b>


where ***PrimProc*** is the type of the values returned in the meta-language (TypeScript in our case) when we compute the expressions <b>+, *, -, /, not, <, >, =</b>.

In more details, we define:

```typescript
export type Value = number | boolean | PrimProc;
type PrimitiveProcedure = (args: Value[]) => Value;
type PrimProc = {
    tag: "PrimProc";
    op: PrimitiveProcedure;
};
```


<h4><b> Change the applyPrimitive procedure </b></h4>

The original version of <code>applyPrimitive</code> is:

```typescript
const applyPrimitive = (proc: PrimOp, args: Value[]): Result<Value> =>
    proc.op === "+" ? makeOk(reduce((x, y) => x + y, 0, args)) :
    proc.op === "-" ? makeOk(reduce((x, y) => x - y, 0, args)) :
    proc.op === "*" ? makeOk(reduce((x, y) => x * y, 1, args)) :
    proc.op === "/" ? makeOk(reduce((x, y) => x / y, 1, args)) :
    proc.op === ">" ? makeOk(args[0] > args[1]) :
    proc.op === "<" ? makeOk(args[0] < args[1]) :
    proc.op === "=" ? makeOk(args[0] === args[1]) :
    proc.op === "not" ? makeOk(!args[0]) :
    makeFailure("Bad primitive op " + proc.op);
```
In the new version - we replace it with the following:

```typescript
const applyPrimitive = (proc: PrimProc, args: Value[]): Result<Value> =>
    makeOk(proc.op(args));
```

This is a shorter version of the function - but one that exploits the fact that we have pre-defined the primitive functions
and bound them to functions in the meta-language that all expect arguments in the same form (an array of Values).

<h4><b> Initializing the Global Environment with Primitive Values</b></h4>

This last step is where we map variable references to actual TypeScript functions.
We initialize the global environment with the bindings of the primitives defined in L1 to the corresponding procedures in the meta-language.

```typescript
const makeGlobalEnv = (): Env => 
  makeEnv('+', plus,
    makeEnv('-', minus,
      makeEmptyEnv()))

// where:
const plus : (args: number[]) => number =
    reduce((x, y) => x + y, 0, args)
const minus : (args: number[]) => number =
    reduce((x, y) => x - y, 0, args)
```
And in the procedure ```evalL1program```, we initialize the global environment differently:

```typescript
const evalL1program = (program: Program): Value =>
    evalExps(program.exps, makeGlobalEnv());  // instead of makeEmptyEnv()
```


This implementation explains properly what is meant by "primitive functions": they are the functions which are pre-defined in the global environment when the interpreter starts running - without the need for the programmer to define them.

<h4><b> Adding a Primitive to the Interpreter</b></h4>

With this implementation, it is easy to add a primitive to the interpreter - we need only:
* Add the proper binding in the initialization of the global environment


For example:

```typescript
// Primitive operators are encoded as varRef bound out of the box to
// functions in the meta-language with an appropriate interface.
// The PrimProc wrapper is used to tag such primitive operator values in
// an unambiguous manner.
const makeGlobalEnv = (): Env =>
    makeEnv('+', makePrimProc((args: number[]) => reduce((x, y) => x + y, 0, args)),
    makeEnv('-', makePrimProc((args: number[]) => reduce((x, y) => x - y, 0, args)),
    makeEnv('*', makePrimProc((args: number[]) => reduce((x, y) => x * y, 1, args)),
    makeEnv('/', makePrimProc((args: number[]) => reduce((x, y) => x / y, 1, args)),
    makeEnv('display', makePrimProc((args: Value[]) => console.log(args)),
    makeEnv('newline', makePrimProc((args: Value[]) => console.log("")),
    makeEmptyEnv()))));
```

and we have two new primitives in our language (display and newline).


<h3><b>Order of Evaluation of Parameters in a Procedure Application</b></h3>

evalL1program(program) receives a program, which includes an ordered sequence of expressions. 
It iterates over the expressions and depending on the type of each expression, it either evaluates a ***DefineExp*** and obtains a new environment, which is then used to evaluate the next steps of the program; or it evaluates the expression.

In expressions such as `(+ (+ 1 2) (+ 3 4) (+ 5 6))` the operational semantics does <b>not</b> specify the order of execution among the arguments - we could compute (+ 5  6) first and (+ 1 2) next, or in reverse, or even together (in parallel).

This is the case in the formal semantics of Scheme - the standard explicitly indicates that the order of evaluation of operands in an application is NOT specified.

This means that we can obtain different outputs if we insert side-effects as part of the operands depending on the implementation in Scheme:

```scheme
(define side-effect
  (lambda (x) (display x) x))

(+ (* (side-effect 2) (side-effect 3))
   (* (side-effect 4) (side-effect 5)))

=> can display 
2345 26

or
4235 26

(or any other variant of the order of parameters)
```


<h3><b>L2 Evaluation</b></h3>

NOTE: In the rest of the notes, we return to the syntactic treatment of primitives as discussed in the lectures
(as opposed to the semantic treatment discussed above).


The syntax of  L2  extends that of  L1  with two new expression types:
***IfExp*** and ***ProcExp***

```scheme
<program> ::= (L2 <exp>+) // program(exps:List(exp))
<exp> ::= <define-exp> | <cexp>
<define-exp> ::= (define <var-decl> <cexp>) // def-exp(var:var-decl, val:cexp)
<cexp> ::= <num-exp> // num-exp(val:number)
       | <bool-exp>  // bool-exp(val:boolean)
       | <prim-op>   // prim-op(op:string)
       | <var-ref>   // var-ref(var:string)
       | (if <exp> <exp> <exp>) // if-exp(test,then,alt)                                    ##### L2
       | (lambda (<var-decl>*) <cexp>+) // proc-exp(params:List(var-decl), body:List(cexp)) ##### L2
       | (<cexp> <cexp>*) // app-exp(rator:cexp, rands:List(cexp))
<prim-op> ::= + | - | * | / | < | > | = | not
<num-exp> ::= a number token
<bool-exp> ::= #t | #f
<var-ref> ::= an identifier token
<var-decl> ::= an identifier token
````

The Value types includes closure values in addition to the previous types:

Value = Number \| Boolean \| Prim-op \| <b>Closure</b>


* ***We define the closure data type as a record with two fields:***

* ***Params: a list of VarDecl values***

* ***Body: a list of cexp values***

<b>
`Closure ::= (Closure (<var-decl>*) <cexp>+) // closure(params:List(var-decl), body:List(cexp))`
</b>

<b>Note</b> that a proc-exp ```(lambda (x) (* x x))``` is an <b>expression</b> while a closure is a <b>value</b>.
They are of 2 different types - closures are the result of a computation.

The key change in L2 vs L1 is the support of closures when applying procedure values to arguments:

```typescript
const L3applyProcedure = (proc: Value, args: Value[], env: Env): Result<Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
    isClosure(proc) ? applyClosure(proc, args, env) :
    makeFailure("Bad procedure " + JSON.stringify(proc));

const applyClosure = (proc: Closure, args: Value[], env: Env): Result<Value> => {
    let vars = map((v: VarDecl) => v.var, proc.params);
    let body = renameExps(proc.body);
    let litArgs = map(valueToLitExp, args);
    return evalSequence(substitute(body, vars, litArgs), env);
}
```
The body is evaluated with ***evalExps*** - but we only return the last value that is computed:
For example:
```scheme
(lambda (x)
    (+ 1 x)
    (+ 2 x)) 
```

First we evaluate`(+ 1 x)` only then we evaluate `(+ 2 x)`. The return value is the return value of the last expression. 

The substitution procedure addresses 3 issues:
* <b>we make sure the body is renamed so that we avoid capturing free variables</b>

* <b>we map the evaluated arguments to expressions to ensure that the result of the substitution is a well-typed AST which can be evaluated (this is done with the procedure ***valueToLitExp***)</b>.

* <b>we do not replace bound variables inside the body - only free variables</b>


<h3><b>Renaming and Substitution</b></h3>

<h4><b>Renaming</b></h4>

Bound variables in expressions can be consistently renamed by new variables (that do not occur in the expression) without changing the intended meaning of the expression.
That is, expressions that differ only by consistent renaming of bound variables are semantically equivalent.

For example, the following are equivalent pairs:

```scheme
(lambda (x) x) <==> (lambda (x1) x1)
(lambda (x) (+ x y)) <==> (lambda (x1) (+ x1 y))  // y is not renamed because it occurs free
```
      
Why renaming?
      
```scheme
(define z not)

(((lambda (x)
     (lambda (z)
        (x z)))
  (lambda (w)
     (z w)))
  ;; Note: z occurs free in the parent exp.
 #f)
```

For renaming we use the ***renameExps*** procedure:
```typescript
const makeVarGen = (): (v: string) => string => {
    let count: number = 0;
    return (v: string) => {
        count++;
        return `${v}__${count}`;
    }
}

/*
Purpose: Consistently rename bound variables in 'exps' to fresh names.
         Start numbering at 1 for all new var names.
*/
const renameExps = (exps: CExp[]): CExp[] => {
    const varGen = makeVarGen();
    const replace = (e: CExp): CExp =>
        isIfExp(e) ? makeIfExp(replace(e.test), replace(e.then), replace(e.alt)) :
        isAppExp(e) ? makeAppExp(replace(e.rator), map(replace, e.rands)) :
        isProcExp(e) ? replaceProc(e) :
        e;
    // Rename the params and substitute old params with renamed ones.
    // First recursively rename all ProcExps inside the body.
    const replaceProc = (e: ProcExp): ProcExp => {
        const oldArgs = map((arg: VarDecl): string => arg.var, e.args);
        const newArgs = map(varGen, oldArgs);
        const newBody = map(replace, e.body);
        return makeProcExp(map(makeVarDecl, newArgs),
                           substitute(newBody, oldArgs, map(makeVarRef, newArgs)));
    }
    return map(replace, exps);
}
```

Notes on rename-exps:

* The structure of renameExps is the same as that of rewrite-ASTs we saw in the previous lectures - a traversal of the AST top down with recursion on the components.

* The only transformation renameExps does is on expressions of type ProcExp - all other expression types are copied as is.

* It is a <b>pre-condition</b> of <code>substitute</code> that the expressions in which the substitution is performed are renamed. This pre-condition is enforced in the code of ***renameExps***, before we call substitute, we make sure the body is renamed .



<b> Substitution </b>

Substitute is an operation which replaces free occurrences of variable references in an expression by other expressions.
<b>Definition:</b> A substitution s is a mapping from a finite set of variables to a finite set of expressions.

Substitutions are denoted using set notation. For example:

```
6 o {x=5} = 6

(+ x y) o {z = 5} = (+ x y)
 
(+ x y) o {x=5, y=6, z=7} = (+ 5 6)

(lambda (x) (+ x 6)) o {x=5} = 
    renaming: E turns into (lambda (x1) (+ x1 6)
    substitute: E turns into (lambda (x1) (+ x1 6))

( + (x z) ((lambda (x) x) y))  o {x = (lambda (x) x) , y = 7 , z =8}
    renaming: E turns into ( + (x z) ((lambda (x1) x1) y)) 
    renaming: s turns into {x = (lambda (x2) x2) , y = 7, z = 8}
    substitute: E turns into  ( + ((lambda (x2) x2) 8) ((lambda (x1) x1) 7))
```

Composition (combination) of Substitutions s  and  s′ , denoted  s∘s′ , is a substitution  s″  that extends  s  with a binding <x; s' (x)> for every variable  x  for which  s(x) is not defined.

For example: 
```
#1: {} o {x = 3} = {x = 3}

#2: {x = 3} o {y = #t} = {x = 3, y = #t}

#3: {x = y} o {y = 3} = {x = 3, y = 3}            // Note that the binding of x changed to 3

#4: {x = y} o {y = x} = {x = x, y = x} = {y = x}  // What happens when we apply this substitution?
```
Let us explain #3 and #4:

Applying `{x = y} o {y = 3}] (s o s')` to any expression E, we want to obtain the same result as performing first s' then s - that is:

`
For any E, E o [{x = y} o {y = 3}] = [E o {x = y}] o {y = 3}
`

Let us take an example for E that contains x and y:

`
E = (x y)
E o [{x = y} o {y = 3}] 
= [E o {x = y}] o {y = 3}
= (y y) o {y = 3}
= (3 3)
`
After applying {x = y} there are no more instances of x, only instances of y in the result.
Therefore, when we replace <b>simultaneously</b> x and y in the composed substitution, we must make sure x is replaced to 3 and not to y.

Let us look at #4: we apply the same logic as in #3, and obtain that:

`
[{x = y} o {y = x}] = [{x = x, y = x}]
`

The substitution of x into x is not introducing any modification, therefore, we can drop it - and we obtain:

`
[{x = y} o {y = x}] = [{x = x, y = x}] = {y = x}
`

Substitution involves renaming as a first step:

```
((+ x ((lambda (x) (* x 2)) 3))) o {x = 5} =
    Renaming: ((+ x ((lambda (x1) (* x1 2)) 3)))
    Substitute: ((+ 5 ((lambda (x1) (* x1 2)) 3)))
```

<h3><b>Question: Why do we need renaming?</b></h3> 

Let us look at this program:

***(lambda (x) (lambda (y) (y x)))***

rename ***x*** -> ***a***, ***y*** -> ***b***  - all ok:

***(lambda (a) (lambda (b) (b a)))***

From this, you might conclude that any substitution is allowed - i.e. any variable in any lambda term can be replaced by any other. This is not so. Consider the inner lambda in the first expression above:

***(lambda (y) (y x))***

In this expression, ***x*** is free - it is not bound by a lambda abstraction. If we were to replace ***y*** with ***x***, the expression would become:

***(lambda (x) (x x))***

But we already specified that when we apply a substitution, we do not replace bound variables - so that:

***(lambda (y) (y x))*** o {y = x}

is not causing any harm - we return the same value unchanged, because y is bound.

But now consider what happens if we introduce a value into the expression that contains a binding with a name to a variable that occurred free in the original expression.
The replacement will lead to a capture of the free variable by the new binding. 

```scheme
(define z not)

(((lambda (x)
     (lambda (z)
        (x z)))
  (lambda (w)
     (z w)))
  ;; Note: z occurs free in the parent exp.
 #f)
```
Work out the capture - step by step...

How it works with renaming:

```scheme
;; rename z in (lambda (z) (x z)) ==> (lambda (z1) (x z1))
;; rename w in (lambda(w)(z w)) ==> (lambda(w1)(z w1))
;; substitute (lambda(z1)(x z1)) ∘ {x = (lambda(w1)(z w1))} ==> 
(lambda (z1)
   ((lambda (w1)
       (z w1)) ;z remains free!
    z1))

;; substitute ((lambda (w1) (z w1)) z1)∘ {z1 = #f}==>
((lambda(w1)
    (z w1))
 #f)

;;substitute (z w1) ∘ {w1 = #f}==>
(z #f)

;;substitute (z #f) ∘ {z = not}==>
(not #f) 
#t
```

<h3><b>Applicative Eval</b></h3>

Look at the next code:
```scheme
((lambda (x z) 
    (* (+ x z) z))
 1
 (+ 1 2))
```
<b>Q: </b> Evaluate the following expression according to the applicative-eval algorithm

<b> A:</b>
```
applicative-eval     [((lambda (x z) (* (+ x z) z)) 1 (+ 1 2))]
    applicative-eval [(lambda (x z) (* (+ x z) z))] ==> <Closure (x z) (* (+ x z) z)>
    applicative-eval [1] ==> 1
    applicative-eval [(+ 1 2)]
        applicative-eval [+] ==> #<primitive-procedure +>
        applicative-eval [1] ==> 1
        applicative-eval [2] ==> 2
   3
reduce:
    applicative-eval [(* (+ 1 3) 3)]
        applicative-eval [*] ==> #<primitive-procedure *>
        applicative-eval [(+ 1 3)]
            applicative-eval [+] ==> #<primitive-procedure +>
            applicative-eval [1] ==> 1
            applicative-eval [3] ==> 3
        ==> 4
        applicative-eval [3]==> 3
    12
```


<h3><b>Normal Eval</b></h3>

<b> Q:</b>  Evaluate the following expression according to the normal evaluation algorithm:
```scheme
(((lambda (x)
     (lambda (y) 
        (* x y))) 
  (+ 1 2)) 
  2)  
```

<b>A:</b>

```
normal-eval[ (( (lambda (x)(lambda (y)(* x y)))(+ 1 2))2) ]
  normal-eval [( (lambda (x)(lambda (y)(* x y)))(+ 1 2)) ]
    normal-eval[(lambda (x)(lambda (y)(* x y)))] ==><closure (x) (lambda(y) (* x y))>
    substitute:
    (lambda(y) (* x y) ) ᵒ {x = (+ 1 2) } ==> (lambda(y) (* (+ 1 2) y) )
    reduce:
    normal-eval[lambda(y) (* (+ 1 2) y) ] ==><closure (y) (* (+ 1 2) y)>
    substitute:
    (* (+ 1 2) y) ᵒ { y = 2} ==> (* (+ 1 2) 2)
    Reduce:
    Normal-eval[ * (+ 1 2) 2 ] ==>
      Normal-eval[ * ] ==> <primitive-procedure: *>
      Normal-eval[ ( + 1 2) ] ==>
        Normal-eval[+] ==> <primitive-procedure: +>
        Normal-eval[1] ==> 1
        Normal-eval[2] ==> 2
      ==> 3
      Normal-eval[2] ==> 2
   ==> 6
```

<h3><b>applicative-eval VS normal-eval</b></h3>

<b>Q:</b>Does the evaluation Strategy matter? 

Let us look at the next program:

```scheme
(define test
  (lambda (x y)
    (if (= x 0)
        0
        y)))

(define zero-div
  (lambda (n)
    (/ n 0))) ; division by zero!

(test 0 (zero-div 5))
```

<b> Normal Eval </b>

```
normal-eval[(test 0 (zero-div 5))]
    normal-eval[test] ==> <closure (lambda (x y) (if (= x 0) 0 y))>

(if (= x 0) 0 y))ᵒ{x = 0} ==> (if (= 0 0) 0 y))

(if (= 0 0) 0 y))ᵒ{y = (zero-div 5)} ==> (if (= 0 0) 0 (zero-div 5) ))

reduce:

normal-eval[(if (= 0 0) 0 (zero-div 5)))]
    normal-eval[(= 0 0)]
        normal-eval[ = ] ==>#<primitive-procedure =>
    normal-eval[ 0 ] ==>0
    normal-eval[ 0 ] ==>0
    ==> #t
    normal-eval[0] ==> 0
==>0
```

Notice that we didn't evaluate ***(div-zero 5)***!

<b> Applicative Eval </b>

```
applicative-eval[(test 0 (zero-div 5))]
    applicative-eval[test] ==> <closure (lambda (x y) (if (= x 0) 0 y))>
    applicative-eval[0] ==> 0
    applicative-eval[(zero-div 5)]
        applicative-eval[zero-div]==><closure (lambda (n) (/ n 0))>
        applicative-eval[5]==> 5
```

We get a <b>"division by zero"</b> error !!!


Another example:

```scheme
(define loop (lambda (x) (loop x)))

(define g (lambda (x y) y))

(g (loop 0) 7)
```

normal-eval will return 7 while applicative-eval will get into infinite loop.
why is that?


We saw two examples where different manner of evaluation get different result.

<b>Notice:</b> If both evaluations don't get into an infinte loop and/or an error - they both return the same value!

<h3><b> Distinguish between different evaluation strategies</b></h3>

<b>Q: </b> How can we distinguish what evaluation strategy the interpreter is using?

<b>A:</b>
As we saw in the previous questions -
 
if we understand the differences between the evaluations methods we can write a program that each method will act different on.

for example:
```scheme
(define applic
  (lambda ()
    (display 'applic)
    0))

(define test (lambda (x) 1))

(test (applic))
```

Normal eval will not have any side-effects while applicative eval will display "applic" to the screen.
They both will return the value 1.

<!---

<h3><b> letrec </b></h3>

Let us recall the exp procedure:
```scheme
; Signature: exp(b e)
; Purpose: to compute the function b^e
; Type: [Number * Number -> Number]
; Pre-conditions: b >= 0, e is natural
; Tests:
; (exp 2 3) => 8
; (exp 2 4) => 16
; (exp 3 4) => 81

(define exp
  (lambda (b e)
    (exp-iter b e 1)))

(define exp-iter
  (lambda (b e acc)
    (cond ((= e 0) acc)
           (else (exp-iter b (- e 1) (* b acc))))))
```

In this program both "exp" and "exp-iter" are public functions.
but the user will only use "exp" so we want "exp-iter" to be non-public procedure.

how can we do that?

<b>First attempt:</b>
we will use let

```scheme
(define exp
  (lambda (b e)
    (let ((exp-iter (lambda (b e acc)
                      (cond ((= e 0) acc)
                      (else (exp-iter b (- e 1) (* b acc)))))))
    (exp-iter b e 1))))
```

<b>Q:</b> What is the problem with that program?

<b>A:</b> We use ***exp-iter*** in its definition (why is that a problem?)

<b>Second attempt:</b>

```scheme
(define exp
  (lambda (b e)
    (letrec ((exp-iter (lambda (b e acc)
                         (cond ((= e 0) acc)
                         (else (exp-iter b (- e 1) (* b acc)))))))
    (exp-iter b e 1))))
```

Here we use letrec instead let so a defintion of a procedure can be recusive.

Can we do it better? Yes! By noticing that ***b*** stays constant throughout ***exp-iter***, we can just use the existing ***b*** instead of passing it around as an argument.

<b> Third attempt:</b>

```scheme
(define exp
  (lambda (b e)
    (letrec ((exp-iter (lambda (e acc)
                         (cond ((= e 0) acc)
                         (else (exp-iter (- e 1) (* b acc)))))))
    (exp-iter e 1))))
```

-->
