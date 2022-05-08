# Type inference using type constraints

## Definition (seen in class):

A **Type-substitution** is a mapping, s, from a finite set of type-variables to a finite set of type-expressions, such that for every type-variable T, s(T) does not include T.
A type-binding is a pair  `<T,s(T)>`  such that  `T = s(T)`.
Type-Substitutions are written using set notation, for example :

* **{T1 = Number, T2 = [Number -> T3]}**.
* **{T1 = Number, T2 = [[Number -> T3]->T2]}, is this legal?**
* **{T1 = Number, T1 = Boolean}, and this one?**


## Question 1 
### Typing statement

A typing statement is a true/false formula that states a judgment about the type of a language expression, given a type environment.

It has the notation:  `Tenv ⊢ e:T`, which reads: under the type-environment Tenv, the expression e has type T.

E.g.,  `{x:Number} ⊢ (+ 3 x):Number`  states that under the assumption that the type of x is Number, the type of (+ 3 x) is Number. For the typing statement below, note whether they are  **true or false:**

* **{f:[Number->T1]} ⊢ (f 7): T1**	
* **{y:Number, f:[T1->T1]} ⊢ (f x):T1**	
* **{x:Number} ⊢ x: Boolean**	
* **{x:Boolean, y:Number} ⊢ x: Boolean**
* **{f:[T1->T2], g:[T2->T3], x:T1} ⊢ (g (f x)):T3**

## Question 2 

A typing-statement is based on assumptions in a given type-environment.
The less assumptions are involved, the stronger is the statement.
In order to end up with the strongest statement, we follow a simple heuristic rule: always pick a minimal type environment (or: always infer the strongest statement possible). For each row of statements below, **which statement is stronger?** left or right ?

Note : assumptions in Tenv are an added part to the axioms we have.

|Term 1                         | Term 2 
|-------------------------------|-----------------------------|
|{foo: [T1->T2]} ⊢ 5:N	        |{} ⊢ 5:N  
|{x:N} ⊢ (+ x 3):N  	        |{y:N,x:N} ⊢ (+ x 3):N
|{} ⊢ (lambda (x)(+ x 3)):[N->N]| {y:N} ⊢ (lambda (x)(+ x y)):[N->N] <br> (Can we compare these two?)
|{f:[N->N]} ⊢ ((lambda (f x) (f x))(lambda (x) (* x x)) 10):N | {} ⊢ ((lambda (f x) (f x)) (lambda (x) (* x x)) 10):N|

Note: A typing statement has to be true in order to determine its strength.

## Definitions (seen in class):

An application of a type-substitution s to a type-expression TE, denoted by TE ○ s, is a consistent replacement of type-variables T in TE by their mapped type-expressions s(T).
For example:

* **[[T1->T2]->T2] ○ {T1=Boolean, T2=[T3->T3]} = [[Boolean->[T3->T3]] -> [T3->T3]]**
* **[[T1->T2]->T2] ○ {T1=Boolean, T2=[T3->T3],T3=Number} = [[Boolean->[T3->T3]] -> [T3->T3]]**

## Question 3

A unifier of type expressions **TE1**, **TE2** is a type substitution s such that **TE1 ○ s = TE2 ○ s** (the type expressions should not include common type variables).
The most general unifier (**MGU**), is a unifier such that any other unifier is an instance of it.

Recall the lecture's definition of "instance of type expression" :  **T'** is an instance of type expression **T**, if there is a type substitution **s** such that  `T o s = T'`.

In case of an instance of the MGU type substitution, it is any type substitution s such that for each type expression e in s, e is an instance of a type expression in our MGU.
All other unifiers are obtained from it by application of additional substitutions. 

**Find the MGU**  for the following pairs of type-environments:

|Type expressions	            | MGU
|-------------------------------|-----------------------------|
|{x: T1}, {x: N}	            |{T1=N}  (a non-MGU unifier: {T1=N, T2=B})
{x: [T1*[T1->T2]->T2]},  {x: [Number * [T3->T4]->T4]}	    |{T1=Number, T3=Number, T4=T2}
{x: [T1*[T1->T2]->N]},   {x: [[T3->T4] * [T5->Number]->N]}  |{T1=[T3->T4], T5=[T3->T4], T2=Number}
{x: [T1*[T1->T2]->N]},   {x: [Number * [Symbol->T3]->N]}    |No unifier

## Type inference using type constraints

-   Rename bound variables.
-   Assign type variables to all sub-expressions.
-   Construct type equations.
-   Solve the equations.

### Typing rules

-   For primitive atomics / primitive procedures, construct type equations using their types.
-   For procedure expressions  `(lambda (p1 ... pn) e1 ... em)`, construct:
		-   `T_{(lambda (p1 ... pn) e1 ... em)} = [T_{p1} * ... * T_{pn} -> T_{em}]`
-   For application expressions  `(op p1 ... pn)`  construct:
		-   `T_{op} = [T_{p1} * ... * T_{pn} -> T_{(op p1 ... pn)}]`

## Question 1

Typing the expression  `((lambda (f1 x1) (f1 x1)) sqrt 4)`: =>  `((lambda ([f1 : Tf] [x1 : Tx]) : T2 (f1 x1)) sqrt 4)`

Note: See how we added type anotations to the lambda's params, and return value.

**Stage I**: Rename bound variables.

`((lambda (f1 x1) (f1 x1)) sqrt 4)`  turn to  `((lambda (f x) (f x)) sqrt 4)`

**Stage II**: Assign type variables for every sub expression:

|Expression               	    | Variable
|-------------------------------|-----------------------------|
((lambda (f x) (f x)) sqrt 4)	|T0
(lambda (f x) (f x))	        |T1 
(f x)	                        |T2 
f	                            |Tf
x	                            |Tx
sqrt	                        |Tsqrt
4	                            |Tnum4


**Stage III**: Construct type equations. 

The equations for the sub-expressions are:

|Expression               	    | Equation
|-------------------------------|-----------------------------|
((lambda (f x) (f x)) sqrt 4)   |T1 = [Tsqrt * Tnum4 -> T0]
(lambda (f x) (f x))	        |T1 = [Tf * Tx -> T2]
(f x)	                        |Tf = [Tx -> T2]


The equations for the primitives are:

|Expression               	    | Equation
|-------------------------------|-----------------------------|
sqrt	                        |Tsqrt = [Number -> Number]
4                               |Tnum4 = Number

**Stage IV**: Solve the equations.

|Equation| Substitution
|-------------------------------|-----------------------------|
| 1. T1 = [Tsqrt * Tnum4 -> T0]	| {}
| 2. T1 = [Tf * Tx -> T2]	    |
| 3. Tf = [Tx -> T2]	        |
| 4. Tsqrt = [Number -> Number] |	
| 5. Tnum4 = Number             |


**Step 1:**

`(T1 = [Tsqrt * Tnum4-> T0]) ○ Substitution = (T1 = [Tsqrt * Tnum4-> T0])`  and is a type-sub.

`Substitution = Substitution ○ (T1 = [Tsqrt * Tnum4-> T0])`.

|Equation| Substitution
| -------------------------------|-----------------------------|
| 2. T1 = [Tf * Tx -> T2]	     | {T1 := [Tsqrt * Tnum4 -> T0]}
| 3. Tf = [Tx -> T2]             |
| 4. Tsqrt = [Number -> Number]  |	
| 5. Tnum4 = Number              |

**Step 2**:

T1 = [Tf * Tx -> T2] ○ Substitution = ([Tsqrt * Tnum4 -> T0] = [Tf * Tx -> T2])
There is not type-sub since  both sides of the equation are composite **we split it into three** equations (6,7,8) and remove equation 2.

|Equation| Substitution
|-------------------------------|-----------------------------|
| 3. Tf = [Tx -> T2]	        | {T1 := [Tsqrt * Tnum4 -> T0]}
| 4. Tsqrt = [Number -> Number]	|
| 5. Tnum4 = Number	            |
| 6. **Tf = Tsqrt**	            |
| 7. **Tx = Tnum4 (or Tnum4 = Tx)**	|
| 8. **T2 = T0**	            |


**Step 3:**

(Tf = [Tx -> T2]) ○ Substitution = (Tf = [Tx -> T2]).
Substitution = Substitution ○ (Tf = [Tx -> T2]).

|Equation| Substitution
|-------------------------------|-----------------------------|
| 4. Tsqrt = [Number -> Number]	| {T1 := [Tsqrt * Tnum4 -> T0],  Tf := [Tx -> T2]}
| 5. Tnum4 = Number	            |
| 6. Tf = Tsqrt	                |
| 7. Tx = Tnum4 (or Tnum4 = Tx)	|
| 8. T2 = T0                    |


**Step 4**:

(Tsqrt = [Number -> Number]) ○ Substitution = (Tsqrt = [Number -> Number]).
Substitution = Substitution ○ (Tsqrt = [Number -> Number]).

|Equation| Substitution
|-------------------------------|-----------------------------|
|5. Tnum4 = Number	            | { [T1 :=[**[Number -> Number]** * Tnum4->T0],<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tf := [Tx -> T2], Tsqrt := [Number -> Number]}                                                    
|6. Tf = Tsqrt                  |
|7. Tx = Tnum4                  |
|8. T2 = T0                     |


**Step 5**:

(Tnum4 =Number) ○ Substitution = (Tnum4 =Number), is a type-sub.
Substitution = Substitution ○ (Tnum4 =Number).

|Equation                       | Substitution
|-----------------|-----------------------------|                             
| 6. Tf = Tsqrt   | {  &nbsp;&nbsp;&nbsp; T1 :=[[Number -> Number] * **Number**->T0],	 <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tf := [Tx -> T2], Tsqrt := [Number -> Number], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  **Tnum4 := Number** &nbsp;&nbsp;&nbsp;}              
| 7. Tx = Tnum4	  |          
| 8. T2 = T0      |

**Step 6**:

(Tf = Tsqrt) ○ Substitution = ([Tx -> T2]=[Number -> Number])
There is no a sub-type. We split the equation into two equations 
Tx = Number
T2 = Number

 We remove the equation number 6.


|Equation   | Substitution
|------------------------------------------|-----------------------------|                                         
|7. Tx = Tnum4                             | {  &nbsp;&nbsp;&nbsp; T1 :=[[Number -> Number] * Number->T0],	 <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tf := [Tx -> T2], Tsqrt := [Number -> Number], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Tnum4 := Number &nbsp;&nbsp;&nbsp;}   
|8. T2 = T0                                |
|9. **Tx = Number**                        |
|10. **T2 = Number**                       |



**Step 7:**

(Tx = Tnum4) ○ Substitution = ([Tx = Number]), type-sub.
Substitution = Substitution ○ ([Tx = Number]).

|Equation   | Substitution
|------------------------------------------|-----------------------------|                             
|8. T2 = T0                                |  {  &nbsp;&nbsp;&nbsp; T1 :=[[Number -> Number] * Number->T0],	 <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tf := [**Number** -> T2], Tsqrt := [Number -> Number], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Tnum4 := Number, <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; **Tx = Number**   &nbsp;&nbsp;&nbsp;}                  
|9. Tx = Number                            |
|10. T2 = Number                           |


**Step 8:**

(T2 = T0) ○ Substitution = (T2 = T0), type-sub.
Substitution = Substitution ○ (T2 = T0).

|Equation   | Substitution
|------------------------------------------|-----------------------------|                             
|9. Tx = Number   | {  &nbsp;&nbsp;&nbsp; T1 :=[[Number -> Number] * Number->T0],	 <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tf := [Number-> **T0**], Tsqrt := [Number -> Number], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Tnum4 := Number, <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tx = Number,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  **T2 := T0** <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;}                  
|10. T2 = Number    |


**Step 9:** `(Tx = Number) ○ Substitution = (Number = Number) always true.`


|Equation   | Substitution
|------------------------------------------|-----------------------------|                             
|10. T2 = Number | {  &nbsp;&nbsp;&nbsp; T1 :=[[Number -> Number] * Number->T0],	 <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tf := [Number-> T0], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tsqrt := [Number -> Number], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Tnum4 := Number, <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tx = Number , <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  T2 := T0 &nbsp;&nbsp;&nbsp;&nbsp;}                  


**Step 10:**

(T2 = Number) ○ Substitution = (T0 = Number), type-sub.
Substitution = Substitution ○ (T0 = Number).

|Equation   | Substitution
|------------------------------------------|-----------------------------|                             
| | {  &nbsp;&nbsp;&nbsp; T1 :=[[Number -> Number] * **Number->Number**],	 <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tf := [Number-> **Number**], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tsqrt := [Number -> Number], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Tnum4 := Number, <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tx = Number , <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  T2 := **Number** <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  **T0 := Number**   &nbsp;&nbsp;&nbsp;&nbsp;}                  


The type inference succeeds since we have a type for T0, meaning that the expression is  **well typed**.
Because there are no free variables, the inferred type of T0 is:  **Number.**

**Note**: Our expression can be written now as  `((lambda ([f : (N -> N)] [x : N]) : N (f x)) sqrt 4)`

## Question 2

Extending the typing mechanism for if expressions: Recall the typing rule for if expressions shown in the typing system:

```
### Typing rule of If experession :
For every: type environment _Tenv, 
           expressions _e1, _e2, _e3,
           and type expressions Boolean, _S2:
If         _Tenv ⊢ _e1:Boolean,
           _Tenv ⊢ _e2:_S2,
           _Tenv ⊢ _e3:_S2
Then _Tenv ⊢ (if _e1 _e2 _e3):_S2
```

First thing we notice from the rule is that the consequence expression (_c) and the alternative expression (_a) have the same type.
Given the first observation, the second thing we notice is that the type of the if expression is the type of the consequence expression.
Given the two observation we can add the following equations:
```
Expression	 Equation
(if _p _c _a)	 T_p = Boolean
               T_c = T_a
            	 Tif = T_c
```

#### Questions:

-   If both consequence and alternative expressions had different types could we type the if expression?
-   Do we need to add an equation for _p?

  
Example: type the expression `(if #t (+ 1 2) 3)`

**Again**, look how we add type annotations.

**Stage I**: Rename bound variables: No reference of variables so no renaming is needed.

**Stage II**: Assign type variables for every sub expression:



|Expression               	    | Variable
|-------------------------------|-----------------------------|
|(if #t (+ 1 2) 3)	            |T0
|(+ 1 2)	                    |T1
|+	                            |T+
|#t	                            |T#t
|1	                            |Tnum1
|2	                            |Tnum2
|3	                            |Tnum3



**Stage III**: Construct type equations. The equations for the sub-expressions are:


|Expression               	    | Equation
|-------------------------------|-----------------------------|
|(if #t (+ 1 2) 3)	            |T1 = Tnum3
|                               |T0 = T1
|(+ 1 2)	                    |T+ = [Tnum1 * Tnum2 -> T1]


The equations for the primitives are:

|Expression               	    | Equation
|-------------------------------|-----------------------------|
|+	                            |T+ = [Number * Number -> Number]
|#t	                            |T#t = Boolean
|1	                            |Tnum1 = Number
|2	                            |Tnum2 = Number
|3	                            |Tnum3 = Number


**Stage IV** : Solve the equations:


|Equation| Substitution
|-------------------------------|-----------------------------|
|1. T1 = Tnum3	                |{}
|2. T0 = T1	                    |
|3. T+ = [Tnum1 * Tnum2 -> T1]	|
|4. T+ = [Number * Number -> Number] |	
|5. T#t = Boolean	            |
|6. Tnum1 = Number              |	
|7. Tnum2 = Number              |	
|8. Tnum3 = Number              |



**Step 1:**

(T1 = Tnum3) ○ Substitution = (T1 = Tnum3), type-sub.

Substitution = Substitution ○ (T1 = Tnum3).


|Equation| Substitution
|-------------------------------|-----------------------------|
|2. T0 = T1	|   {&nbsp;&nbsp;&nbsp; **T1 := Tnum3** &nbsp;&nbsp;&nbsp; }
|3. T+ = [Tnum1 * Tnum2 -> T1]	|
|4. T+ = [Number * Number -> Number] |	
|5. T#t = Boolean	|
|6. Tnum1 = Number|	
|7. Tnum2 = Number|	
|8. Tnum3 = Number |	




**Step 2:**

(T0 = T1) ○ Substitution = (T0 = Tnum3), type-sub.

Substitution = Substitution ○ (T0 = Tnum3).

|Equation| Substitution
|-------------------------------|-----------------------------|
|3. T+ = [Tnum1 * Tnum2 -> T1]	|   {&nbsp;&nbsp;&nbsp; T1 := Tnum3,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; **T0 := Tnum3**  &nbsp;&nbsp;&nbsp; }
|4. T+ = [Number * Number -> Number] |	
|5. T#t = Boolean	|
|6. Tnum1 = Number|	
|7. Tnum2 = Number|	
|8. Tnum3 = Number |	


**Step 3:**

(T+ = [Tnum1 * Tnum2 -> T1]) ○ Substitution = (T+ = [Tnum1 * Tnum2 -> Tnum3]), type-sub.

Substitution = Substitution ○ (T+ = [Tnum1 * Tnum2 -> Tnum3]).


|Equation| Substitution
|-------------------------------|-----------------------------|
|4. T+ = [Number * Number -> Number] |	 {&nbsp;&nbsp;&nbsp; T1 := Tnum3,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; T0 := Tnum3,  <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; **T+ = [Tnum1 * Tnum2 -> Tnum3]** &nbsp;&nbsp;&nbsp; }
|5. T#t = Boolean	|
|6. Tnum1 = Number|	
|7. Tnum2 = Number|	
|8. Tnum3 = Number |	

**Step 4:**

(T+ = [Number * Number -> Number]) ○ Substitution = ([Tnum1 * Tnum2 -> Tnum3] = [Number * Number -> Number])

There is not  a type-sub. 
We split the equation to  `Tnum1 = Number, Tnum2 = Number and Tnum3 = Number`, 
and add them to the equations. Since they already exists,  **we only need to remove equation 4.**

|Equation| Substitution
|-------------------------------|-----------------------------|
|4. T+ = [Number * Number -> Number] |	 {&nbsp;&nbsp;&nbsp; T1 := Tnum3,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; T0 := Tnum3,  <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; **T+ = [Tnum1 * Tnum2 -> Tnum3]** &nbsp;&nbsp;&nbsp; }
|5. T#t = Boolean	|
|6. Tnum1 = Number|	
|7. Tnum2 = Number|	
|8. Tnum3 = Number |	

Skipping the trivial steps, we get:

|Equation| Substitution
|-------------------------------|-----------------------------|
| | { &nbsp;&nbsp;&nbsp;  T1 := Number,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; T0 := Number,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;T#t := Boolean,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Tnum1 := Number,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Tnum2 := Number,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Tnum3 := Number,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; T+ := [Number * Number -> Number]&nbsp;&nbsp;&nbsp; }


The type inference succeeds, meaning that the expression is  **well typed**.
Because there are no free variables, the inferred type of T0 is:  **Number**.

## Question 3

Typing the application `((lambda (f1 x1) (f1 x1)) 4 sqrt)`:

**Stage I**: Rename bound variables.

`((lambda (f1 x1) (f1 x1)) 4 sqrt)`  turns into  `((lambda (f x) (f x)) 4 sqrt)`

**Stage II**: Assign type variables for every sub expression and primitives:

|Expression	               | Variable
|-------------------------------|------------|
((lambda (f x) (f x)) 4 sqrt)   |T0
(lambda (f x) (f x))	        |T1
(f x)	                        |T2
f	                            |Tf
x	                            |Tx
4	                            |Tnum4
sqrt	                        |Tsqrt

**Stage III**: Construct type equations.

 The equations for the sub-expressions are:

|Expression               	    | Equation
|-------------------------------|-----------------------------|
|((lambda(f x) (f x)) 4 sqrt)   |T1 = [Tnum4 * Tsqrt -> T0]
|(lambda(f x) (f x))	        |T1 = [Tf * Tx -> T2]
|(f x)                     	    |Tf = [Tx -> T2]

The equations for the primitives are:

|Expression               	    | Equation
|-------------------------------|-----------------------------|
|4	                            |Tnum4 = Number
|sqrt	                        |Tsqrt = [Number -> Number]

**Stage IV**: Solve the equations.
|Equation                       | Substitution
|-------------------------------|-----------------------------|
|1. T1 = [Tnum4 * Tsqrt -> T0]	|{}
|2. T1 = [Tf * Tx -> T2]	    |
|3. Tf = [Tx -> T2]	            |
|4. Tnum4 = Number	            |
|5. Tsqrt = [Number -> Number]  |	

**Step 1:**  

`(T1 = [Tnum4 * Tsqrt -> T0]) ○ Substitution = (T1 = [Tnum4 * Tsqrt -> T0])`, type-sub.  

`Substitution = Substitution ○ (T1 = [Tnum4 * Tsqrt -> T0]).`


|Equation| Substitution
|-------------------------------|-----------------------------|
|2. T1 = [Tf * Tx -> T2]	    |{ &nbsp;&nbsp;&nbsp; **T1 := [Tnum4 * Tsqrt -> T0]** &nbsp;&nbsp;&nbsp;}
|3. Tf = [Tx -> T2]	            |
|4. Tnum4 = Number	            |
|5. Tsqrt = [Number -> Number]  |	

**Step 2:**

(T1 = [Tf * Tx -> T2]) ○ Substitution = ([Tf * Tx -> T2] = [Tnum4 * Tsqrt -> T0])

There is no type-sub. We split the equation to

* Tf = Tnum4, 
* Tx = Tsqrt, 
* T2 = T0

We add them to the equations (6,7,8)  and remove equation 2.

|Equation| Substitution
|-------------------------------|-----------------------------|
|3. Tf = [Tx -> T2]  	        |{ &nbsp;&nbsp;&nbsp; T1 := [Tnum4 * Tsqrt -> T0] &nbsp;&nbsp;&nbsp;}
|4. Tnum4 = Number	            |
|5. Tsqrt = [Number -> Number]  |	
|6.  **Tf = Tnum4**	            |
|7.  **Tx = Tsqrt**	            |
|8.  **T2 = T0**	            |


**Step 3:**

(Tf = [Tx -> T2]) ○ Substitution = (Tf = [Tx -> T2]) , type-sub.
Substitution = Substitution ○ (Tf = [Tx -> T2]).

|Equation| Substitution
|-------------------------------|-----------------------------|
|4. Tnum4 = Number	            |  { &nbsp;&nbsp;&nbsp; T1 := [Tnum4 * Tsqrt -> T0], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  **Tf = [Tx -> T2]** &nbsp;&nbsp;&nbsp;}
|5. Tsqrt = [Number -> Number]  |	
|6.  Tf = Tnum4	                |
|7.  Tx = Tsqrt                 |
|8.  T2 = T0	                |

**Step 4:**  `(Tnum4 = Number) ○ Substitution = (Tnum4 = Number)`, type-sub.

Substitution = Substitution ○ (Tnum4 = Number).

|Equation| Substitution
|-------------------------------|-----------------------------|
|5. Tsqrt = [Number -> Number]  |	  { &nbsp;&nbsp;&nbsp; T1 :=[**Number** * Tsqrt -> T0], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Tf = [Tx -> T2], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; **Tnum4 = Number** &nbsp;&nbsp;&nbsp;}
|6.  Tf = Tnum4	                |
|7.  Tx = Tsqrt                 |
|8.  T2 = T0	                |


**Step 5:**

(Tsqrt = [Number -> Number]) ○ Substitution = (Tsqrt = [Number -> Number]) , type-sub.
Substitution = Substitution ○ (Tsqrt = [Number -> Number]).


|Equation| Substitution
|-------------------------------|-----------------------------|
|6.  Tf = Tnum4	                |  { &nbsp;&nbsp;&nbsp; T1 :=[Number * **[Number -> Number]**-> T0], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  Tf = [Tx -> T2], <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Tnum4 = Number <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  **Tsqrt := [Number->Number]** &nbsp;&nbsp;&nbsp;}
|7.  Tx = Tsqrt                 |
|8.  T2 = T0	                |


**Step 6:**  
`(Tf = Tnum4) ○ Substitution = ([Tx -> T2] = Number)`  

We get the conflicting equation:

`[Tx -> T2] = Number`  and we can say that the expression is  **not well typed.**


