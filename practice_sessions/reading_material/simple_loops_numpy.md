# Benchmarking Loops in Python
## PPL 2020 - Week #1

This notebook demonstrates the performance advantage obtained through vectorization on a loop.
We compare a procedural implementation of a loop with mutation, a slightly better version using Python's list comprehensions, and a vastly superior vectorized version using Numpy's arrays and broadcast operations.

Our requirement is to compute the cube of all elements in a list of numbers of size $$n$$.
We first implement the Python version of the same function we reviewed in class:


```python
n = 100

def cube(x):
    return x * x * x
```

## Procedural Loop

```python
def cubes_procedural(a):
    res = list(a)  # Copy the list
    for i in range(len(a)):
        res[i] = cube(a[i])
    return res
```

The function first copies the input parameter, then fills it with the required values.

Let us test the function:

```python
s = list(range(10))
cubes_procedural(s)  # ==> [0, 1, 8, 27, 64, 125, 216, 343, 512, 729]
```

Let us now benchmark this function - using the IPython magic `%%timeit` command:

```python
%%timeit 
for i in range(n):
    cubes_procedural(range(i))
```

```
1.23 ms ± 51.7 µs per loop (mean ± std. dev. of 7 runs, 1000 loops each)
```

## Map

The procedure could be improved - both in terms of readability, style and performance by using Python's *map* function. Python's `map` function receives the same parameters as in TypeScript - a function and a list (or more generally an iterator) - and it returns an iterator.  (We will learn later that `map` is a *lazy* function, it does not compute the resulting list immediately, only when we ask to construct a list out of the iterator, we obtain the resulting list).

```python
map(cube, [1, 2, 3])  # ==> <map at 0x28a542e29b0>
```

To force the evaluation of the result, we copy the resulting lazy iterator into a list:

```python
list(map(cube, [1, 2, 3]))  # ==> [1, 8, 27]
```

```python
def cubes_map(a):
    return list(map(cube, a))
```

Let us benchmark the performance of `map` to compare it with the procedural loop:

```python
%%timeit
for i in range(n):
    cubes_map(range(i))
```

```
579 µs ± 20.1 µs per loop (mean ± std. dev. of 7 runs, 1000 loops each)
```

The `map` version reduces the time of the test from ~1.23ms to ~600 µs. This is mainly because `map` is implemented as a primitive function instead of interpreting the loop execution.

## Vectorization

We can improve this more significantly by using *vectorizing* - supported in Python by the NumPy package.
NumPy offers a highly optimized *array* data structure together with mechanisms to *broadcast* functions over arrays - that is, basically perform a `map` of a function elementwise on arrays of numbers.


```python
import numpy as np

# Use a NumPy Array instead of a list
a = np.arange(100)
a
```

```
array([ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,
       17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
       34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
       51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67,
       68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
       85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99])
```


NumPy interprets the application of a function to an array as a map of the function elementwise.
This is called *broadcasting* a method to an array.


```python
cube(a)
```

```
array([     0,      1,      8,     27,     64,    125,    216,    343,
          512,    729,   1000,   1331,   1728,   2197,   2744,   3375,
         4096,   4913,   5832,   6859,   8000,   9261,  10648,  12167,
        13824,  15625,  17576,  19683,  21952,  24389,  27000,  29791,
        32768,  35937,  39304,  42875,  46656,  50653,  54872,  59319,
        64000,  68921,  74088,  79507,  85184,  91125,  97336, 103823,
       110592, 117649, 125000, 132651, 140608, 148877, 157464, 166375,
       175616, 185193, 195112, 205379, 216000, 226981, 238328, 250047,
       262144, 274625, 287496, 300763, 314432, 328509, 343000, 357911,
       373248, 389017, 405224, 421875, 438976, 456533, 474552, 493039,
       512000, 531441, 551368, 571787, 592704, 614125, 636056, 658503,
       681472, 704969, 729000, 753571, 778688, 804357, 830584, 857375,
       884736, 912673, 941192, 970299])
```

```python
%%timeit 
for i in range(n):
    cube(np.arange(i))
```

```
184 µs ± 16.7 µs per loop (mean ± std. dev. of 7 runs, 1000 loops each)
```

Vectorization improved performance by a factor of close to 5.

The way vectorization works is that it exploits hardware parallelism: because all the work performed is trivially parallel, the interpreter is free to split the array into as many blocks as there are available cores in the CPU, and execute a subset of the work on each of the blocks in parallel.  There is no need to lock anything as the function that is applied (`cube`) is a pure function with no side effect, and the blocks are non-overlapping.

The benefits of vectorization increase as the size of the arrays increase and the number of cores in the hardware increases.
On GPUs, one can exploit thousands of cores and obtain significant speedups when adopting this style of programming.
This is on the key ingredients that has recently enabled significant progress in Artificial Intelligence and Deep Learning.

From a programming language perspective - vectorization is made possible because the language encourages a declarative style which is based on the Functional Programming paradigm, and the usage of pure functions with no side effects.
