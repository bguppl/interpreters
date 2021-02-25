# Vectorizing Loops: Random Walk Example with Accumulate
## PPL 2021 - Week #1

This example is taken from: [http://www.labri.fr/perso/nrougier/from-python-to-numpy/](http://www.labri.fr/perso/nrougier/from-python-to-numpy/).

To demonstrate the performance advantage of the FP paradigm, we show an example written in Python. Python is similar to JavaScript in the sense that it is a multi-paradigm language - supporting procedural, object-oriented and functional programming. In this example, we exploit Python's FP features.

Let's take a slightly more interesting example than map(cube): a random walk. A random walk is an important tool in statistics that is used to compute approximations of samplers for complex distributions. (See [https://en.wikipedia.org/wiki/Random_walk](https://en.wikipedia.org/wiki/Random_walk))

(In particular, random walks can be used to derive an efficient approximation of the PageRank algorithm which is used by Google to rank search results - see [https://en.wikipedia.org/wiki/PageRank](https://en.wikipedia.org/wiki/PageRank)).

## Object-oriented Approach

One possible object-oriented approach would be to define a `RandomWalker` class and to write a `walk` method that would return the current position after each (random) steps. It's nice, it's readable, but it is slow:

```python
import random

class RandomWalker:
    # Constructor
    def __init__(self):
        self.position = 0

    # Walk n steps forward from 0 - each step updates the state of the object with a random increment.
    def walk(self, n):
        self.position = 0
        for i in range(n):
            yield self.position
            self.position += 2 * random.randint(0, 1) - 1
```

The `RandomWalker` object encapsulates the state of the object, which contains a single field `position`.
The method `walk()` describes how to advance to the next step in a stochastic manner:

$$position_{n+1} = position_{n} + 2 \times random(0,1) - 1$$

This relation means that at each step, we randomly select a direction $$+1$$ or $$-1$$ and move left or right
accordingly.  After $$n$$ steps, we observe the position reached.

We will explain the behavior of `yield` in Chapter 4, when introducing advanced control flow (lazy evaluation and generators).  For now, think of `yield` as a way to do `return`, but when the function is invoked next time, it will continue on the line after the `yield`.

Let us test this OO version:

```python
%%timeit
walker = RandomWalker()
walk = [position for position in walker.walk(1000)]
```

```
1.01 ms ± 85.2 µs per loop (mean ± std. dev. of 7 runs, 1000 loops each)
```

## Functional Approach

For such a simple problem, we can probably save the class definition and concentrate only on the walk method that computes successive positions after each random step.

Instead of generating positions one at a time on the basis of the state of the object, this approach accumulates the successive positions into a list.

```python
def random_walk(n):
    position = 0
    walk = [position]
    for i in range(n):
        position += 2 * random.randint(0, 1) - 1
        walk.append(position)
    return walk
```

This new method saves some CPU cycles but not that much because this function is pretty much the same as in the object-oriented approach and the few cycles we saved probably come from the inner Python object-oriented machinery.

```python
%%timeit
random_walk(n=1000)
```

```
916 µs ± 54.3 µs per loop (mean ± std. dev. of 7 runs, 1000 loops each)
```

This function is in fact not very *functional* - it uses a loop and it mutates the `walk` local variable.

## Vectorized Approach

But we can do better using the itertools Python module that offers a set of functions creating iterators for smart looping. 

If we observe that a random walk is an *accumulation of steps*, we can rewrite the function as follows:

First observe that we can sample all the decisions to move left or right all at once, using the `sample` method.
`sample(list, n)` returns `n` random elements out of `list` (which is expected to have more than `n` elements).
`[1, -1] * n` returns the list `[1, -1]` repeated `n` times.

```python
import random
random.sample([1, -1] * 10, 10)  # ==> [-1, -1, 1, 1, -1, 1, 1, -1, -1, -1]
```

```python
from itertools import accumulate

def random_walk_faster(n=1000):
    steps = random.sample([1, -1] * n, n)
    return list(accumulate(steps))
```

Note that this function does not use mutation anymore - we initialize the local variable steps with a list of the expected size (`n` elements).

Instead of looping for picking sequential steps and add them to the current position, we first generated all the steps at once and used the `accumulate` function to compute all the positions.

`accumulate` is the third most useful array or list operator most commonly used in FP (after `map` and `filter`) - it consists of reducing a list of values to a single value, by applying the same operation successively.  In our case, `accumulate([1, 2, 3])` computes the sum `0 + 1 + 2 + 3` (where 0 is the neutral element of the addition). Naturally, we can use `accumulate` to perform other operations than `+` and start with other values than `0`.

In fact, we've just vectorized our function. We got rid of the loop and this makes things faster:

```python
%%timeit
random_walk_faster(n=1000)
```

```
499 µs ± 19.6 µs per loop (mean ± std. dev. of 7 runs, 1000 loops each)
```

We gained ~50% of computation-time compared to the previous version, not so bad. 

But the advantage of this new version is that it makes numpy vectorization super simple. We just have to translate `itertools` call into `numpy` ones.


```python
import numpy as np

def random_walk_fastest(n=1000):
    steps = 2 * np.random.randint(0, 2, size=n) - 1
    return np.cumsum(steps)
```

Not too difficult, but we gained a factor ~500x using NumPy:


```python
%%timeit
random_walk_fastest(n=1000)
```

```
31 µs ± 3.72 µs per loop (mean ± std. dev. of 7 runs, 10000 loops each)
```

This specific example demonstrates the performance benefit of vectorization. 

The key take-away message though, is that because the functional version of the program is declarative (it indicates what we intend to compute, not exactly how to compute it), we can decide at runtime to use different, more effective ways to compute the process described by the code, that take advantage of the processors and of concurrency.
