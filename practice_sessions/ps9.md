# Asynchronous Programming in TypeScript

We exercise techniques of asynchronous programming in TypeScript.
* How to invoke Node methods that take a callback parameter - such as `fs.writeFile` or `http.get`.
* How to process the callback methods when we want to compose successive / conditional calls inside callbacks.
* How promises help to compose asynchronous calls.
* How the `async` and `await` syntax simplifies promises.

## The Callback

One way to handle asynchronous operations in a programming language
is to provide a _callback_ function as a parameter.
The callback function would then be called when the asynchronous operation ended.

```typescript
import fs from 'fs';

fs.writeFile('hello_world.txt', 'Hello World!', function (err) {
    if (err) {
        console.log(err);
    } else {
        console.log('Wrote "Hello World" to hello_world.txt');
    }
});
```

This approach has two issues:

1) Callbacks are not standardized.
   <br><br>
   With different APIs the callback is expected to be either the last parameter, 
   the first parameter, a property on a config parameter, 
   a property set on the calling object, etc., 
   <br><br>
   In some conventions the first parameter was expected to be either an `Error` or `null` and the second parameter was the result of the asynchronous computation,
   while with other conventions it was the order was reversed, another convention 
   returned a compound object containing error or result, etc.,
<br><br>
2) The second problem, nicknamed "callback hell".
   <br><br>
    It's hard to write and read composed asynchronous operations. Complex flows that involve a few asynchronous operations quickly becomes 
hard to write and even harder to read. For example:

```typescript
function nestedSequentialRequests(baseUrl, studentName) {
    let args = {
        headers: { "Content-Type":" application/json"},
    };
    request.get(baseUrl+"/posts", args, function(err, httpResponse, body) {
        if (err || httpResponse.statusCode >= 400) {
            console.error('something went wrong on the first request:', err);
        } else {
            // Process the return value of the first GET request: extract max(id)
            // console.log("Server response: ", body);
            let students = JSON.parse(body),
                maxStud = students.reduce(((prev, cur)=> (Number(cur.id) > Number(prev.id)) ? cur : prev), {id:0});
            console.log("Current max Id = ", maxStud.id);
            let newStud = {id: Number(maxStud.id) + 1, title: studentName, author: "st"+(Number(maxStud.id)+1)};

            // Submit a second request
            let args = {
                url: baseUrl+"/posts",
                headers: { 'Content-Type': 'application/json' },
                form: newStud
            };
            request.post(args, function(err, httpResponse, body) {
                if (err || httpResponse.statusCode >= 400) {
                    console.error('something went wrong in the second request:', err)
                }  else {
                    console.log("Created student: ", newStud);
                    console.log("Server response: ", body);
                }
            });
        }
    });
};
```

So how did we compose callbacks?

We had to **nest** the callbacks.  We start from the first method that we want to invoke.  Then in the callback of the first method, we invoke the second function.
That is, instead of `f(g(x))` - we used the following pattern:

```
g(x, (gRes) => f(gRes))
```

where the function `(gRes) => f(gRes)` is the callback to g.

**NOTE**: Observe that the order of the functions is inverted from regular composition, instead of `f(g(x))` where `f` appears first and `g` second,
we write `g` first, then `f` - which reflects the order in which the functions are invoked.

## Promises to the rescue

As we have observed in the example above, it is not practical to combine asynchronous functions.
* It is difficult to compose/"pipe" functions into each other
* It is difficult to handle errors and propagate them

[Promises](http://exploringjs.com/es6/ch_promises.html) are a technique that was introduced to make asynchronous composition more convenient.

A Promise object is a proxy for the result we expect to receive in the future, which is delivered via that object.
Because it takes time to obtain the value (when the promise encapsulates an asynchronous computation) - the promise may be
in several states:
* Before the result is ready, the Promise is pending.
* If a result is available, the Promise is fulfilled.
* If an error happened, the Promise is rejected.

A Promise is **settled** if the computation it represents has completed - it is either fulfilled or rejected.
A Promise is settled exactly once and then remains unchanged (it is "set" only once).

### Reacting to Promise State Changes

Promise reactions are callbacks that you register with the Promise method then(), to be notified of a fulfillment or a rejection.

A **thenable** is an object that has a Promise-style then() method.

Changing states: There are two operations for changing the state of a Promise - `resolve()` and `reject()`.
After either one of them is invoked once, further invocations have no effect.
* Rejecting a Promise moves the Promise to the *rejected* state.  It means the encapsulated computation has raised an error.
* Resolving a Promise has different effects, depending on what value you are resolving with:
    * Resolving with a normal (non-thenable) value fulfills the Promise - the promise moves to state *fulfilled*.
    * Resolving a Promise P with a thenable T means that P will now continue with T’s computation and state, including its fulfillment or rejection value. The appropriate P reactions (callbacks) will get called once T settles (or are called immediately if T is already settled).

### Building and Using Promises

The following function returns a result asynchronously, via a Promise:

This is a factory for a Promise object - it receives as a parameter the functions resolve and reject to involve in case the requested computation completes or throws an error:

```typescript
function asyncFunc() {
    return new Promise(
        function (resolve, reject) {
            // ···
            resolve(result);
            // ···
            reject(error);
        });
}
```

We call asyncFunc() as follows:

```typescript
asyncFunc()
  .then(result => { /* ··· */ })
  .catch(error => { /* ··· */ });
```

### Chaining http Requests with Promises

Let us re-implement the example above with requests encapsulated as Promises.

We will use a http-request module which produces a Promise object when we invoke it:

> note that promise based http requests have been standardtized in the browser and are coming to node (currently experimental in node 18)

It is installed with npm:

```
npm install request-promise-native --save
```

We first simply rewrite the callback version using promises instead of callbacks:

```typescript
// Request Promise Native
const rpn = require('Request-Promise-Native');

// get request using request-promise-native
function createStudentPromise(baseUrl: string, studentName: string) {
    // GET request 
    const request = {
        method: 'GET',
        uri: baseUrl+"/posts",
        headers: {"Content-Type":" application/json"},
    };
    rpn(request)
        .then((data) => {
          let students = JSON.parse(data),
              maxStud = students.reduce(((prev, cur)=> (Number(cur.id) > Number(prev.id)) ? cur : prev), {id:0});
          console.log("Current max Id = ", maxStud.id);
          let newStud = {id: Number(maxStud.id) + 1, title: studentName, author: "st"+(Number(maxStud.id)+1)};
          console.log('Newstud = ', newStud);
          // Submit a second request
          let args = {
                method: 'POST',
                url: baseUrl+"/posts",  
                headers: { 'Content-Type': 'application/json' },
                form: newStud
          };
          rpn(args)
            .then((data)=> {
                    console.log("Created student: ", newStud);
                    console.log("Server response: ", data);
            })
            .catch((err)=> console.error('something went wrong in the second request:', err));
        })
        .catch(function (err) {
            console.error('something went wrong on the request: Error'); 
        });
        
    // this will be executed *before* the callback is executed
    console.log("GET submitted to retrieve list of students");
};
```

### Chaining instead of Nesting

The version above has the same nested structure as the one we observed with callbacks - it did not improve pn this respect - we require an embedded level for each step in a sequence of operations, and the error handling branches are also nested inside the callbacks.

The solution to avoid such nesting is to return a Promise from inside the then() of another promise.
When this is achieved, we can then benefit from the chaining capabilities of promises:

```typescript
// get request using request-promise-native
function createStudentPromiseChain(baseUrl: string, studentName: string) {
    // GET request 
    const request = {
        method: 'GET',
        uri: baseUrl+"/posts",
        headers: {"Content-Type":" application/json"},
    };
    rpn(request)
        .then((data) => {
          let students = JSON.parse(data),
              maxStud = students.reduce(((prev, cur)=> (Number(cur.id) > Number(prev.id)) ? cur : prev), {id:0});
          let newStud = {id: Number(maxStud.id) + 1, title: studentName, author: "st"+(Number(maxStud.id)+1)};
          // Submit a second request
          let args = {
                method: 'POST',
                url: baseUrl+"/posts",  
                headers: { 'Content-Type': 'application/json' },
                form: newStud
            };
            return rpn(args);  // **** This is the key change
        })
        .then((data) => {       // **** This second then() is chained at the same level as the first
            console.log("Server response: ", data);
        })
        // All error handlers can be chained - error will be propagated
        .catch((err) => console.error('something went wrong in a request:', String(err).substring(0,200)+'...'));
};
```

When we return promises instead of nesting them, we obtain code whose structure reflects the sequence of calls we intend.
Error handling can also be centralized into a single `catch` handler instead of repeated for each invocation.

As a matter of style, we understand that we should disentangle the code that creates the asynchronous tasks from the code that processes the values the tasks return.  

```typescript
// Separate simple functions which generate promises
// from a function that combines the promises together

function getStudents(baseUrl: string) {
    const request = {
        method: 'GET',
        uri: baseUrl+"/posts",
        headers: {"Content-Type":" application/json"},
    };
    return rpn(request);
}

function createStudent(baseUrl: string, student: Student) {
    let request = {
        method: 'POST',
        url: baseUrl+"/posts",  
        headers: { 'Content-Type': 'application/json' },
        form: student
    };
    return rpn(request);
}

function createStudentChain(baseUrl: string, studentName: string) {
    getStudents(baseUrl)
    .then(data => {
        let students = JSON.parse(data),
            maxStud = students.reduce(((prev, cur)=> (Number(cur.id) > Number(prev.id)) ? cur : prev), {id:0}),
            newStud = {id: Number(maxStud.id) + 1, title: studentName, author: "st"+(Number(maxStud.id)+1)};
          return createStudent(baseUrl, newStud);
    })
    .then(data => {
        console.log("Student created: ", data);
    })
    .catch(err => {
        console.error("Something went wrong: ", String(err).substring(0,100)+'...');
    });
};
```

### Higher Order Callbacks Combinations

**Question**: how do we use promises to iterate over a list of values and perform an asynchronous operation on each value?

Let us consider an example: we want to delete all the items in the server that have an id larger than 10.

One solution would be to chain the calls to the operation.

```typescript
function deleteStudent(baseUrl: string, id: string) {
    const request = {
        method: 'DELETE',
        uri: baseUrl+"/posts/"+id,
        headers: {"Content-Type":" application/json"},
    };
    console.log("Submitted delete request for ", id);
    return rpn(request);
}

function deleteStudentsWithLargeId(baseUrl: string) {
    return getStudents(baseUrl)
        .then(data => {
            let students = JSON.parse(data);
            students.forEach(s => s.id > 10 ?
                deleteStudent(baseUrl, s.id).then(data => console.log("Student ", s.id, " is deleted")) :
                console.log("Student ", s.id, " is not deleted."));
        })
        .catch(err => console.error("Error :", err));
}
```

* Do the delete operations above happen in sequence or in parallel?
* Does the `deleteStudentsWithLargeId` return after all students have been deleted?

**Question**: How can we force the asynchronous calls to be performed one after the other with no overlap _and_ make sure to wait until the end of all operations?

**Answer**: We can invoke the subsequent asynchronous functions in the scope of the then() resolution of the previous calls.

```typescript
function createManyStudents(baseUrl: string, names: string[]) {
    if (names.length === 0) {
        return Promise.resolve();
    } else {
        return getStudents(baseUrl)
        .then(data => {
            let students = JSON.parse(data),
                maxStud = students.reduce(((prev, cur)=> (Number(cur.id) > Number(prev.id)) ? cur : prev), {id:0}),
                newStud = {id: Number(maxStud.id) + 1, title: names[0], author: "st"+(Number(maxStud.id)+1)};
            return createStudent(baseUrl, newStud);
        })
        .then(data => {
            console.log("Student created: ", data);
            names.shift();
            return createManyStudents(baseUrl, names);     // Invoke the next only after the previous has completed.
        })
        .catch(err => {
            console.error("Something went wrong: ", err);
        });
    }
}
```

We can also run the operations in parallel using `Promise.all` which accepts and array of promises
and resolved when all of them are resolved.

```typescript
function deleteStudentsWithLargeId(baseUrl: string) {
    return getStudents(baseUrl)
        .then(data => {
            let students = JSON.parse(data);
            
            const deleteRequests = students.filter(s => {
                if (s.id > 10) {
                    return true;
                } else {
                    console.log("Student ", s.id, " is not deleted.")
                    return false;
                }
            }).map(s => deleteStudent(baseUrl, s.id)
                       .then(data => console.log("Student ", s.id, " is deleted"))
            )
            return Promise.all(deleteRequests)
        })
        .catch(err => console.error("Error :", err));
}
```

## Better promises with the `async`/`await` syntax

In newer version of Javascript you can use the `async` keyword to declare that a function that always return a promise
and use the `await` keyword be for an expression that evaluates to a promise to indicate not to continue execution until that promise is resolved (similar to `Promise.prototype.then`).
When that awaited promise is rejected an exception is thrown (similar to `Promise.prototype.catch`).

This special syntax enables us to write a promise based code that look more natural. It is important to note that underneath the syntax the same `Promise` object are being used conceptually.

Here is an example of the `async` syntax alone:

```typescript
async function greeting1() {
    return 'Hello, world!';
}

const greeting2 = async () => 'Hello, World';

// in both cases the type is () => Promise<string>
// so we cant just use the value 
// console.log(greeting1) will print a Promise object
// we need to do the following
greeting1().then(s => console.log(s))
```

The `await` is only valid within an `async` function:

```typescript
async function greeting3() {
    const hello = await new Promise((resolve) => {
        setTimeout(() => resolve('Hello'), 1000)
    });
    const world = await new Promise((resolve) => {
        setTimeout(() => resolve('world'), 1000)
    });
    return `${hello}, ${world}!`
}
```

```typescript 
async function greeting4() {
    const [hello, world] =  await Promise.all([
        new Promise((resolve) => {
            setTimeout(() => resolve('Hello'), 1000)
        }),
        new Promise((resolve) => {
            setTimeout(() => resolve('world'), 1000)
        })
    ]);
    return `${hello}, ${world}!`
}
```

**Question:** The functions above execution is sequential or parallel?


Here is our createManyStudents function written using `async` / `await`:

```typescript
async function createManyStudents2(baseUrl: string, names: string[]) {
    if (names.length === 0) {
        return;
    } else {
        try {
            const data = await getStudents(baseUrl);
            const students = JSON.parse(data);
            const maxStud = students.reduce(((prev, cur) => (Number(cur.id) > Number(prev.id)) ? cur : prev), {id: 0});
            let currStudId = Number(maxStud.id) + 1
            for (const name of names) {
                newStud = {id: currStudId, title: name, author: `st${currStudId}`};
                await createStudent(baseUrl, newStud);
                currStudId++;
            }
        } catch (err) {
            console.error("Something went wrong: ", err);
        }
    }
}
```


## A few notes on iterator and Generator types


In the lecture you saw generator function, that can used inside loops similarly to arrays (but can have custom logic, be infinite etc.,)

```typescript
function * count3() {
    yield 1;
    yield 2;
    yield 3;
}
```

we know that we can iterate over count3() 
```typescript
for (const n of count3()) {
    console.log(n)
}
```

But what exactly did count3() return?

<br><br>

To understand this we need the following two protocols:

First, *JavaScript* defines the following very general **iterator protocol**:

```typescript
interface IteratorResult<T> {
    value: T;
    done: boolean;
}
interface Iterator<T> {
    next(): IteratorResult<T>;
}
```

The second protocol we need the **iterable protocol**. 
In JavaScript any object can hook into the `for-of` construct (and the `...` spread syntax)
by adding a special function name `Symbol.iterator` that returns an iterator. This is called the iterable protocol:

```typescript
interface Iterable<T> {
    [Symbol.iterator](): Iterator<T>;
}
```

The generator functions that you saw in the lectures act in the same manner. 
We can iterate over their return type because they return an iterable object, more specifically the return type of a generator is both an iterator and an iterable (that returns the same iterator)

***Question:*** Can you write the return type of the `count3` function?


<br><br><br><br><br>



```typescript
interface Generator<T> extends Iterator<T> {
    [Symbol.iterator](): Generator<T>;
}

interface GeneratorFunction {
    (...args: any[]): Generator;
}
```

That is, a generator function (`function *`) returns an *iterator* that also adheres to the *iterable* protocol thus can be used with `for-of` and `...`. Its `Symbol.iterator`  returns the generator itself.



