# TypeScript: Complex Data Types, JSON, Map, Filter, Reduce

## PPL 2023 - [Course Website](https://bguppl.github.io/interpreters/)
## Practical Session - Week #1

### How to Install Node.js and NPM on Windows

Node.js is a JavaScript interpreter. NPM (Node Package Manager) let's you install software (libraries, plugins, frameworks and applications) used to build Node applications. Both are installed using the installer package avaiable from the Node.js web site. The isntallation is pretty straightforward: 

1. Download the LTS (version 16) Windows installer from [nodejs.org](https://nodejs.org/en/)
2. Run the installer (the .msi file you downloaded in the previous step.)
3. Follow the prompts in the installer (Accept the license agreement, click the NEXT button a bunch of times and accept the default installation settings).
4. Restart your computer.

#### Test the installation: 
* Open the Windows Command Prompt (press the Win + R keys. Then, type `cmd` and press Enter).
* To verify node is installed, type `node -v`, then press Enter. You should see a version number (should start with 'v16.').
* To verify NPM installation, type `npm -v` and press Enter. You should see a version number (should start with '8.').

#### More information:
For instructions on how to install other software that will be used throughout the course, please refer to the [useful links](https://bguppl.github.io/interpreters/useful_links.html) section in the course web site.  In particular, see instructions on how to install the TypeScript compiler `tsc` and the Visual Studio Code IDE.

## 1. Types of Values

A **data-type** is a classification of data which indicates what the programmer intends to do with the data. Types correspond to a set of values (eg. the boolean type corresponds to {true,false}, number type corresponds to the infinite set of numeric values), and they define the operations that can be done on the data (e.g. booleans can be computed using logical operators).

We pay attention to two distinguishable defintions: The **type of a value** (atomic/compound) and the **type of a variable**. The type of a variable may be declared by the programmer of inferred by the type of the value it stores. The type of a value is determined by a defined group to which it belongs. The type of a variable may be different than the type of the value it stores, but they must be compatiable.

Apart from atomic types (number, string, boolean), there are **compound types**. Javascript introduces two basic compound types: Arrays and Maps. In Javascript, compound types are called *object* types (note: not to be confused with the more standard term - *object* here simply refers to any value which is not an atomic type)

##### Arrays

Arrays are the first basic compound type in Javascript. Here is an example of using arrays:


```typescript
// This is how we declare an array containing the elements 16,8,27,13
let a = [16, 8, 27, 13];

// Here's how we use the Array compound value getter:
console.log(a[3]); // ==> 13

// There is also a slice method:
console.log(a.slice(1, 4)); // ==> [ 8, 27, 13 ]
```


Let's check that it is a compound type:


```typescript
console.log(typeof a); // ==> 'object'
```

And now the more specific reflection operator: 


```typescript
console.log(a instanceof Array); // ==> true
```

The array we defined above is **homogeneous**, meaning that it contains a single data type. 
The **homogeneous** array `a` above contains values whose types are `number`. 
Arrays in Javascript can be heterogeneous as well: 


```typescript
let manyArray = [1, 'a', '2', "The Dawn", true];

console.log(`manyArray = ${manyArray}`); // ==> manyArray = 1,a,2,The Dawn,true
```

##### Maps and `Object.keys`

Maps are the second basic compound type in Javascript:

```typescript
// This is how we declare them
let map = { a: 1, b: 2 };

// This is how we use the Map compound value getter for the values:
console.log(map['a']); // ==> 1
console.log(map.a); // ==> 1
console.log(map['b']); // ==> 2
console.log(map.b); // ==> 2

// And for keys (relevant for both Arrays [the keys there are the indexes] and Maps)
console.log(Object.keys(a)); // ==> [ '0', '1', '2', '3', '4' ]
console.log(Object.keys(map)); // ==> [ 'a', 'b' ]
```

##### Variable Types

Javascript is a *dynamic language*, meaning that variables are not typed. But when a variable is bound to a value, we can inspect its type at runtime. 

Typescript extends Javascript to introduce optional variable types - it is compiled into Javascript and at compilation time, type checking is performed. Let's recall the example from the lectures: 


```typescript
let typedVarNum: number = 6;
let typedVarStr: string = "blue";
let typedVarBool: boolean = true;
```

## 2. JSON

### 2.1 JSON Review

JSON stands for JavaScript Object Notation and is a standard way to serialize Javascript compound values into strings (and vice-versa). It is widely used to allow data exchange in server-client communication. It serves a role that is similar to XML.

JSON is a **notation** - that is, a way of writing Javascript values in a way that allows reading them back easily. 
Most Javascript values can be written in JSON - but not all of them.

Once you have a Javascript object, and once there is an agreed-upon-way of "stringifying" the object (in our case, the standard way is `JSON.stringify()`), then you can easily pass this string through the network, from a computer to a computer, as passing strings through the network is easy, and the receiving side can only do an agreed-upon way to parse the string into an object (in our case, the standard way is `JSON.parse()`).

The JSON interface defines the following two methods: 
1. `JSON.stringify(o)` - maps a value to a string
2. `JSON.parse(s)` - maps a string written according to the JSON syntax to a value

JSON serves a similar goal to that of `Serializable` of Java that we saw in SPL.


```typescript
let person1 = { name : "Yosi", age : 31, city : "Beer Sheva" };
let person1JSONString = JSON.stringify(person1);

console.log(`person1 serialized in JSON = ${person1JSONString}`); // ==> person1 serialized in JSON = {"name":"Yosi","age":31,"city":"Beer Sheva"}
console.log(`person1JSONString is of type ${typeof person1JSONString}`); // ==> person1JSONString is of type string
```

```typescript
let person2 = JSON.parse(person1JSONString);

console.log(person2); // ==> { name: 'Yosi', age: 31, city: 'Beer Sheva' }
console.log(`person2 is of type ${typeof person2}`); // ==> person2 is of type object
```

Note that the way values are written in JSON is not exactly the same as we write them in a Javascript program - there are quotes ("") around keys for example.

Note also that `stringify()` and `parse()` work on atomic values as well:


```typescript
// note that the input to JSON.parse is a JSON string
console.log(typeof JSON.parse('2')); // ==> number
console.log(typeof JSON.parse('true')); // ==> boolean
console.log(typeof JSON.parse('"abc"')); // ==> string
```

The values that can be encoded into JSON (let us call them "JSON values") must be one of the following types:
1. `string`
2. `number`
3. `boolean`
4. `null`
5. a map where all the keys are strings and all the values are JSON values (recursively) 
6. an array where all the values are JSON values

The Javascript compound data types, *arrays* and *maps*, can be combined in a recursive manner. 
This applies to JSON as well.

The Javascript values that **cannot be encoded** into JSON strings are `undefined` and functions.

Let us look at an example of a complex value and how we can describe it in JSON and then how using types can help us describe its structure in a well-documented manner:


```typescript
let studentsData = {
    department: "Computer Science", 
    students: [
        { name: "Alice", degree: "PhD" }
        { name: "Bob", degree: "MSc" },
    ]
};

let studentsJSON = JSON.stringify(studentsData);
console.log(studentsJSON); // ==> {"department":"Computer Science","students":[{"name":"Alice","degree":"PhD"},{"name":"Bob","degree":"MSc"}]}
console.log(`studentsJSON is of type ${typeof studentsJSON}`); // ==> studentsJSON is of type string
let studentsJSONParsed = JSON.parse(studentsJSON);
console.log(studentsJSONParsed);
// ==> 
// {
//   department: 'Computer Science',
//   students: [ { name: 'Alice', degree: 'PhD' }, { name: 'Bob', degree: 'MSc' } ]
// }
console.log(`studentsJSONParsed is of type ${typeof studentsJSONParsed}`); // ==> studentsJSONParsed is of type object
```

We verified that this complex structure can be written into JSON, and then parsed back into an identical value (this is a **round-trip** that preserves the value).

We can look at the type of `studentData` as a **tree** of simpler values:
* `studentsData` is a map which has 2 keys:
  * `department` with a value which is a string
  * `students` with a value which is an embedded array whose items are compound values.
* The nested values are maps that have 2 keys:
  * `name` whose value is a `string` 
  *  `degree` whose value is also a `string`
  

### 2.2 Documenting Complex Values with Type Annotations

We can describe this structure using the following TypeScript declaration:

```typescript
{
    department: string;
    students: { name: string, degree: string }[]
}
```

This can be made more readable if we name the types, using the **type alias** TypeScript construct to name map compound types:


```typescript
type Student = {
    name: string;
    degree: string;
}

type StudentsData = {
    department: string;
    students: Student[];
}
```

This type definition is useful to document the type of the expected values that we expect to read.

### 2.3 Working on complex JSON values

In this section, we will use the methods **map**, **filter**, **reduce** to operate over complex JSON values. 

We will take a videos-database example ([http://reactivex.io/learnrx/](http://reactivex.io/learnrx/)) as a basis for our examples.

Suppose you are a developer at the popular streaming-movie website Netflix, and that your system's *API* uses JSON to communicate data. The JSON values we describe are actual JSON values returned by the Netflix API.

When searching for new movie releases, you send a query to the API, and obtain a JSON reply:


```typescript
let newReleases = [
        {
            "id": 70111470,
            "title": "Die Hard",
            "boxart": "http://cdn-0.nflximg.com/images/2891/DieHard.jpg",
            "uri": "http://api.netflix.com/catalog/titles/movies/70111470",
            "rating": 4.0,
            "bookmark": []
        },
        {
            "id": 654356453,
            "title": "Bad Boys",
            "boxart": "http://cdn-0.nflximg.com/images/2891/BadBoys.jpg",
            "uri": "http://api.netflix.com/catalog/titles/movies/70111470",
            "rating": 5.0,
            "bookmark": [{ id: 432534, time: 65876586 }]
        },
        {
            "id": 65432445,
            "title": "The Chamber",
            "boxart": "http://cdn-0.nflximg.com/images/2891/TheChamber.jpg",
            "uri": "http://api.netflix.com/catalog/titles/movies/70111470",
            "rating": 4.0,
            "bookmark": []
        },
        {
            "id": 675465,
            "title": "Fracture",
            "boxart": "http://cdn-0.nflximg.com/images/2891/Fracture.jpg",
            "uri": "http://api.netflix.com/catalog/titles/movies/70111470",
            "rating": 5.0,
            "bookmark": [{ id: 432534, time: 65876586 }]
        }
    ];
```

Let us describe the **type** of this value:
* We observe it is a homogeneous array of map values.
* Each map value describes a "movie" with fields id, title, boxart, uri, rating and bookmark.

The corresponding TypeScript annotation is:

```typescript
type Video = {
    id: number;
    title: string;
    boxart: string;
    uri: string;
    rating: number;
    bookmark: { id: number, time: number }[];
}

type Releases = Video[];
```

This type annotation lets us think about the data we obtained from the API as an array `[v1, v2, v3, v4]` where each item $$v_i$$ is a map `{ id, title, boxart, uri, rating, bookmark }`.


#### 2.3.1 Map over an Array of Videos

We would like to transform this data into an array of `{ id, title }` properties. 
This transformation is called a **projection** as we select only some of the properties in the input.

In terms of types, this transformation maps from the type `Video[]` into the type `{ id: number, title: string }[]`.

A procedural way to achieve this goal would be:


```typescript
function getIDsAndTitles_1(reply) {
    let res = [];
    for (let i = 0; i < reply.length; i++) {
        res.push({ id: reply[i].id, title: reply[i].title });
    }
    return res;
}

let newReleasesIDAndTitle = getIDsAndTitles_1(newReleases);
console.log(newReleasesIDAndTitle);
// ==> 
// [ { id: 70111470, title: 'Die Hard' },
//   { id: 654356453, title: 'Bad Boys' },
//   { id: 65432445, title: 'The Chamber' },
//   { id: 675465, title: 'Fracture' } ]
```

A functional way of achieving the same result uses the **map** method (seen in the reading material for week 1) to abstract away the loop and the mutations we observe in the procedural solution (`i++`, `res.push()`).

```typescript
let newReleasesIDAndTitle = newReleases.map(x => ({ id: x.id, title: x.title }));
console.log(newReleasesIDAndTitle);
// ==> 
// [ { id: 70111470, title: 'Die Hard' },
//   { id: 654356453, title: 'Bad Boys' },
//   { id: 65432445, title: 'The Chamber' },
//   { id: 675465, title: 'Fracture' } ]
```

Note that in this version:
* There is no loop (`map` abstracts away the loop control structure)
* There is no variable assignment (no need to define a loop counter `i` and mutate it and to maintain a `res` accumulator variable).

Let us note what is the type of the transformer function we pass as an argument to `map`:
it gets as an argument `x` an item from `newReleases` - which is of type `Video` - and it returns a map of type `{ id: number, name: title }`.  

#### 2.3.2 Filter an Array of Videos

The user is quite picky, and would like to search for new releases that have a rating of 5.0 only. 

We can solve this problem using **filter**.

In terms of types, the operation we want to define maps an array of `Video[]` to an array of `Video[]`.
This fits the definition of `filter` - which does not change the type of items in the array.

```typescript
let newReleasesOfRating5 = newReleases.filter(x => x.rating === 5);
console.log(newReleasesOfRating5);
// ==>
// [ { id: 654356453,
//     title: 'Bad Boys',
//     boxart: 'http://cdn-0.nflximg.com/images/2891/BadBoys.jpg',
//     uri: 'http://api.netflix.com/catalog/titles/movies/70111470',
//     rating: 5,
//     bookmark: [ [Object] ] },
//   { id: 675465,
//     title: 'Fracture',
//     boxart: 'http://cdn-0.nflximg.com/images/2891/Fracture.jpg',
//     uri: 'http://api.netflix.com/catalog/titles/movies/70111470',
//     rating: 5,
//     bookmark: [ [Object] ] } ]
```

#### 2.3.4 Reminder: Reduce and Neutral Elements as Initializers
Compare the following 5 invocations of `reduce`:

```typescript
// Compute the sum of an array of integers
[1, 2, 3].reduce((acc, cur) => acc + cur, 0); // ==> 6

// Compute the product of an array of integers
[1, 2, 3].reduce((acc, cur) => acc * cur, 1); // ==> 6

// Compute the logical and of an array of booleans
[true, false, true].reduce((acc, cur) => acc && cur, true); // ==> false

// Compute the logical or of an array of booleans
[true, false, true].reduce((acc, cur) => acc || cur, false); // ==> true

// Compute the max of an array of numbers
[3, 1, 4].reduce((acc,cur) => Math.max(acc,cur), 0); // ==> 4
```

Note that in all 5 cases, the initializer passed to `reduce(reducer, init)` is the neutral element of the reducer operator (0 for `+`, 1 for `*`, `true` for `and`, `false` for `or`).

This makes sense in general - and allows one to answer the puzzling question of **what should be the value of reduce when applied to an empty array**.  The general answer is that it should be the neutral element of the reducer.

**NOTE**: the `max` version takes as an initializer the value 0.  This is not correct when the array can contain negative numbers.  What should be the initial value in this case?

#### 2.3.5 Rectangle Selection with Reduce

A user has chosen to view movies as boxarts (that is, the image that is shown for each movie), as it is easier to select movies according to their boxarts. The API returned the following value as a reply to our query:


```typescript
let boxarts = [
    { width: 200, height: 200, url: "http://cdn-0.nflximg.com/images/2891/Fracture200.jpg" },
    { width: 150, height: 200, url: "http://cdn-0.nflximg.com/images/2891/Fracture150.jpg" },
    { width: 300, height: 200, url: "http://cdn-0.nflximg.com/images/2891/Fracture300.jpg" },
    { width: 425, height: 150, url: "http://cdn-0.nflximg.com/images/2891/Fracture425.jpg" }
]
```

Let us define the type of this returned value: it is a homogeneous array of maps. Each map describes a boxart.

```typescript
type Boxart = {
    width: number; 
    height: number;
    url: string;
}
```

We would like to find the largest box-art image size, so that we could know what is the maximal size of the image placeholder should be.  We measure rectangles by their area ($$width \times height$$).

A straightforward way to do this is by going over each boxart, and keeping a temporary variable that holds the largest size out of the boxarts we have seen so far.

In terms of types, the transformation maps an array `Boxart[]` into a `number`. Let us annotate this type into the function definition:


```typescript
type Boxart = {
    width: number; 
    height: number;
    url: string;
}

function maxBox(boxes: Boxart[]): number {
    let maxBox = 0;
    for (let i = 0; i < boxes.length; i++) {
        let curBoxSize = boxes[i].width * boxes[i].height;
        if (curBoxSize > maxBox) {
            maxBox = curBoxSize;
        }
    }
    return maxBox;
}
maxBox(boxarts); // ==> 63750
```

This is a case of a loop to accumulate a value from an array of values. In terms of types, it maps an array to a single value. The **reduce** method fits perfectly for this scenario: The `reduce` method takes as parameter a reducer function and an initial value, and returns the accumulated value. 

The reducer function takes two arguments: the current accumulator, and the current item. The return value of the reducer function is the "successive" value of the accumulator parameter. That is, the result of some operation applied to the current accumulator, and the current item.

Our current accumulator should be the current largest box-art image size, and the initial value should be 0.

```typescript
let largestBoxartSize = boxarts.reduce(
    (curMax,curBox) => {
        let curBoxSize = curBox.width * curBox.height;
           if (curBoxSize > curMax) {
               return curBoxSize;
           } else {
               return curMax;
           }
    }, // this is the reducer function
    0  // this is the initial value
    );
    
console.log(largestBoxartSize); // ==> 63750
```

Indeed, the largest boxart image size is 63750, which is the 4th boxart.

Note that this version has no side effect, no mutation and no loop.

We can make this version more readable by using the Math.max function - to express clearly that what we are doing in this `reduce` invocation is the identification of the max of an array.  We also annotate the expected type of the parameters in the reducer.

```typescript
const boxSize = (box: Boxart) => box.width * box.height;
let maxBoxSize = boxarts.reduce((curMax: number, curBox: Boxart) => Math.max(curMax, boxSize(curBox)), 0);

console.log(maxBoxSize); // ==> 63750
```

### 2.4 Tree Values

Some replies will be more complex than what we have witnessed above: it is very often that we face replies that have a **tree** form, as opposed to a flat homogeneous array.

One example of such, is the following API reply - which returns values grouped by movie genre ("New Releases", "Dramas" ...)

```typescript
let movieLists = [
        {
            name: "New Releases",
            videos: [
                {
                    "id": 70111470,
                    "title": "Die Hard",
                    "boxart": "http://cdn-0.nflximg.com/images/2891/DieHard.jpg",
                    "uri": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 4.0,
                    "bookmark": []
                },
                {
                    "id": 654356453,
                    "title": "Bad Boys",
                    "boxart": "http://cdn-0.nflximg.com/images/2891/BadBoys.jpg",
                    "uri": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 5.0,
                    "bookmark": [{ id: 432534, time: 65876586 }]
                }
            ]
        },
        {
            name: "Dramas",
            videos: [
                {
                    "id": 65432445,
                    "title": "The Chamber",
                    "boxart": "http://cdn-0.nflximg.com/images/2891/TheChamber.jpg",
                    "uri": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 4.0,
                    "bookmark": []
                },
                {
                    "id": 675465,
                    "title": "Fracture",
                    "boxart": "http://cdn-0.nflximg.com/images/2891/Fracture.jpg",
                    "uri": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 5.0,
                    "bookmark": [{ id: 432534, time: 65876586 }]
                }
            ]
        }
    ]
```

To document the structure of this value, let us write its type:
* It is an array of 2 maps of identical structure.
* Each item represents a category of Videos. The category maps have the following keys:
  * name: string
  * videos: an array of Video.

The TypeScript definition is thus:

```typescript
type VideoCategory = {
    name: string;
    videos: Video[];
}
type VideoCategories = VideoCategory[];
```

The structure of the value is thus a *tree*: $$[ category_1, category_2 ]$$ where $$category_i$$ is of the form:<br>
`{ name: string, videos: [v1,...] }`.

These kinds of trees impose a challenge when working with them: if we apply `map` or `filter`, the transformer or the predicate will receive as argument a `VideoCategory` object.  If we want to process the embedded `Video` objects inside the categories, we must first **flatten** the tree.

Suppose we would like to have a list of all movie ids in that tree. The most trivial way, as before, is to do the following (using a foreach loop):

```typescript
let movieIds = []
movieLists.forEach(category => category.videos.forEach(video => movieIds.push(video.id)));
console.log(`movieIds=${movieIds}`); // ==> movieIds=70111470,654356453,65432445,675465
```

`forEach` abstracts a single loop. The embedded calls to `forEach` inside `forEach` reflects the two-level traversal of the tree.

Note that `forEach` is **not a functional** style: this is precisely the type of operations we want to abstract away using map/filter/reduce. This version has an accumulator variable (`movieIds`) which we must initialize and then mutate as part of the `forEach` iteration.

Not a functional solution...

The general feeling of the task we want to perform is an **accumulation** - so that `reduce` seems to be the good tool. But we cannot apply `reduce` as is, because `reduce` would work on `Category` values and we want to accumulate `Video` values.

Alternatively, we could attempt to use **map** as we are mapping `Video` values into `number` values.  But again, we cannot map over Categories - we need to map over Videos - so that `map` cannot be used directly.

Let's recall the `concat` method of arrays:

```typescript
let arrayOne = [1, 2, 3, 4];
let arrayTwo = [5, 6, 7, 8];
arrayOne.concat(arrayTwo); // ==> [ 1, 2, 3, 4, 5, 6, 7, 8 ]
```

In contrast to `push`, `concat` does not mutate its arguments - it returns a new array and can be used safely.

If we `reduce` an array of arrays and accumulate using the `concat` operator,  we will get a method that flattens one level of an array of arrays:

```typescript
let a = [[1, 2, 3], [4, 5, 6]];
a.reduce((acc, curr) => acc.concat(curr), []); // ==> [ 1, 2, 3, 4, 5, 6 ]
```

We now need to apply a mixture of `map` and `reduce` in order to map and flatten:

```typescript
// We want the following as an array of the numbers:
let a = [{ group: 1, numbers: [1, 2, 3] }, { group: 2, numbers: [4, 5, 6] }];
a.map(x => x.numbers).reduce((acc,curr) => acc.concat(curr), []); // ==> [ 1, 2, 3, 4, 5, 6 ]
```

### 2.4.1 The Ramda package

Ramda is a library of functions designed to facilitate functional programming in JavaScript.

Ramda includes a function called `chain` (often also called `flatmap`) which can come handy in our last example: If we want to return all of the results as a single flat array instead of as an array of arrays, then we can use Ramda's `chain`:

```typescript
import * as R from "ramda";

R.chain(x => x.numbers, a); // ==> [ 1, 2, 3, 4, 5, 6 ]
```

The `chain` function goes through an array, and flattens the result of 'doing something' (which can be 'nothing' or applying a 'mapping') by one level.

Back to our videos example, if we `chain` the `movieLists` with a function that just returns the category videos, then the result would be an array containing the videos. As such:

```typescript
R.chain(category => category.videos, movieLists);
// ==>
// [ { id: 70111470,
//     title: 'Die Hard',
//     boxart: 'http://cdn-0.nflximg.com/images/2891/DieHard.jpg',
//     uri: 'http://api.netflix.com/catalog/titles/movies/70111470',
//     rating: 4,
//     bookmark: [] },
//   { id: 654356453,
//     title: 'Bad Boys',
//     boxart: 'http://cdn-0.nflximg.com/images/2891/BadBoys.jpg',
//     uri: 'http://api.netflix.com/catalog/titles/movies/70111470',
//     rating: 5,
//     bookmark: [ [Object] ] },
//   { id: 65432445,
//     title: 'The Chamber',
//     boxart: 'http://cdn-0.nflximg.com/images/2891/TheChamber.jpg',
//     uri: 'http://api.netflix.com/catalog/titles/movies/70111470',
//     rating: 4,
//     bookmark: [] },
//   { id: 675465,
//     title: 'Fracture',
//     boxart: 'http://cdn-0.nflximg.com/images/2891/Fracture.jpg',
//     uri: 'http://api.netflix.com/catalog/titles/movies/70111470',
//     rating: 5,
//     bookmark: [ [Object] ] } ]
```

The only change that is left, is that we do not want the category videos flattened, rather something extra: taking the `category.videos` and mapping each element there to its `id`.

```typescript
R.chain(category => category.videos, movieLists).map(video => video.id); // ==> [ 70111470, 654356453, 65432445, 675465 ]
```

It helps to annotate the types of the arguments in such chained transformations to document and verify their correctness:

```typescript
R.chain((category: VideoCategory) => category.videos, movieLists).map((video: Video) => video.id); // ==> [ 70111470, 654356453, 65432445, 675465 ]
```

## 3. Exercises

### 3.1 Implement the `map` method using `reduce`

Let's think step by step: What should be our *accumulated* value? It is natural to think that the accumulation consists in 'pushing' the current element **as a mapped value** to the currently accumulated array.

To that end, the initial value should be the empty array. As we go, we will take the current element, and our next accumulator will be the current accumulator, only expanded by the current element after it is mapped by the transformer.

[Solution](./ps1_sol.html#exercise-1-implementing-map-with-reduce)

### 3.2 Implement `filter` using `reduce`

What should be changed in the solution of the previous exercise?

We can skip adding the current element if it does not satisfy the predicate, in which case the next accumulator will not change. However, if the current element does satisfy the predicate, we will set the next accumulator to be the current one, only enlarged by the current element

[Solution](./ps1_sol.html#exercise-2-implementing-filter-with-reduce)

### 3.3 Implement `some` and `every` using `map` and `reduce`

`some(pred)` is a method that returns `true` if and only if *at least one* element in an array satisfies a given predicate.

`every(pred)` is a method that returns true if and only if *all* the elements in an array satisfy a given predicate.

For example:

```typescript
let even = (x => x % 2 == 0);
let arr1 = [1,2,3,4];
let arr2 = [1,3,5,7];
let arr3 = [2,4,6,8];
let arr1HasEvenNumbers = arr1.some(even);
let arr2HasEvenNumbers = arr2.some(even);
let allInArr1AreEven = arr1.every(even);
let allInArr3AreEven = arr3.every(even);
console.log(`arr1HasEvenNumbers = ${arr1HasEvenNumbers}`); // ==> arr1HasEvenNumbers = true
console.log(`arr2HasEvenNumbers = ${arr2HasEvenNumbers}`); // ==> arr2HasEvenNumbers = false
console.log(`allInArr1AreEven = ${allInArr1AreEven}`); // ==> allInArr1AreEven = false
console.log(`allInArr3AreEven = ${allInArr3AreEven}`); // ==> allInArr3AreEven = true
```

Instructions: 

A way to implement `some` is by mapping the array to `true/false` values of whether each element satisfies the predicate, and then doing an accumulated logical `or` between the elements using `reduce`, with the initial accumulator equal to `false`.

Similarly, we can implement `every` in the same way as above, except that we need to use a logical `and` and our initial accumulator should be `true`.

[Solution](./ps1_sol.html#exercise-3-implementing-some-and-every-with-map-and-reduce)

## Shortcut Semantics

The native `some` and `every` methods employ a concept known as 'shortcut semantics'. What this means, is that `some` stops and immediately returns `true` at the moment it finds an element that satisfies the predicate. `every` stops and immediately returns `false` at the moment it finds an element that does not satisfy the predicate.

**Question**: does our implementation satisfy shortcut semantics?

It is difficult to distinguish between shortcut semantics and non-shortcut semantics because both shortcut and non-shortcut versions return the same values for all parameters.  

Can we conclude that a shortcut and non-shortcut versions of `some` are equivalent according to the definition of function equivalence we provided in class?

Remember though the difference between mathematical function equivalence and programming functions equivalence: 2 programming functions `f` and `g` are equivalent if they have the same domain, same range and for all values in the domain:
* Either `f(x)` has a value and `g(x)` has the same value
* Or `f(x)` does not terminate and `g(x)` does not terminate as well
* Or `f(x)` throws an error and `g(x)` throws an error as well

How can we then distinguish between a shortcut semantics version of `some` and one that is not shortcut?

The solution is to choose parameters that will distinguish between the two computations using either non-termination (infinite loops) or errors.

Let us define such an example on arrays of numbers.  We can trigger an error in JavaScript by using the `throw` primitive.

To test whether an error was thrown, we use the same syntax as in Java - with `try/catch`.

```typescript
const throwOnZero = x => {
    if (x > 0)
        return true;
    else
        throw false;
}

let a = [1, 0];

try {
    a.some(throwOnZero);
} catch (e) {
    e;
}
```

The code above returns `true`.

```typescript
const someExercise = (pred, arr) => arr.map(pred).reduce((acc, cur) => acc || cur, false);

try {
    someExercise(throwOnZero, a);
} catch (e) {
    e;
}
```

The code above will return `false`.

By using deliberate error throwing, we are able to distinguish between shortcut and non-shortcut semantics on the definition of the `some` function.

The next question that arises is whether we can define a version of `some` using `reduce` that has shortcut semantics. We will revisit this question in Chapter 3 of the course, when we introduce generators.
