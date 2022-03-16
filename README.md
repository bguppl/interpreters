# Principles of Programming Languages

This repository contains the source code used in the "Principles of Programming Languages" course taught at the Computer Science Dept of Ben Gurion University.
Course material is available [here](https://bguppl.github.io/interpreters/).
Exercises and practical sessions are available [here](https://github.com/bguppl/practice).

The main part of this repo is a sequence of interpreters written in TypeScript with a functional style of a Scheme-like language.
The languages start from a very simple subset of Scheme and increase up to a tail call optimized version using continuation passing style.

The repo also contains a small Prolog interpreter implemented in Racket.


## Installation

1. Clone the repository either using `git clone https://github.com/bguppl/interpreters.git` or download the archive from [here](https://github.com/bguppl/interpreters/archive/master.zip).
2. Run `npm install` inside the `interpreters` folder.

## Running the Tests

Run `npm test` inside the `interpreters` folder.

## Code Coverage

Run `npm run coverage` inside the `interpreters` folder. A new folder called `coverage` will be created. 
Open the `lcov-report/index.html` file inside it to see coverage statistics.  
In each file of the tests, you can see which lines have been covered by a test.
