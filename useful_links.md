# Useful Links

## Books to which we refer in class

1. Structure and Interpretation of Computer Programs, by H. Abelson and G.J. Sussman, MIT Press, 2nd edition, 1996. Available online in [pdf](https://sicpebook.wordpress.com/ebook/) or [html](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html).

2. How to Design Programs, by M. Felleisen, R.B. Findler, M. Flatt and S. Krishnamurthi, MIT Press, 2003. Available online [http://www.htdp.org/](http://www.htdp.org/)

3. Programming Languages: Application and Interpretation, by S. Krishnamurthi, 2nd edition 2012. Available online [https://www.plai.org/](https://www.plai.org/)

4. The Art of Prolog by L. Sterling and E. Shapiro, 2nd edition MIT Press, 1994. Available online [https://mitpress.mit.edu/books/art-prolog-second-edition](https://mitpress.mit.edu/books/art-prolog-second-edition) (under the Open Access tab)

5. Essentials of Programming Languages, by D.P. Friedman and M.Wand, 3rd edition, MIT Press, 2008.

6. Programming and Programming Languages, by Shriram Krishnamurthi, Benjamin S. Lerner, Joe Gibbs Politz, 2019. Available online [https://papl.cs.brown.edu/2019/](https://papl.cs.brown.edu/2019/)

7. Professor Frisby's Mostly Adequate Guide to Functional Programming, Mostly Adequate team, 2020, Available online [https://mostly-adequate.gitbooks.io/mostly-adequate-guide/](https://mostly-adequate.gitbooks.io/mostly-adequate-guide/)

8. Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I John McCarthy, Communications of the ACM, April 1960, Available online [http://www-formal.stanford.edu/jmc/recursive.pdf](http://www-formal.stanford.edu/jmc/recursive.pdf). 

## Software

This section contains links to installations of the programming languages used in this course. These links may be useful if you intend to develop programs in these languages on computers that are not CS lab computers.

The instructions for installations are similar for Windows, Linux and MacOS (using a command line shell).

### Node / TypeScript

The full installation and configuration guide can be seen [here](https://youtu.be/tnn_a4hReKs) (the video shows older versions of VS Code and Node, but the setup should still be relevant).


0. Download node.js from [https://nodejs.org](https://nodejs.org) and install the LTS version (18).
0. Download VS Code from [https://code.visualstudio.com/](https://code.visualstudio.com/) and install.
0. Open command prompt by clicking on Start and typing `cmd`.
0. Make sure node is installed by running `node -v`. The expected output should start with 'v18.'.
0. Globally install Typescript's compiler and REPL `npm i -g ts-node typescript`.
1. Make sure Typescript is installed by running `tsc -v`. The expected output should start with '4.5'.
2. Navigate to a directory you want to create your projects in (e.g., 'Desktop\ppl').
3. Open VS Code in this foler using `code .`
4. Open the "Extensions" panel (`ctrl-shift-x`) and search for "runner".
5. Install the "Code Runner" extension.
6. Open the "Explorer" panel (`ctrl-shift-E`) and select "New Folder" and name the new folder `hw`.
7. Right-click on the new folder and select `open in the integrated terminal`.
8. Initialize `npm` in this directory using `npm init -y` in the terminal (this will create a `package.json` file in the `hw` folder)
9. Run `npm i ramda` to install the `ramda` library as a runtime dependency for this project.
10. Run `npm i -D typescript @types/ramda` to install development/compiletime dependencies for this project.
11. Right click `hw` and select "New File". Call it `hw1.ts`.
12. In the newly created file, type `console.log("Hello, world");`, save using Ctrl+S, and run using Ctrl+Alt+N. You should see "Hello, world" on the screen.
13. Check `ramda` is properly installed by adding to the top of your file the line `import { map } from "ramda";` and change your `console.log("Hello, world");` to `console.log(map(x => x * x, [1, 2, 3, 4]));`. Again, save using Ctrl+S, and run using Ctrl+Alt+N. You should see `[1, 4, 9, 16]` on the screen.

**Note**: every assignment that has TypeScript coding in it will come with a predefined `package.json` which specifies the packages and package versions for the assignment. For every assignment, extract the files in a *new folder*, open a `cmd` in that folder, and run `npm i`. This will install all necessary packages for the assignment.

### Visual Studio Live Share

In order to work on the assignments in pairs, we recommend using the Visual Studio Live Share extension. A short tutorial on how to install and use it can be seen [here](https://youtu.be/tjJf0UgMd0Q).

#### Notes

1. Upon first starting or joining a collaboration session on VS Live Share, you will be prompted to log in using either a GitHub account or a Microsoft account. We suggest logging in using a GitHub account. If you don't have one already, create a GitHub account using [this link](https://education.github.com/students).
2. You can work on different files simultaneously and not necessarily work on the same file, as shown in the video.

## Scheme

* The Racket [homepage](http://racket-lang.org/) includes downloads and documentation. Current version is Racket 8.8.
* A tutorial [The Scheme Programming Language](http://www.scheme.com/tspl4/) by R.K. Dybvig.
* [R6RS Standard](http://www.r6rs.org/final/r6rs.pdf)

## Prolog

* SWI Prolog [homepage](http://www.swi-prolog.org/) includes downloads and documentation.
