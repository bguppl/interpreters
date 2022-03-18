import { pipe } from "fp-ts/function";

// Refer to the types from Sexp
import { Sexp } from "s-expression";
import { parse } from "../shared/parser";

// We adopt the functional Either<Err, T> pattern for Error handling.
// Because parse can fail on ill-formed expressions, 
// parse returns an Either<string, Exp> value (which is a disjoint union Left<string> or Right<Exp>).
import * as E from "fp-ts/Either";

// Import the lexical rules predicates.
import { isNumericString, isString, isArray } from "../shared/type-predicates";
import { isEmpty } from "../shared/list";

// --------------------------------------------
// AST Type Definition

// Disjoint type E
type Exp = NumExp | AddExp | MulExp;
const isE = (x: any): x is Exp => isNumExp(x) || isAddExp(x) || isMulExp(x);

// For each constituent type define an interface, a constructor and a type predicate.
interface NumExp { tag: "NumExp"; val: number; };
const makeNumExp = (n: number): NumExp => ({ tag: "NumExp", val: n });
const isNumExp = (x: any): x is NumExp => x.tag === "NumExp";

interface AddExp { tag: "AddExp"; left: Exp; right: Exp };
const makeAddExp = (left: Exp, right: Exp): AddExp => ({ tag: "AddExp", left: left, right: right });
const isAddExp = (x: any): x is AddExp => x.tag === "AddExp";

interface MulExp { tag: "MulExp"; left: Exp; right: Exp };
const makeMulExp = (left: Exp, right: Exp): MulExp => ({ tag: "MulExp", left: left, right: right });
const isMulExp = (x: any): x is MulExp => x.tag === "MulExp";

// --------------------------------------------
// Toplevel function: parse a string into an E expression.
// Since parse can fail, return an Either<string, Exp>
// We split the parsing process in 2 stages:
// - Tokenization and embedding with the general S-expression parser.
// - Parsing according to the Exp grammar implemented in this package.
// We adopt the Either<Err, T> monad pattern to process errors.
// chain is used to compose functions that return Either<Err, T> values.
// We use the `pipe` function to "pipe" a value through a series of functions:
// - Take `x` and pass it to `parse`
// - Take the result of `parse(x)` and pass it to `chain(parseExpSexp)`
// This is equivalent to: `E.chain(parseExpSexp)(parse(x))`
export const parseExp = (x: string): E.Either<string, Exp> =>
    pipe(x, parse, E.chain(parseExpSexp));

// ========================================================
// Parsing

const parseExpSexp = (sexp: Sexp): E.Either<string, Exp> =>
    isEmpty(sexp) ? E.left("Unexpected empty") :
    isString(sexp) ? parseEAtomic(sexp) :
    isArray(sexp) ? parseECompound(sexp) :
    // Quoted strings are not legal in the E-expression language
    E.left("Expected either a compound expression or a token, got a quoted string");

// Only numeric tokens are ok in this language
// We decided not to refer to "+" and other primitives as distinct atomic expressions.
// The decision is different in Scheme (and L1)
const parseEAtomic = (sexp: string): E.Either<string, Exp> =>
    isNumericString(sexp) ? E.of(makeNumExp(+sexp)) :
    E.left("Bad token " + sexp);

// Compound expressions must be of the form (<exp> <op> <exp>) where op in (*, +)
// This procedure is recursive since the left and right sides can be embedded compound expressions.
const parseECompound = (sexps: readonly Sexp[]): E.Either<string, Exp> =>
    (sexps.length !== 3) ? E.left("Wrong length") :
    // TODO Explain chaining
    isString(sexps[1]) ? pipe(parseExpSexp(sexps[0]),
                              E.chain((arg1) => pipe(parseExpSexp(sexps[2]),
                                                     E.chain((arg2) => parseExp3(sexps[1], arg1, arg2))))) :
    E.left("Expected operator, got compound expression");

const parseExp3 = (op: Sexp, arg1: Exp, arg2: Exp): E.Either<string, Exp> =>
    op === "+" ? E.of(makeAddExp(arg1, arg2)) :
    op === "*" ? E.of(makeMulExp(arg1, arg2)) :
    E.left("Bad operator " + op);

// Examples

// As returned by the S-expression parser:
console.log(parse("(1 + 2)"));
// { tag: 'Ok', value: [ '1', '+', '2' ] }

console.log(parseExp("(1 + 2)"));
// { tag: 'Ok',
//   value:
//   { tag: 'AddExp',
//     left: { tag: 'NumExp', val: 1 },
//     right: { tag: 'NumExp', val: 2 } } }

console.log(parse("(1 + 2"));
// { tag: 'Failure', message: 'Syntax error: Expected `)` - saw: ``' }
console.log(parseExp("(1 + 2"));
// { tag: 'Failure', message: 'Syntax error: Expected `)` - saw: ``' }

console.log(JSON.stringify(parseExp("(2 * (1 + 2))"), undefined, 2));
/*
{
  "tag": "Ok",
  "value": {
    "tag": "MulExp",
    "left": {
      "tag": "NumExp",
      "val": 2
    },
    "right": {
      "tag": "AddExp",
      "left": {
        "tag": "NumExp",
        "val": 1
      },
      "right": {
        "tag": "NumExp",
        "val": 2
      }
    }
  }
}
*/
