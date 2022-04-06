// Refer to the types from Sexp
import { Sexp } from "s-expression";
import { parse } from "../shared/parser";

// We adopt the functional Result<T> pattern for Error handling.
// Because parse can fail on ill-formed expressions, 
// parse returns a Result<E> value (which is a disjoint union Ok<E> or Failure).
import { Result, makeOk, makeFailure, bind } from "../shared/result";

// Import the lexical rules predicates.
import { isNumericString, isString, isArray } from "../shared/type-predicates";
import { isEmpty } from "../shared/list";

// --------------------------------------------
// AST Type Definition

// Disjoint type E
type E = NumExp | AddExp | MulExp;
const isE = (x: any): x is E => isNumExp(x) || isAddExp(x) || isMulExp(x);

// For each constituent type define an interface, a constructor and a type predicate.
interface NumExp { tag: "NumExp"; val: number; };
const makeNumExp = (n: number): NumExp => ({ tag: "NumExp", val: n });
const isNumExp = (x: any): x is NumExp => x.tag === "NumExp";

interface AddExp { tag: "AddExp"; left: E; right: E };
const makeAddExp = (left: E, right: E): AddExp => ({ tag: "AddExp", left: left, right: right });
const isAddExp = (x: any): x is AddExp => x.tag === "AddExp";

interface MulExp { tag: "MulExp"; left: E; right: E };
const makeMulExp = (left: E, right: E): MulExp => ({ tag: "MulExp", left: left, right: right });
const isMulExp = (x: any): x is MulExp => x.tag === "MulExp";

// --------------------------------------------
// Toplevel function: parse a string into an E expression.
// Since parse can fail, return a Result<E>
// We split the parsing process in 2 stages:
// - Tokenization and embedding with the general S-expression parser.
// - Parsing according to the E-grammar implemented in this package.
// We adopt the Result<T> monad pattern to process errors.
// bind is used to compose functions that return Result<T> values.
// - First invoke parse(x)
// - If the result is a Failure, stop
// - Else we received an Ok<Sexp> value, pass the Sexp result to the next function (parseESexp)
export const parseE = (x: string): Result<E> =>
    bind(parse(x), (s: Sexp) => parseESexp(s));

// ========================================================
// Parsing

const parseESexp = (sexp: Sexp): Result<E> =>
    isEmpty(sexp) ? makeFailure("Unexpected empty") :
    isString(sexp) ? parseEAtomic(sexp) :
    isArray(sexp) ? parseECompound(sexp) :
    // Quoted strings are not legal in the E-expression language
    makeFailure("Expected either a compound expression or a token, got a quoted string");

// Only numeric tokens are ok in this language
// We decided not to refer to "+" and other primitives as distinct atomic expressions.
// The decision is different in Scheme (and L1)
const parseEAtomic = (sexp: string): Result<E> =>
    isNumericString(sexp) ? makeOk(makeNumExp(+sexp)) :
    makeFailure("Bad token " + sexp);

// Compound expressions must be of the form (<exp> <op> <exp>) where op in (*, +)
// This procedure is recursive since the left and right sides can be embedded compound expressions.
const parseECompound = (sexps: Sexp[]): Result<E> =>
    (sexps.length !== 3) ? makeFailure("Wrong length") :
    isString(sexps[1]) ? bind(parseESexp(sexps[0]), (arg1: E) =>
                          bind(parseESexp(sexps[2]), (arg2: E) =>
                            parseE3(sexps[1], arg1, arg2))) :
    makeFailure("Expected operator, got compound expression");

const parseE3 = (op: Sexp, arg1: E, arg2: E): Result<E> =>
    op === "+" ? makeOk(makeAddExp(arg1, arg2)) :
    op === "*" ? makeOk(makeMulExp(arg1, arg2)) :
    makeFailure("Bad operator " + op);

// Examples

// As returned by the S-expression parser:
console.log(parse("(1 + 2)"));
// { tag: 'Ok', value: [ '1', '+', '2' ] }

console.log(parseE("(1 + 2)"));
// { tag: 'Ok',
//   value:
//   { tag: 'AddExp',
//     left: { tag: 'NumExp', val: 1 },
//     right: { tag: 'NumExp', val: 2 } } }

console.log(parse("(1 + 2"));
// { tag: 'Failure', message: 'Syntax error: Expected `)` - saw: ``' }
console.log(parseE("(1 + 2"));
// { tag: 'Failure', message: 'Syntax error: Expected `)` - saw: ``' }

console.log(JSON.stringify(parseE("(2 * (1 + 2))"), undefined, 2));
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
