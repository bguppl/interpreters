import p, { Sexp } from "s-expression";
import { makeFailure, makeOk, Result } from "./result";

const isError = (x: any): x is Error => x instanceof Error;

export default function parse(x: string): Result<Sexp> {
    const parsed = p(x);
    return isError(parsed) ? makeFailure(parsed.message) : makeOk(parsed);
}