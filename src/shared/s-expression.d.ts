declare module 's-expression' {
    export type SexpString = String;
    export type Token = string | SexpString;
    export type CompoundSexp = Sexp[];
    export type Sexp = Token | CompoundSexp;

    /*
        The types returned by the parser are:
        string - for any token which is not a string,
                 according to the tokenization rules of S-expressions.
        SexpString - for tokens of the form "..."
        Sexp[] - for S-expressions that contain sub-expressions
                 (of the form "(<s-expr1> ... <s-exprn>)")
    */
    export default function parse(x: string): Sexp;
}