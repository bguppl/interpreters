declare module 's-expression' {
    export type SexpString = String;
    export type StringTree = string | SexpString | StringTree[];

    /*
        The types returned by the parser are:
        string - for any token which is not a string,
                 according to the tokenization rules of S-expressions.
        SexpString - for tokens of the form "..."
        StringTree[] - for S-expressions that contain sub-expressions
                       (of the form "(<expr1> ... <exprn>)")
    */
    export default function parse(x: string): StringTree;
}