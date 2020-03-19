declare module 's-expression' {
    export type SexpString = String;
    export type StringTree = string | SexpString | StringTree[];
    export default function parse(x: string): StringTree;
}