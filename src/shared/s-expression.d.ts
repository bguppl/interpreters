declare module 's-expression' {
    export type StringTree = string | String | StringTree[];
    export default function parse(x: string): StringTree;
}