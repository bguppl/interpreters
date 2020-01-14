declare module 's-expression' {
    export type StringArray = string | Array<StringArray>;
    export default function SParse(stream: string): StringArray;
}