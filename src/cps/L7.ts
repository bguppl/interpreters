import { readFileSync } from "fs";
import { parseL5 } from "../L5/L5-ast";
import { evalProgram } from "./L7c-eval";
import { isClosure, valueToString, Value, Closure } from "../L5/L5-value";
import { bind, either } from "../shared/result";

const filename = process.argv[2];
if (!filename) {
    console.error("Usage: L7 <filename>");
    process.exit(1);
}

let source: string;
try {
    source = readFileSync(filename, "utf-8");
} catch (e) {
    console.error(`Error reading file '${filename}': ${(e as Error).message}`);
    process.exit(1);
}

const stripComments = (x: string): string =>
    x.replace(/;[^\r\n]*/g, '');

const displayClosure = (c: Closure): string => {
    const params = c.params.map((p) => p.var).join(" ");
    return `<Closure (${params}) ${c.body}>`;
};

const displayValue = (val: Value): string =>
    isClosure(val) ? displayClosure(val) :
    valueToString(val);

const result = bind(parseL5(stripComments(source)), evalProgram);

either(
    result,
    (value) => console.log(displayValue(value)),
    (message) => {
        console.error(`Error: ${message}`);
        process.exit(1);
    }
);
