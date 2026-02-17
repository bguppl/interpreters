import { readFileSync } from "fs";
import { map } from "ramda";
import { parseL3, unparseL3 } from "./L3-ast";
import { evalL3program } from "./L3-eval";
import { isClosure, valueToString, Value, Closure } from "./L3-value";
import { bind, either } from "../shared/result";

const filename = process.argv[2];
if (!filename) {
    console.error("Usage: L3 <filename>");
    process.exit(1);
}

let source: string;
try {
    source = readFileSync(filename, "utf-8");
} catch (e) {
    console.error(`Error reading file '${filename}': ${(e as Error).message}`);
    process.exit(1);
}

// Display a closure with properly unparsed body using the L3 concrete syntax.
const displayClosure = (c: Closure): string => {
    const params = map((p) => p.var, c.params).join(" ");
    const body = map(unparseL3, c.body).join(" ");
    return `<Closure (${params}) ${body}>`;
};

const displayValue = (val: Value): string =>
    isClosure(val) ? displayClosure(val) :
    valueToString(val);

const result = bind(parseL3(source), evalL3program);

either(
    result,
    (value) => console.log(displayValue(value)),
    (message) => {
        console.error(`Error: ${message}`);
        process.exit(1);
    }
);
