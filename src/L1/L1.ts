import { readFileSync } from "fs";
import { parseL1 } from "./L1-ast";
import { evalL1program } from "./L1-eval";
import { bind, either } from "../shared/result";

const filename = process.argv[2];
if (!filename) {
    console.error("Usage: L1 <filename>");
    process.exit(1);
}

let source: string;
try {
    source = readFileSync(filename, "utf-8");
} catch (e) {
    console.error(`Error reading file '${filename}': ${(e as Error).message}`);
    process.exit(1);
}

const result = bind(parseL1(source), evalL1program);

either(
    result,
    (value) => console.log(JSON.stringify(value)),
    (message) => {
        console.error(`Error: ${message}`);
        process.exit(1);
    }
);
