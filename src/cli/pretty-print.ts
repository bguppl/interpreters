// pretty-print.ts
// Simple recursive pretty-printer for s-expression strings.
// If a value fits on one line (within maxWidth) it is left as-is.
// Otherwise the top-level list is broken: first element stays on the same
// line, remaining elements are placed on separate lines indented by 2 spaces
// per nesting level.

// Split the inner content of an s-expression string into its top-level tokens.
// E.g. "(a (b c) d)" → ["a", "(b c)", "d"]
const splitSexpParts = (s: string): string[] => {
    s = s.trim();
    if (!s.startsWith('(')) return [s];
    const inner = s.slice(1, s.lastIndexOf(')')).trim();
    const parts: string[] = [];
    let depth = 0;
    let inStr = false;
    let start = 0;
    for (let i = 0; i < inner.length; i++) {
        const ch = inner[i];
        if (inStr) {
            if (ch === '\\') { i++; }
            else if (ch === '"') { inStr = false; }
        } else if (ch === '"') {
            inStr = true;
        } else if (ch === '(' || ch === '[') {
            depth++;
        } else if (ch === ')' || ch === ']') {
            depth--;
        } else if (ch === ' ' && depth === 0) {
            const part = inner.slice(start, i).trim();
            if (part) parts.push(part);
            start = i + 1;
        }
    }
    const last = inner.slice(start).trim();
    if (last) parts.push(last);
    return parts;
};

// Recursively pretty-print an s-expression string.
// indentLevel: current nesting depth (each level = 2 spaces)
// maxWidth: line length threshold
export const prettyPrint = (s: string, indentLevel = 0, maxWidth = 80): string => {
    s = s.trim();
    // Fits on one line — return as-is
    if (indentLevel * 2 + s.length <= maxWidth) return s;
    // Not a list — can't break further
    if (!s.startsWith('(')) return s;

    const parts = splitSexpParts(s);
    if (parts.length <= 1) return s;

    const head = parts[0];
    const tail = parts.slice(1);
    const childPrefix = '  '.repeat(indentLevel + 1);

    return (
        '(' +
        head +
        '\n' +
        tail
            .map(p => childPrefix + prettyPrint(p, indentLevel + 1, maxWidth))
            .join('\n') +
        ')'
    );
};
