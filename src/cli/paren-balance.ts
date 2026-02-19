// paren-balance.ts
// Tracks net parenthesis / bracket depth in a string,
// respecting string literals ("...") and semicolon line-comments.

export const parenDepth = (s: string): number => {
    let depth = 0;
    let inString = false;
    let i = 0;
    while (i < s.length) {
        const ch = s[i];
        if (inString) {
            if (ch === '\\') {
                i += 2;   // skip escaped character
                continue;
            }
            if (ch === '"') inString = false;
        } else {
            if (ch === '"') {
                inString = true;
            } else if (ch === ';') {
                // skip rest of line (comment)
                while (i < s.length && s[i] !== '\n') i++;
                continue;
            } else if (ch === '(' || ch === '[') {
                depth++;
            } else if (ch === ')' || ch === ']') {
                depth--;
            }
        }
        i++;
    }
    return depth;
};

// True when s is non-empty and all opened parens are closed.
export const isComplete = (s: string): boolean =>
    s.trim().length > 0 && parenDepth(s) <= 0;
