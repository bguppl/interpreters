# Interactive REPL for PPL Interpreters

An interactive Read-Eval-Print Loop (REPL) for all five interpreter levels
(L1, L3, L4, L5, L7) built into the PPL course project.

## Usage

```
npm run repl
```

## Features

### Language switching
Start in L3 by default. Switch at any time with `/lang`:

```
L3> /lang L5
;; Switched to L5
L5> (define x 10)
;; defined x
L5> (+ x 1)
11
```

Available languages: **L1**, **L3**, **L4**, **L5**, **L7**

Switching language resets the environment to empty. Use `/reset` to reset
the environment while staying in the same language.

### Multi-line expressions
Type any expression spanning multiple lines. The REPL accumulates input and
shows a `...>` continuation prompt until all opened parentheses are closed:

```
L3> (define fact
...>   (lambda (n)
...>     (if (= n 0)
...>       1
...>       (* n (fact (- n 1))))))
;; defined fact
L3> (fact 10)
3628800
```

Parenthesis balance is tracked character-by-character, respecting string
literals (`"..."`) and semicolon line comments (`;`).

### Persistent global environment
`define` expressions update the REPL's environment so that subsequent
expressions can use the defined name:

```
L4> (define square (lambda (x) (* x x)))
;; defined square
L4> (square 7)
49
```

**Environment model per language:**

| Language | Environment model |
|----------|------------------|
| L1, L3, L4 | Immutable linked-list env threaded explicitly |
| L5 | Mutable `theGlobalEnv` singleton (box-based) |
| L7 | Shares L5's `theGlobalEnv`; evaluation uses CPS registers |

Because L5 and L7 share the same global environment, bindings created in L5
are visible after switching to L7 and vice versa.

### Pretty-printed output
Results that would exceed 80 characters are automatically indented:

```
L3> (list 1 (list 2 (list 3 4)) (list 5 6))
(1
  (2
    (3 4))
  (5 6))
```

### Command history
History is automatically saved to `~/.interpreters_history` on exit and
restored at startup. The path can be overridden with the `REPL_HISTORY`
environment variable.

### Commands

| Command | Description |
|---------|-------------|
| `/lang <L1\|L3\|L4\|L5\|L7>` | Switch to a different interpreter |
| `/reset` | Clear the current language's environment |
| `/help` | Show available commands and keyboard shortcuts |
| `/primitives` | Show the primitives defined in the current interpreter |
| `/quit` or `/exit` | Exit the REPL |

### Keyboard shortcuts (readline)

| Key | Action |
|-----|--------|
| `↑` / `↓` | Navigate command history |
| `Ctrl+A` | Move cursor to start of line |
| `Ctrl+E` | Move cursor to end of line |
| `Ctrl+K` | Cut from cursor to end of line |
| `Ctrl+Y` | Paste (yank) previously cut text |
| `Ctrl+D` | Exit (EOF) |
| `Ctrl+C` | Cancel current multi-line input; exit if input is empty |

Cut/Copy/Paste via the system clipboard works through the terminal emulator as usual.

## Architecture

```
src/cli/
├── repl.ts           Main entry point — readline loop, command dispatch
├── languages.ts      Language registry (parse / eval / display per language)
├── pretty-print.ts   Recursive s-expression pretty-printer
└── paren-balance.ts  Character-level parenthesis-balance checker
```

### `paren-balance.ts`
Provides `parenDepth(s)` which counts net open parentheses in a string,
skipping string literals and `;` comments. Used by the REPL to decide
whether the accumulated input is a complete expression or needs more lines.

### `pretty-print.ts`
`prettyPrint(s, indentLevel?, maxWidth?)` — if the flat value string fits
within `maxWidth` (default 80) it is returned as-is. Otherwise the top-level
list is broken: the first element stays on the opening line and each
subsequent element is placed on its own indented line, recursively.

### `languages.ts`
Defines `LangConfig` — a uniform interface over the five interpreters:

```typescript
type LangConfig = {
    name:          string;
    parseExpr:     (sexp: Sexp)          => Result<Exp>;
    evalExpr:      (exp: Exp,  env: Env) => Result<Value>;
    evalDefineRHS: (rhs: CExp, env: Env) => Result<Value>;
    makeInitialEnv: ()                   => Env | null;
    extendEnv:     (env, var, val)       => Env | null;
    display:       (val: Value)          => string;
};
```

Each entry wires up the language-specific parser, evaluator, and display
function. L5 and L7 receive `null` for the env parameter because they use
the shared mutable `theGlobalEnv` from `L5-env.ts`.

The three immutable-env languages (L1, L3, L4) required a one-line addition
to their evaluators (`evalL1Exp`, `evalL3Exp`, `evalL4Exp`) that exposes
the internal applicative-eval function with an explicit env parameter.

### `repl.ts`
The main loop uses Node's built-in `readline` module with `terminal: true`.
On each `'line'` event:

1. Strip semicolon comments from the line.
2. If the accumulator is empty and the line starts with `/`, dispatch to the
   command handler.
3. Otherwise append to the accumulator and check paren balance.
4. When balance ≤ 0 and the accumulator is non-empty, evaluate:
   - Parse with the shared `s-expression` library to get a `Sexp`.
   - Convert to a language `Exp` via `lang.parseExpr`.
   - If it is a `DefineExp`: evaluate the RHS, extend the env, print
     `;; defined <name>`.
   - Otherwise: evaluate with `lang.evalExpr`, pretty-print the result.
5. Reset the accumulator and restore the language prompt.

### Note about typing of the LangConfig type

Why method syntax instead of arrow-function properties: TypeScript checks object method declarations **bivariantly**, so a precisely-typed LangConfig<L1Exp, L1CExp, L1Value, L1Env> is assignable to LangConfig<AllExp, AllCExp, AllValue, AllEnv> in the LANGUAGES record. With arrow-function property types (strict contra-variance) this would fail.

