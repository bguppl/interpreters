// repl.ts
// Interactive Read-Eval-Print Loop for the PPL interpreters.
//
// Features:
//   • Persistent command history (saved to ~/.interpreters_history)
//   • Multi-line input — accumulates until parentheses are balanced
//   • Language switching: /lang L1|L3|L4|L5|L7
//   • Global environment persists across iterations per language
//   • Pretty-printed output for complex s-expressions
//   • readline keyboard shortcuts: ↑/↓ history, Ctrl+A/E, Ctrl+K/Y, Ctrl+D

import * as readline from 'readline';
import * as fs       from 'fs';
import * as path     from 'path';
import * as os       from 'os';

import { parse as parseSexp } from '../shared/parser';
import { either }             from '../shared/result';
import { Sexp }               from 's-expression';

import {
    AnyLangConfig, AllExp, AllValue, AllEnv,
    LANGUAGES, DEFAULT_LANGUAGE,
    isDefine, defineVarName, defineRHS,
} from './languages';
import { parenDepth }  from './paren-balance';
import { prettyPrint } from './pretty-print';

// ============================================================
// History file
// ============================================================

const HISTORY_FILE = process.env.REPL_HISTORY ??
    path.join(os.homedir(), '.interpreters_history');
const HISTORY_SIZE = 1000;

const loadHistory = (): string[] => {
    try {
        return fs.readFileSync(HISTORY_FILE, 'utf-8')
            .split('\n')
            .filter(l => l.trim())
            .slice(-HISTORY_SIZE);
    } catch {
        return [];
    }
};

const saveHistory = (lines: string[]): void => {
    try {
        fs.writeFileSync(
            HISTORY_FILE,
            lines.slice(-HISTORY_SIZE).join('\n') + '\n',
            'utf-8',
        );
    } catch { /* ignore write errors */ }
};

// ============================================================
// Deal with comment in input (strip them before checking for parentheses or parsing)
// ============================================================

const stripComments = (s: string): string => s.replace(/;[^\r\n]*/g, '');

// ============================================================
// REPL state
// ============================================================

type ReplState = {
    lang:  AnyLangConfig;
    env:   AllEnv;      // L1/L3/L4: typed Env; L5/L7: null (uses theGlobalEnv)
    accum: string;      // multi-line accumulator
};

const makeInitialState = (lang: AnyLangConfig): ReplState => ({
    lang,
    env:   lang.makeInitialEnv(),
    accum: '',
});

// ============================================================
// Evaluate one complete expression and print the result
// ============================================================

const evaluateInput = (input: string, state: ReplState): void => {
    const stripped = stripComments(input).trim();
    if (!stripped) return;

    either(
        parseSexp(stripped),
        (sexp: Sexp) => {
            either(
                state.lang.parseExpr(sexp),
                (exp: AllExp) => {
                    if (isDefine(exp)) {
                        const varName = defineVarName(exp);
                        const rhs     = defineRHS(exp);
                        either(
                            state.lang.evalDefineRHS(rhs, state.env),
                            (val: AllValue) => {
                                state.env = state.lang.extendEnv(state.env, varName, val);
                                console.log(`;; defined ${varName}`);
                            },
                            (msg: string) => console.error(`Error: ${msg}`),
                        );
                    } else {
                        either(
                            state.lang.evalExpr(exp, state.env),
                            (val: AllValue) =>
                                console.log(prettyPrint(state.lang.display(val))),
                            (msg: string) => console.error(`Error: ${msg}`),
                        );
                    }
                },
                (msg: string) => console.error(`Parse error: ${msg}`),
            );
        },
        (msg: string) => console.error(`Parse error: ${msg}`),
    );
};

// ============================================================
// Command dispatcher
// ============================================================

const HELP_TEXT = `
PPL Interpreter REPL — commands
  /lang <L1|L3|L4|L5|L7>   Switch language (resets environment)
  /reset                     Reset current language environment
  /primitives                List primitive operations in current language  
  /help                      Show this help
  /quit  /exit               Exit the REPL

Keyboard shortcuts
  ↑ / ↓           Navigate history
  Ctrl+A / Ctrl+E  Jump to start / end of line
  Ctrl+K           Cut from cursor to end of line
  Ctrl+Y           Paste (yank) cut text
  Ctrl+D           Exit
`;

const handleCommand = (
    cmd: string,
    state: ReplState,
    rl: readline.Interface,
): ReplState => {
    const parts    = cmd.trim().split(/\s+/);
    const name     = parts[0].toLowerCase();

    if (name === '/help') {
        console.log(HELP_TEXT);
        console.log(`Current language: ${state.lang.name}`);
        return state;
    }

    if (name === '/reset') {
        const fresh = makeInitialState(state.lang);
        console.log(`;; Environment reset for ${state.lang.name}`);
        return fresh;
    }

    if (name === '/primitives') {
        const prims = state.lang.primitives.join(', ');
        console.log(`;; Primitives for ${state.lang.name}: ${prims}`);
        return state;
    }

    if (name === '/lang') {
        const requested = (parts[1] ?? '').toUpperCase();
        const cfg = LANGUAGES[requested];
        if (!cfg) {
            const choices = Object.keys(LANGUAGES).join(', ');
            console.error(`Unknown language: ${requested}. Choose from: ${choices}`);
            return state;
        }
        const fresh = makeInitialState(cfg);
        console.log(`;; Switched to ${cfg.name}`);
        rl.setPrompt(`${cfg.name}> `);
        return fresh;
    }

    if (name === '/quit' || name === '/exit') {
        rl.close();
        return state;
    }

    console.error(`Unknown command: ${parts[0]}. Type /help for available commands.`);
    return state;
};

// ============================================================
// Main
// ============================================================

const main = (): void => {
    const initialLang = LANGUAGES[DEFAULT_LANGUAGE];
    let state: ReplState = makeInitialState(initialLang);

    const rl = readline.createInterface({
        input:    process.stdin,
        output:   process.stdout,
        terminal: true,
        historySize: HISTORY_SIZE,
        removeHistoryDuplicates: true,
    } as readline.ReadLineOptions);

    // Restore saved history
    const savedHistory = loadHistory();
    // readline stores history newest-first
    (rl as any).history = [...savedHistory].reverse();

    // Banner
    console.log(`\nPPL Interpreter REPL  (type /help for commands)`);
    console.log(`Current language: ${initialLang.name}\n`);
    rl.setPrompt(`${initialLang.name}> `);
    rl.prompt();

    // ---- line handler ----
    rl.on('line', (line: string) => {
        const stripped = stripComments(line);

        // A command is recognized only when the accumulator is empty
        if (state.accum === '' && stripped.trim().startsWith('/')) {
            state = handleCommand(stripped.trim(), state, rl);
            rl.setPrompt(`${state.lang.name}> `);
            rl.prompt();
            return;
        }

        // Accumulate
        state.accum += (state.accum ? '\n' : '') + stripped;
        const depth = parenDepth(state.accum);

        if (depth <= 0 && state.accum.trim() !== '') {
            // Complete expression — evaluate
            evaluateInput(state.accum, state);
            state.accum = '';
            rl.setPrompt(`${state.lang.name}> `);
        } else if (depth > 0) {
            // Open parens — ask for continuation
            rl.setPrompt('...> ');
        } else {
            // Only whitespace / comments
            state.accum = '';
            rl.setPrompt(`${state.lang.name}> `);
        }

        rl.prompt();
    });

    // ---- Ctrl+C: cancel current input or exit ----
    rl.on('SIGINT', () => {
        if (state.accum) {
            console.log('\n;; Input cancelled');
            state.accum = '';
            rl.setPrompt(`${state.lang.name}> `);
            rl.prompt();
        } else {
            console.log('');
            rl.close();
        }
    });

    // ---- Ctrl+D / close: save history and exit ----
    rl.on('close', () => {
        const hist: string[] = (rl as any).history ?? [];
        saveHistory([...hist].reverse());
        console.log('\nBye!');
        process.exit(0);
    });
};

main();
