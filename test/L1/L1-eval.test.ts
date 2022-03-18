import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { parseL1, parseL1Exp, Exp, makePrimOp, makeBoolExp } from "../../src/L1/L1-ast";
import { evalL1program, makeEnv, makeEmptyEnv, evalSequence } from '../../src/L1/L1-eval';
import { parse as p } from "../../src/shared/parser";

describe('L1 Eval', () => {
    it('Evaluates a program without an explicit environment', () => {
        expect(pipe(parseL1(`(L1 (define x 3) + #t (+ (* x x) (+ x x)))`), E.chain(evalL1program))).toEqual(E.of(15));
        expect(pipe(parseL1(`(L1 (define x 3) + #t (/ (* x x) (- (+ x x) x)))`), E.chain(evalL1program))).toEqual(E.of(3));
    });

    it('Evaluates all arithmetic primitives', () => {
        expect(pipe(parseL1(`(L1 *)`), E.chain(evalL1program))).toEqual(E.of(makePrimOp('*')));
        expect(pipe(parseL1(`(L1 (- 6 3))`), E.chain(evalL1program))).toEqual(E.of(3));
        expect(pipe(parseL1(`(L1 (/ 6 3))`), E.chain(evalL1program))).toEqual(E.of(2));
        expect(pipe(parseL1(`(L1 (+ 6 3))`), E.chain(evalL1program))).toEqual(E.of(9));
        expect(pipe(parseL1(`(L1 (* 6 3))`), E.chain(evalL1program))).toEqual(E.of(18));
    });

    it('Evaluates all boolean primitives', () => {
        expect(pipe(parseL1(`(L1 (> 6 3))`), E.chain(evalL1program))).toEqual(E.of(true));
        expect(pipe(parseL1(`(L1 (< 6 3))`), E.chain(evalL1program))).toEqual(E.of(false));
        expect(pipe(parseL1(`(L1 (= 6 6))`), E.chain(evalL1program))).toEqual(E.of(true));
        expect(pipe(parseL1(`(L1 (= 6 3))`), E.chain(evalL1program))).toEqual(E.of(false));
        expect(pipe(parseL1(`(L1 (not #t))`), E.chain(evalL1program))).toEqual(E.of(false));
    });

    it('Evaluates a program with an explicit environment', () => {
        const env1 = makeEnv("x", 1, makeEmptyEnv());
        const result1 = pipe(
            p("(+ x 2)"),
            E.chain(parseL1Exp),
            E.chain(exp => evalSequence([exp], env1))
        );
        expect(result1).toEqual(E.of(3));
        
        const env2 = makeEnv("x", 1, makeEnv("y", 2, makeEmptyEnv()));
        const result2 = pipe(
            p("(+ y 2)"),
            E.chain(parseL1Exp),
            E.chain(exp => evalSequence([exp], env2))
        );
        expect(result2).toEqual(E.of(4));
    });

    describe("Failures", () => {
        it("returns a Failure when accessing a variable in an empty env", () => {
            const env = makeEmptyEnv();
            const result = pipe(
                p("(+ y 2)"),
                E.chain(parseL1Exp),
                E.chain(exp => evalSequence([exp], env))
            );
            expect(result).toSatisfy(E.isLeft);
        });

        it("returns a Failure when accessing a variable not present in the env", () => {
            const env = makeEnv("x", 1, makeEmptyEnv());
            const result = pipe(
                p("(+ y 2)"),
                E.chain(parseL1Exp),
                E.chain((exp: Exp) => evalSequence([exp], env))
            );
            expect(result).toSatisfy(E.isLeft);
        });

        it("returns a Failure when evaluating an empty sequence of Exps", () => {
            expect(evalL1program({ tag: "Program", exps: []})).toSatisfy(E.isLeft);
        });

        it("returns a Failure for an unknown primitive op", () => {
            expect(pipe(
                p("(eq? 1 1)"),
                E.chain(parseL1Exp),
                E.chain(exp => evalSequence([exp], makeEmptyEnv()))
            )).toSatisfy(E.isLeft);
        })
    });
});
