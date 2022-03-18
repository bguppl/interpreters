import * as E from "fp-ts/Either";
import { and } from "fp-ts/Predicate";
import { pipe } from "fp-ts/function";
import * as S from "../../src/L5/L5-substitution-adt";
import { makeTVar, parseTE, unparseTExp, TExp } from "../../src/L5/TExp";
import { sub, subToStr } from '../shared/test-helpers';

describe('L5 Substitution ADT', () => {
    describe('makeSub', () => {
        it('returns an error for circular dependencies', () => {
            const te1 = parseTE("(number -> T1)");
            const sub1 = pipe(te1, E.chain((te: TExp) => S.makeSub([makeTVar("T1")], [te])));
            expect(sub1).toSatisfy(E.isLeft);
        });
    });

    describe('applySub', () => {
        it('applies a substitution on a type expression', () => {
            const sub1 = sub(["T1", "T2"], ["number", "boolean"]);
            const te1 = parseTE("(T1 * T2 -> T1)");
            const unparsed = pipe(
                sub1,
                E.chain(sub => pipe(
                    te1,
                    E.chain(te => unparseTExp(S.applySub(sub)(te)))
                ))
            );
            expect(unparsed).toEqual(E.of("(number * boolean -> number)"));
        });
    });

    describe('combineSub', () => {
        it('combines substitutions (the o operator)', () => {
            // {T1:(number -> S1), T2:(number -> S4)} o {T3:(number -> S2)} =>
            // {T1:(number -> S1), T2:(number -> S4), T3:(number -> S2)}
            const sub11 = sub(["T1", "T2"], ["(number -> S1)", "(number -> S4)"]);
            const sub12 = sub(["T3"], ["(number -> S2)"]);
            const expected1 = pipe(sub(["T1", "T2", "T3"], ["(number -> S1)", "(number -> S4)", "(number -> S2)"]), E.chain(subToStr));
            const res1 = pipe(
                sub11,
                E.chain(s1 => pipe(
                    sub12,
                    E.chain(s2 => S.combineSub(s1, s2))
                )),
                E.chain(subToStr)
            );
            expect(res1).toEqual(expected1);

            // {T1:(number -> S1), T2:(number -> T5)} o {T3:(number -> S2), T4:(number -> S1), T5:boolean} =>
            // {T1:(number -> S1), T2:(number -> boolean), T3:(number -> S2), T4:(number -> S1), T5:boolean}
            const sub21 = sub(["T1", "T2"], ["(number -> S1)", "(number -> T5)"]);
            const sub22 = sub(["T3", "T4", "T5"], ["(number -> S2)", "(number -> S1)", "boolean"]);
            const expected2 = pipe(sub(["T1", "T2", "T3", "T4", "T5"], ["(number -> S1)", "(number -> boolean)", "(number -> S2)", "(number -> S1)", "boolean"]), E.chain(subToStr));
            const res2 = pipe(
                sub21,
                E.chain(s1 => pipe(
                    sub22,
                    E.chain(s2 => S.combineSub(s1, s2))
                )),
                E.chain(subToStr)
            );
            expect(res2).toEqual(expected2);

            // {T1:(number -> S1), T2:(T5 -> T4)} o {S1:boolean, T3:(number -> S2), T5:(number -> S1), T4:boolean} =>
            // {T1:{number -> boolean}, T2:((number -> S1) -> boolean), T3:(number -> S2), T4:boolean, T5:(number -> S1), S1:boolean}
            const sub31 = sub(["T1", "T2"], ["(number -> S1)", "(T5 -> T4)"]);
            const sub32 = sub(["S1", "T3", "T4", "T5"], ["boolean", "(number -> S2)", "boolean", "(number -> S1)"]);
            const expected3 = pipe(sub(["T1", "T2", "T3", "T4", "T5", "S1"], ["(number -> boolean)", "((number -> S1) -> boolean)", "(number -> S2)", "boolean", "(number -> S1)", "boolean"]), E.chain(subToStr));
            const res3 = pipe(
                sub31,
                E.chain(s1 => pipe(
                    sub32,
                    E.chain(s2 => S.combineSub(s1, s2))
                )),
                E.chain(subToStr)
            );
            expect(res3).toEqual(expected3);


            // {T1:S1, T2:(S2 -> number), T3:boolean} o {S1:(T5 -> (number * T2 -> T2)), S2:T3} =>
            // {T1:(T5 -> (number * T2 -> T2)), T2:(T3 -> number), T3:boolean, S1:(T5 -> (number * T2 -> T2)), S2:T3}
            const sub41 = sub(["T1", "T2", "T3"], ["S1", "(S2 -> number)", "boolean"]);
            const sub42 = sub(["S1", "S2"], ["(T5 -> (number * T2 -> T2))", "T3"]);
            const expected4 = pipe(sub(["T1", "T2", "T3", "S1", "S2"], ["(T5 -> (number * T2 -> T2))", "(T3 -> number)", "boolean", "(T5 -> (number * T2 -> T2))", "T3"]), E.chain(subToStr));
            const res4 = pipe(
                sub41,
                E.chain(s1 => pipe(
                    sub42,
                    E.chain(s2 => S.combineSub(s1, s2))
                )),
                E.chain(subToStr)
            );
            expect(res4).toEqual(expected4);

            // {T1:S1, T2:(S2 -> number), T3:boolean} o {S1:(T5 -> (number * T2 -> T2)), S2:T3} =>
            // {T1:(T5 -> (number * T2 -> T2)), T2:(T3 -> number), T3:boolean, S1:(T5 -> (number * T2 -> T2)), S2:T3}
            const sub51 = sub(["T1", "T2", "T3"], ["S1", "(S2 -> number)", "boolean"]);
            const sub52 = sub(["S1", "S2"], ["(T5 -> (number * T2 -> T2))", "T3"]);
            const expected5 = pipe(sub(["T1", "T2", "T3", "S1", "S2"], ["(T5 -> (number * T2 -> T2))", "(T3 -> number)", "boolean", "(T5 -> (number * T2 -> T2))", "T3"]), E.chain(subToStr));
            const res5 = pipe(
                sub51,
                E.chain(s1 => pipe(
                    sub52,
                    E.chain(s2 => S.combineSub(s1, s2))
                )),
                E.chain(subToStr)
            );
            expect(res5).toEqual(expected5);

            // {T1:number, T2:(T4 -> number), T3:T9} o {T4:(T1 -> number), T5:boolean, T6:T7} =>
            // {T1:number, T2:((T1 -> number) -> number), T3:T9, T4:(T1 -> number), T5:boolean, T6:T7}
            const sub61 = sub(["T1", "T2", "T3"], ["number", "(T4 -> number)", "T9"]);
            const sub62 = sub(["T4", "T5", "T6"], ["(T1 -> number)", "boolean", "T7"]);
            const expected6 = pipe(sub(["T1", "T2", "T3", "T4", "T5", "T6"], ["number", "((T1 -> number) -> number)", "T9", "(T1 -> number)", "boolean", "T7"]), E.chain(subToStr));
            const res6 = pipe(
                sub61,
                E.chain(s1 => pipe(
                    sub62,
                    E.chain(s2 => S.combineSub(s1, s2))
                )),
                E.chain(subToStr)
            );
            expect(res6).toEqual(expected6);
        });

        it('returns an error for circular substitutions', () => {
            // {T3:boolean, S1:(number -> T2), T4:(number -> S1), T5:boolean} o {T1:(number -> S1), T2:(T3 -> S1)}
            const sub1 = sub(["T3", "T4", "T5", "S1"], ["boolean", "(number -> S1)", "boolean", "(number -> T2)"]);
            const sub2 = sub(["T1", "T2"], ["(number -> S1)", "(T3 -> S1)"]);
            const res = pipe(
                sub1,
                E.chain(s1 => pipe(
                    sub2,
                    E.chain(s2 => S.combineSub(s1, s2))
                ))
            );
            expect(res).toSatisfy(E.isLeft);
        });

        it('combines substitutions with overlapping variables', () => {
            // {T3:boolean, S1:(number -> T2), T4:(number -> S1), T5:boolean} o {T1:(number -> S1), T2:(T3 -> S1)}
            const sub1 = sub(["T7", "T8"], ["number", "(T5 * number -> T3)"]);
            const sub2 = sub(["T5", "T8"], ["T7", "boolean"]);
            const expected = pipe(sub(["T5", "T7", "T8"], ["T7", "number", "(T7 * number -> T3)"]), E.chain(subToStr));
            const res = pipe(
                sub1,
                E.chain(s1 => pipe(
                    sub2,
                    E.chain(s2 => S.combineSub(s1, s2))
                )),
                E.chain(subToStr)
            );
            expect(res).toEqual(expected);
        });
    });

    describe('extendSub', () => {
        it('extends substitutions', () => {
            // {T3:boolean, S1:(number -> T2), T4:(number -> S1), T5:boolean} o {T1:(number -> S1), T2:(T3 -> S1)}
            const sub1 = sub(["T1", "T2", "T3"], ["S1", "(S1 -> number)", "boolean"]);
            const v2 = makeTVar("S1");
            const t2 = parseTE("(T21 -> (number * T23 -> T22))");
            const expected = pipe(sub(["T1", "T2", "T3", "S1"], ["(T21 -> (number * T23 -> T22))", "((T21 -> (number * T23 -> T22)) -> number)", "boolean", "(T21 -> (number * T23 -> T22))"]), E.chain(subToStr));
            const res = pipe(
                sub1,
                E.chain(sub => pipe(
                    t2,
                    E.chain(te => S.extendSub(sub, v2, te)))
                ),
                E.chain(subToStr)
            );
            expect(res).toEqual(expected);
        });
    });
});
