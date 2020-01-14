import { expect } from 'chai';
import * as S from "../../src/L5/L5-substitution-adt";
import { makeTVar, parseTE, unparseTExp } from "../../src/L5/TExp";
import { isError } from '../../src/shared/error';
import { assertEqSub, sub, p } from './test-helpers';

describe('L5 Substitution ADT', () => {
    describe('makeSub', () => {
        it('returns an error for circular dependencies', () => {
            const te1 = parseTE("(number -> T1)");
            if (!isError(te1)) {
                const sub1 = S.makeSub([makeTVar("T1")], [te1]);
                expect(sub1).to.satisfy(isError);
            }
        });
    });

    describe('applySub', () => {
        it('applies a substitution on a type expression', () => {
            const sub1 = sub(["T1", "T2"], ["number", "boolean"]);
            const te1 = parseTE("(T1 * T2 -> T1)");
            if (!isError(te1)) {
                const te2 = S.applySub(sub1, te1);
                expect(unparseTExp(te2)).to.equal("(number * boolean -> number)");
            }
        });
    });

    describe('combineSub', () => {
        it('combines substitutions (the o operator)', () => {
            // {T1:(number -> S1), T2:(number -> S4)} o {T3:(number -> S2)} =>
            // {T1:(number -> S1), T2:(number -> S4), T3:(number -> S2)}
            const sub11 = sub(["T1", "T2"], ["(number -> S1)", "(number -> S4)"]);
            const sub12 = sub(["T3"], ["(number -> S2)"]);
            const expected1 = sub(["T1", "T2", "T3"], ["(number -> S1)", "(number -> S4)", "(number -> S2)"]);
            const res1 = S.combineSub(sub11, sub12);
            assertEqSub(res1, expected1);

            // {T1:(number -> S1), T2:(number -> T5)} o {T3:(number -> S2), T4:(number -> S1), T5:boolean} =>
            // {T1:(number -> S1), T2:(number -> boolean), T3:(number -> S2), T4:(number -> S1), T5:boolean}
            const sub21 = sub(["T1", "T2"], ["(number -> S1)", "(number -> T5)"]);
            const sub22 = sub(["T3", "T4", "T5"], ["(number -> S2)", "(number -> S1)", "boolean"]);
            const expected2 = sub(["T1", "T2", "T3", "T4", "T5"], ["(number -> S1)", "(number -> boolean)", "(number -> S2)", "(number -> S1)", "boolean"]);
            const res2 = S.combineSub(sub21, sub22);
            assertEqSub(res2, expected2);

            // {T1:(number -> S1), T2:(T5 -> T4)} o {S1:boolean, T3:(number -> S2), T5:(number -> S1), T4:boolean} =>
            // {T1:{number -> boolean}, T2:((number -> S1) -> boolean), T3:(number -> S2), T4:boolean, T5:(number -> S1), S1:boolean}
            const sub31 = sub(["T1", "T2"], ["(number -> S1)", "(T5 -> T4)"]);
            const sub32 = sub(["S1", "T3", "T4", "T5"], ["boolean", "(number -> S2)", "boolean", "(number -> S1)"]);
            const expected3 = sub(["T1", "T2", "T3", "T4", "T5", "S1"], ["(number -> boolean)", "((number -> S1) -> boolean)", "(number -> S2)", "boolean", "(number -> S1)", "boolean"]);
            const res3 = S.combineSub(sub31, sub32);
            assertEqSub(res3, expected3);


            // {T1:S1, T2:(S2 -> number), T3:boolean} o {S1:(T5 -> (number * T2 -> T2)), S2:T3} =>
            // {T1:(T5 -> (number * T2 -> T2)), T2:(T3 -> number), T3:boolean, S1:(T5 -> (number * T2 -> T2)), S2:T3}
            const sub41 = sub(["T1", "T2", "T3"], ["S1", "(S2 -> number)", "boolean"]);
            const sub42 = sub(["S1", "S2"], ["(T5 -> (number * T2 -> T2))", "T3"]);
            const expected4 = sub(["T1", "T2", "T3", "S1", "S2"], ["(T5 -> (number * T2 -> T2))", "(T3 -> number)", "boolean", "(T5 -> (number * T2 -> T2))", "T3"]);
            const res4 = S.combineSub(sub41, sub42);
            assertEqSub(res4, expected4);

            // {T1:S1, T2:(S2 -> number), T3:boolean} o {S1:(T5 -> (number * T2 -> T2)), S2:T3} =>
            // {T1:(T5 -> (number * T2 -> T2)), T2:(T3 -> number), T3:boolean, S1:(T5 -> (number * T2 -> T2)), S2:T3}
            const sub51 = sub(["T1", "T2", "T3"], ["S1", "(S2 -> number)", "boolean"]);
            const sub52 = sub(["S1", "S2"], ["(T5 -> (number * T2 -> T2))", "T3"]);
            const expected5 = sub(["T1", "T2", "T3", "S1", "S2"], ["(T5 -> (number * T2 -> T2))", "(T3 -> number)", "boolean", "(T5 -> (number * T2 -> T2))", "T3"]);
            const res5 = S.combineSub(sub51, sub52);
            assertEqSub(res5, expected5);

            // {T1:number, T2:(T4 -> number), T3:T9} o {T4:(T1 -> number), T5:boolean, T6:T7} =>
            // {T1:number, T2:((T1 -> number) -> number), T3:T9, T4:(T1 -> number), T5:boolean, T6:T7}
            const sub61 = sub(["T1", "T2", "T3"], ["number", "(T4 -> number)", "T9"]);
            const sub62 = sub(["T4", "T5", "T6"], ["(T1 -> number)", "boolean", "T7"]);
            const expected6 = sub(["T1", "T2", "T3", "T4", "T5", "T6"], ["number", "((T1 -> number) -> number)", "T9", "(T1 -> number)", "boolean", "T7"]);
            const res6 = S.combineSub(sub61, sub62);
            assertEqSub(res6, expected6);
        });

        it('returns an error for circular substitutions', () => {
            // {T3:boolean, S1:(number -> T2), T4:(number -> S1), T5:boolean} o {T1:(number -> S1), T2:(T3 -> S1)}
            const sub1 = sub(["T3", "T4", "T5", "S1"], ["boolean", "(number -> S1)", "boolean", "(number -> T2)"]);
            const sub2 = sub(["T1", "T2"], ["(number -> S1)", "(T3 -> S1)"]);
            const res = S.combineSub(sub1, sub2);
            expect(res).to.satisfy(isError);
        });

        it('combines substitutions with overlapping variables', () => {
            // {T3:boolean, S1:(number -> T2), T4:(number -> S1), T5:boolean} o {T1:(number -> S1), T2:(T3 -> S1)}
            const sub1 = sub(["T7", "T8"], ["number", "(T5 * number -> T3)"]);
            const sub2 = sub(["T5", "T8"], ["T7", "boolean"]);
            const expected = sub(["T5", "T7", "T8"], ["T7", "number", "(T7 * number -> T3)"]);
            const res = S.combineSub(sub1, sub2);
            assertEqSub(res, expected);
        });
    });

    describe('extendSub', () => {
        it('extends substitutions', () => {
            // {T3:boolean, S1:(number -> T2), T4:(number -> S1), T5:boolean} o {T1:(number -> S1), T2:(T3 -> S1)}
            const sub1 = sub(["T1", "T2", "T3"], ["S1", "(S1 -> number)", "boolean"]);
            const v2 = makeTVar("S1");
            const t2 = p("(T21 -> (number * T23 -> T22))");
            const expected = sub(["T1", "T2", "T3", "S1"], ["(T21 -> (number * T23 -> T22))", "((T21 -> (number * T23 -> T22)) -> number)", "boolean", "(T21 -> (number * T23 -> T22))"]);
            const res = S.extendSub(sub1, v2, t2);
            assertEqSub(res, expected);
        });
    });
});
