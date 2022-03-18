import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { isBoolExp, isNumExp, isPrimOp, isStrExp, isVarDecl, isVarRef, isSetExp,
    isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp, isLitExp, isProcExp, isProgram,
    parseL4, unparse, parseL4Exp, Exp } from '../../src/L4/L4-ast';
import { parse as parseSexp } from "../../src/shared/parser";
import { isRightT } from "../shared/test-helpers";

const p = (x: string): E.Either<string, Exp> => pipe(parseSexp(x), E.chain(parseL4Exp));

describe('L4 Parser', () => {
    it('parses atomic expressions', () => {
        expect(p("1")).toSatisfy(isRightT(isNumExp));
        expect(p("#t")).toSatisfy(isRightT(isBoolExp));
        expect(p("x")).toSatisfy(isRightT(isVarRef));
        expect(p('"a"')).toSatisfy(isRightT(isStrExp));
        expect(p(">")).toSatisfy(isRightT(isPrimOp));
        expect(p("=")).toSatisfy(isRightT(isPrimOp));
        expect(p("string=?")).toSatisfy(isRightT(isPrimOp));
        expect(p("eq?")).toSatisfy(isRightT(isPrimOp));
        expect(p("cons")).toSatisfy(isRightT(isPrimOp));
    });

    it('parses programs', () => {
        expect(parseL4("(L4 (define x 1) (> (+ x 1) (* x x)))")).toSatisfy(isRightT(isProgram));
    });

    it('parses "define" expressions', () => {
        const def = p("(define x 1)");
        expect(def).toSatisfy(isRightT(isDefineExp));
        if (isRightT(isDefineExp)(def)) {
            expect(def.right.var).toSatisfy(isVarDecl);
            expect(def.right.val).toSatisfy(isNumExp);
        }
    });

    it('parses applications', () => {
        expect(p("(> x 1)")).toSatisfy(isRightT(isAppExp));
        expect(p("(> (+ x x) (* x x))")).toSatisfy(isRightT(isAppExp));
    });

    it('parses "if" expressions', () => {
        expect(p("(if #t 1 2)")).toSatisfy(isRightT(isIfExp));
        expect(p("(if (< x 2) x 2)")).toSatisfy(isRightT(isIfExp));
    });

    it('parses procedures', () => {
        expect(p("(lambda () 1)")).toSatisfy(isRightT(isProcExp));
        expect(p("(lambda (x) x x)")).toSatisfy(isRightT(isProcExp));
    });

    it('parses "let" expressions', () => {
        expect(p("(let ((a 1) (b #t)) (if b a (+ a 1)))")).toSatisfy(isRightT(isLetExp));
    });

    it('parses literal expressions', () => {
        expect(p("'a")).toSatisfy(isRightT(isLitExp));
        expect(p("'()")).toSatisfy(isRightT(isLitExp));
        expect(p("'(1)")).toSatisfy(isRightT(isLitExp));
    });

    it('parses "letrec" expressions', () => {
        expect(p("(letrec ((e (lambda (x) x))) (e 2))")).toSatisfy(isRightT(isLetrecExp));
    });

    it('parses "set!" expressions', () => {
        expect(p("(set! x (+ 1 2))")).toSatisfy(isRightT(isSetExp));
    });

    describe("Failures", () => {
        it("returns a Failure when parsing a single-token program", () => {
            expect(parseL4("x")).toSatisfy(E.isLeft);
        });

        it("returns a Failure when parsing an empty program", () => {
            expect(parseL4("")).toSatisfy(E.isLeft);
        });

        it("returns a Failure if the program does not start with (L3 ...)", () => {
            expect(parseL4("(+ 1 2)")).toSatisfy(E.isLeft);
        });

        it("returns a Failure for a program with no Exps", () => {
            expect(parseL4("(L4)")).toSatisfy(E.isLeft);
        });
    
        it("returns a Failure if a program has an empty Exp", () => {
            expect(parseL4("(L4 ())")).toSatisfy(E.isLeft);
        });

        it('returns a Failure for an ill-formed "define"', () => {
            expect(p("(define)")).toSatisfy(E.isLeft);
            expect(p("(define x)")).toSatisfy(E.isLeft);
            expect(p("(define x y z)")).toSatisfy(E.isLeft);
            expect(p('(define "1" y)')).toSatisfy(E.isLeft);
            expect(p('(define 1 y)')).toSatisfy(E.isLeft);
        });

        it('returns a Failure for an ill-formed "set!"', () => {
            expect(p("(set!)")).toSatisfy(E.isLeft);
            expect(p("(set! x)")).toSatisfy(E.isLeft);
            expect(p("(set! x y z)")).toSatisfy(E.isLeft);
            expect(p('(set! "1" y)')).toSatisfy(E.isLeft);
            expect(p('(set! 1 y)')).toSatisfy(E.isLeft);
        });

        it("returns a Failure for an empty CExp", () => {
            expect(p("(+ ())")).toSatisfy(E.isLeft);
        });

        it("returns a Failure for an ill-formed special form", () => {
            expect(p("(if)")).toSatisfy(E.isLeft);
            expect(p("(if 1)")).toSatisfy(E.isLeft);
            expect(p("(lambda x x)")).toSatisfy(E.isLeft);
            expect(p("(let x x)")).toSatisfy(E.isLeft);
            expect(p("(let (x y) x)")).toSatisfy(E.isLeft);
            expect(p("(let ((1 y)) x)")).toSatisfy(E.isLeft);
        });
    });
});

describe('L4 Unparse', () => {
    const roundTrip = (x: string): E.Either<string, string> => pipe(p(x), E.map(unparse));

    it("doesn't change concrete values", () => {
        const concretes = ["1", "#t", "x", '"a"', ">", "=", "string=?", "eq?", "cons"];
        concretes.forEach(concrete => {
            expect(roundTrip(concrete)).toEqual(E.of(concrete));
        });
    });

    it('unparses programs', () => {
        const program = "(L4 (define x 1) (> (+ x 1) (* x x)))";
        expect(roundTrip(program)).toEqual(E.of(program));
    });

    it('unparses "define" expressions', () => {
        const define = "(define x 1)";
        expect(roundTrip(define)).toEqual(E.of(define));
    });

    it('unparses applications', () => {
        const app1 = "(> x 1)";
        const app2 = "(> (+ x x) (* x x))";
        expect(roundTrip(app1)).toEqual(E.of(app1));
        expect(roundTrip(app2)).toEqual(E.of(app2));
    });

    it('unparses "if" expressions', () => {
        const if1 = "(if #t 1 2)";
        const if2 = "(if (< x 2) x 2)";
        expect(roundTrip(if1)).toEqual(E.of(if1));
        expect(roundTrip(if2)).toEqual(E.of(if2));
    });

    it('unparses procedures', () => {
        const proc1 = "(lambda () 1)";
        const proc2 = "(lambda (x) x x)";
        expect(roundTrip(proc1)).toEqual(E.of(proc1));
        expect(roundTrip(proc2)).toEqual(E.of(proc2));
    });

    it('unparses "let" expressions', () => {
        const let1 = "(let ((a 1) (b #t)) (if b a (+ a 1)))";
        expect(roundTrip(let1)).toEqual(E.of(let1));
    });

    it('unparses literal expressions', () => {
        const lits = ["'a", "'()", "'(1)", "'(1 . 2)", "'(1 2 . 3)"];
        lits.forEach(lit => {
            expect(roundTrip(lit)).toEqual(E.of(lit));
        });
    });

    it('normalizes dotted pairs', () => {
        const dp1 = "'(1 . (2 . 3))";
        expect(roundTrip(dp1)).toEqual(E.of("'(1 2 . 3)"));
    });

    it('unparses "letrec" expressions', () => {
        const letrec1 = "(letrec ((f (lambda (x) x))) (f 2))";
        expect(roundTrip(letrec1)).toEqual(E.of(letrec1));
    });

    it('unparses "set!" expressions', () => {
        const set1 = "(set! x (+ 1 2))";
        expect(roundTrip(set1)).toEqual(E.of(set1));
    });
});