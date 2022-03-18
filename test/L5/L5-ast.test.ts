import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { isNumExp, isBoolExp, isVarRef, isPrimOp, isProgram, isDefineExp, isVarDecl,
         isAppExp, isStrExp, isIfExp, isProcExp, isLetExp, isLitExp, isLetrecExp, isSetExp,
         parseL5Exp, unparse, Exp, parseL5 } from "../../src/L5/L5-ast";
import { parse as parseSexp } from "../../src/shared/parser";
import { isRightT } from "../shared/test-helpers";

const p = (x: string): E.Either<string, Exp> => pipe(parseSexp(x), E.chain(parseL5Exp));

describe('L5 Parser', () => {
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
        expect(parseL5("(L5 (define x 1) (> (+ x 1) (* x x)))")).toSatisfy(isRightT(isProgram));
    });

    it('parses "define" expressions', () => {
        const def = p("(define x 1)");
        expect(def).toSatisfy(isRightT(isDefineExp));
        if (isRightT(isDefineExp)(def)) {
            expect(def.right.var).toSatisfy(isVarDecl);
            expect(def.right.val).toSatisfy(isNumExp);
        }
    });

    it('parses "define" expressions with type annotations', () => {
        const define = "(define (a : number) 1)";
        expect(p(define)).toSatisfy(isRightT(isDefineExp));
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

    it('parses procedures with type annotations', () => {
        expect(p("(lambda ((x : number)) : number (* x x))")).toSatisfy(isRightT(isProcExp));
    });

    it('parses "let" expressions', () => {
        expect(p("(let ((a 1) (b #t)) (if b a (+ a 1)))")).toSatisfy(isRightT(isLetExp));
    });

    it('parses "let" expressions with type annotations', () => {
        expect(p("(let (((a : boolean) #t) ((b : number) 2)) (if a b (+ b b)))")).toSatisfy(isRightT(isLetExp));
    });

    it('parses literal expressions', () => {
        expect(p("'a")).toSatisfy(isRightT(isLitExp));
        expect(p("'()")).toSatisfy(isRightT(isLitExp));
        expect(p("'(1)")).toSatisfy(isRightT(isLitExp));
    });

    it('parses "letrec" expressions', () => {
        expect(p("(letrec ((e (lambda (x) x))) (e 2))")).toSatisfy(isRightT(isLetrecExp));
    });

    it('parses "letrec" expressions with type annotations', () => {
        expect(p("(letrec (((p : (number * number -> number)) (lambda ((x : number) (y : number)) (+ x y)))) (p 1 2))")).toSatisfy(isRightT(isLetrecExp));
    });

    it('parses "set!" expressions', () => {
        expect(p("(set! x 1)")).toSatisfy(isRightT(isSetExp));
    });
});

describe('L5 Unparse', () => {
    const roundTrip = (x: string): E.Either<string, string> => pipe(p(x), E.chain(unparse));

    it('unparses "define" expressions with type annotations', () => {
        const define = "(define (a : number) 1)";
        expect(roundTrip(define)).toEqual(E.of(define));
    });

    it('unparses procedures with type annotations', () => {
        const lambda = "(lambda ((x : number)) : number (* x x))";
        expect(roundTrip(lambda)).toEqual(E.of(lambda));
    });

    it('unparses "let" expressions with type annotations', () => {
        const let1 = "(let (((a : boolean) #t) ((b : number) 2)) (if a b (+ b b)))";
        expect(roundTrip(let1)).toEqual(E.of(let1));
    });

    it('unparses "letrec" expressions', () => {
        const letrec = "(letrec (((p : (number * number -> number)) (lambda ((x : number) (y : number)) (+ x y)))) (p 1 2))";
        expect(roundTrip(letrec)).toEqual(E.of(letrec));
    });
});
