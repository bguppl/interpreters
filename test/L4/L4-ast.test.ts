import { isBoolExp, isNumExp, isPrimOp, isStrExp, isVarDecl, isVarRef, isSetExp,
    isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp, isLitExp, isProcExp, isProgram,
    parseL4, unparse, parseL4Exp, Exp } from '../../src/L4/L4-ast';
import { Result, bind, isOkT, makeOk, isFailure } from '../../src/shared/result';
import { parse as parseSexp } from "../../src/shared/parser";

const p = (x: string): Result<Exp> => bind(parseSexp(x), parseL4Exp);

describe('L4 Parser', () => {
    it('parses atomic expressions', () => {
        expect(p("1")).toSatisfy(isOkT(isNumExp));
        expect(p("#t")).toSatisfy(isOkT(isBoolExp));
        expect(p("x")).toSatisfy(isOkT(isVarRef));
        expect(p('"a"')).toSatisfy(isOkT(isStrExp));
        expect(p(">")).toSatisfy(isOkT(isPrimOp));
        expect(p("=")).toSatisfy(isOkT(isPrimOp));
        expect(p("string=?")).toSatisfy(isOkT(isPrimOp));
        expect(p("eq?")).toSatisfy(isOkT(isPrimOp));
        expect(p("cons")).toSatisfy(isOkT(isPrimOp));
    });

    it('parses programs', () => {
        expect(parseL4("(L4 (define x 1) (> (+ x 1) (* x x)))")).toSatisfy(isOkT(isProgram));
    });

    it('parses "define" expressions', () => {
        const def = p("(define x 1)");
        expect(def).toSatisfy(isOkT(isDefineExp));
        if (isOkT(isDefineExp)(def)) {
            expect(def.value.var).toSatisfy(isVarDecl);
            expect(def.value.val).toSatisfy(isNumExp);
        }
    });

    it('parses applications', () => {
        expect(p("(> x 1)")).toSatisfy(isOkT(isAppExp));
        expect(p("(> (+ x x) (* x x))")).toSatisfy(isOkT(isAppExp));
    });

    it('parses "if" expressions', () => {
        expect(p("(if #t 1 2)")).toSatisfy(isOkT(isIfExp));
        expect(p("(if (< x 2) x 2)")).toSatisfy(isOkT(isIfExp));
    });

    it('parses procedures', () => {
        expect(p("(lambda () 1)")).toSatisfy(isOkT(isProcExp));
        expect(p("(lambda (x) x x)")).toSatisfy(isOkT(isProcExp));
    });

    it('parses "let" expressions', () => {
        expect(p("(let ((a 1) (b #t)) (if b a (+ a 1)))")).toSatisfy(isOkT(isLetExp));
    });

    it('parses literal expressions', () => {
        expect(p("'a")).toSatisfy(isOkT(isLitExp));
        expect(p("'()")).toSatisfy(isOkT(isLitExp));
        expect(p("'(1)")).toSatisfy(isOkT(isLitExp));
    });

    it('parses "letrec" expressions', () => {
        expect(p("(letrec ((e (lambda (x) x))) (e 2))")).toSatisfy(isOkT(isLetrecExp));
    });

    it('parses "set!" expressions', () => {
        expect(p("(set! x (+ 1 2))")).toSatisfy(isOkT(isSetExp));
    });

    describe("Failures", () => {
        it("returns a Failure when parsing a single-token program", () => {
            expect(parseL4("x")).toSatisfy(isFailure);
        });

        it("returns a Failure when parsing an empty program", () => {
            expect(parseL4("")).toSatisfy(isFailure);
        });

        it("returns a Failure if the program does not start with (L3 ...)", () => {
            expect(parseL4("(+ 1 2)")).toSatisfy(isFailure);
        });

        it("returns a Failure for a program with no Exps", () => {
            expect(parseL4("(L4)")).toSatisfy(isFailure);
        });
    
        it("returns a Failure if a program has an empty Exp", () => {
            expect(parseL4("(L4 ())")).toSatisfy(isFailure);
        });

        it('returns a Failure for an ill-formed "define"', () => {
            expect(p("(define)")).toSatisfy(isFailure);
            expect(p("(define x)")).toSatisfy(isFailure);
            expect(p("(define x y z)")).toSatisfy(isFailure);
            expect(p('(define "1" y)')).toSatisfy(isFailure);
            expect(p('(define 1 y)')).toSatisfy(isFailure);
        });

        it('returns a Failure for an ill-formed "set!"', () => {
            expect(p("(set!)")).toSatisfy(isFailure);
            expect(p("(set! x)")).toSatisfy(isFailure);
            expect(p("(set! x y z)")).toSatisfy(isFailure);
            expect(p('(set! "1" y)')).toSatisfy(isFailure);
            expect(p('(set! 1 y)')).toSatisfy(isFailure);
        });

        it("returns a Failure for an empty CExp", () => {
            expect(p("(+ ())")).toSatisfy(isFailure);
        });

        it("returns a Failure for an ill-formed special form", () => {
            expect(p("(if)")).toSatisfy(isFailure);
            expect(p("(if 1)")).toSatisfy(isFailure);
            expect(p("(lambda x x)")).toSatisfy(isFailure);
            expect(p("(let x x)")).toSatisfy(isFailure);
            expect(p("(let (x y) x)")).toSatisfy(isFailure);
            expect(p("(let ((1 y)) x)")).toSatisfy(isFailure);
        });
    });
});

describe('L4 Unparse', () => {
    const roundTrip = (x: string): Result<string> =>
        bind(p(x), (exp: Exp) => makeOk(unparse(exp)));

    it("doesn't change concrete values", () => {
        const concretes = ["1", "#t", "x", '"a"', ">", "=", "string=?", "eq?", "cons"];
        concretes.forEach(concrete => {
            expect(roundTrip(concrete)).toEqual(makeOk(concrete));
        });
    });

    it('unparses programs', () => {
        const program = "(L4 (define x 1) (> (+ x 1) (* x x)))";
        expect(roundTrip(program)).toEqual(makeOk(program));
    });

    it('unparses "define" expressions', () => {
        const define = "(define x 1)";
        expect(roundTrip(define)).toEqual(makeOk(define));
    });

    it('unparses applications', () => {
        const app1 = "(> x 1)";
        const app2 = "(> (+ x x) (* x x))";
        expect(roundTrip(app1)).toEqual(makeOk(app1));
        expect(roundTrip(app2)).toEqual(makeOk(app2));
    });

    it('unparses "if" expressions', () => {
        const if1 = "(if #t 1 2)";
        const if2 = "(if (< x 2) x 2)";
        expect(roundTrip(if1)).toEqual(makeOk(if1));
        expect(roundTrip(if2)).toEqual(makeOk(if2));
    });

    it('unparses procedures', () => {
        const proc1 = "(lambda () 1)";
        const proc2 = "(lambda (x) x x)";
        expect(roundTrip(proc1)).toEqual(makeOk(proc1));
        expect(roundTrip(proc2)).toEqual(makeOk(proc2));
    });

    it('unparses "let" expressions', () => {
        const let1 = "(let ((a 1) (b #t)) (if b a (+ a 1)))";
        expect(roundTrip(let1)).toEqual(makeOk(let1));
    });

    it('unparses literal expressions', () => {
        const lits = ["'a", "'()", "'(1)", "'(1 . 2)", "'(1 2 . 3)"];
        lits.forEach(lit => {
            expect(roundTrip(lit)).toEqual(makeOk(lit));
        });
    });

    it('normalizes dotted pairs', () => {
        const dp1 = "'(1 . (2 . 3))";
        expect(roundTrip(dp1)).toEqual(makeOk("'(1 2 . 3)"));
    });

    it('unparses "letrec" expressions', () => {
        const letrec1 = "(letrec ((f (lambda (x) x))) (f 2))";
        expect(roundTrip(letrec1)).toEqual(makeOk(letrec1));
    });

    it('unparses "set!" expressions', () => {
        const set1 = "(set! x (+ 1 2))";
        expect(roundTrip(set1)).toEqual(makeOk(set1));
    });
});