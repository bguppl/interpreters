import { expect } from 'chai';
import { isBoolExp, isNumExp, isPrimOp, isStrExp, isVarDecl, isVarRef, isSetExp,
    isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp, isLitExp, isProcExp, isProgram,
    parseL4, unparse, parseL4Exp, Exp } from '../../src/L4/L4-ast';
import { Result, bind, isOkT, makeOk, isFailure } from '../../src/shared/result';
import { parse as parseSexp } from "../../src/shared/parser";

const p = (x: string): Result<Exp> => bind(parseSexp(x), parseL4Exp);

describe('L4 Parser', () => {
    it('parses atomic expressions', () => {
        expect(p("1")).to.satisfy(isOkT(isNumExp));
        expect(p("#t")).to.satisfy(isOkT(isBoolExp));
        expect(p("x")).to.satisfy(isOkT(isVarRef));
        expect(p('"a"')).to.satisfy(isOkT(isStrExp));
        expect(p(">")).to.satisfy(isOkT(isPrimOp));
        expect(p("=")).to.satisfy(isOkT(isPrimOp));
        expect(p("string=?")).to.satisfy(isOkT(isPrimOp));
        expect(p("eq?")).to.satisfy(isOkT(isPrimOp));
        expect(p("cons")).to.satisfy(isOkT(isPrimOp));
    });

    it('parses programs', () => {
        expect(parseL4("(L4 (define x 1) (> (+ x 1) (* x x)))")).to.satisfy(isOkT(isProgram));
    });

    it('parses "define" expressions', () => {
        const def = p("(define x 1)");
        expect(def).to.satisfy(isOkT(isDefineExp));
        if (isOkT(isDefineExp)(def)) {
            expect(def.value.var).to.satisfy(isVarDecl);
            expect(def.value.val).to.satisfy(isNumExp);
        }
    });

    it('parses applications', () => {
        expect(p("(> x 1)")).to.satisfy(isOkT(isAppExp));
        expect(p("(> (+ x x) (* x x))")).to.satisfy(isOkT(isAppExp));
    });

    it('parses "if" expressions', () => {
        expect(p("(if #t 1 2)")).to.satisfy(isOkT(isIfExp));
        expect(p("(if (< x 2) x 2)")).to.satisfy(isOkT(isIfExp));
    });

    it('parses procedures', () => {
        expect(p("(lambda () 1)")).to.satisfy(isOkT(isProcExp));
        expect(p("(lambda (x) x x)")).to.satisfy(isOkT(isProcExp));
    });

    it('parses "let" expressions', () => {
        expect(p("(let ((a 1) (b #t)) (if b a (+ a 1)))")).to.satisfy(isOkT(isLetExp));
    });

    it('parses literal expressions', () => {
        expect(p("'a")).to.satisfy(isOkT(isLitExp));
        expect(p("'()")).to.satisfy(isOkT(isLitExp));
        expect(p("'(1)")).to.satisfy(isOkT(isLitExp));
    });

    it('parses "letrec" expressions', () => {
        expect(p("(letrec ((e (lambda (x) x))) (e 2))")).to.satisfy(isOkT(isLetrecExp));
    });

    it('parses "set!" expressions', () => {
        expect(p("(set! x (+ 1 2))")).to.satisfy(isOkT(isSetExp));
    });

    describe("Failures", () => {
        it("returns a Failure when parsing a single-token program", () => {
            expect(parseL4("x")).to.satisfy(isFailure);
        });

        it("returns a Failure when parsing an empty program", () => {
            expect(parseL4("")).to.satisfy(isFailure);
        });

        it("returns a Failure if the program does not start with (L3 ...)", () => {
            expect(parseL4("(+ 1 2)")).to.satisfy(isFailure);
        });

        it("returns a Failure for a program with no Exps", () => {
            expect(parseL4("(L4)")).to.satisfy(isFailure);
        });
    
        it("returns a Failure if a program has an empty Exp", () => {
            expect(parseL4("(L4 ())")).to.satisfy(isFailure);
        });

        it('returns a Failure for an ill-formed "define"', () => {
            expect(p("(define)")).to.satisfy(isFailure);
            expect(p("(define x)")).to.satisfy(isFailure);
            expect(p("(define x y z)")).to.satisfy(isFailure);
            expect(p('(define "1" y)')).to.satisfy(isFailure);
            expect(p('(define 1 y)')).to.satisfy(isFailure);
        });

        it('returns a Failure for an ill-formed "set!"', () => {
            expect(p("(set!)")).to.satisfy(isFailure);
            expect(p("(set! x)")).to.satisfy(isFailure);
            expect(p("(set! x y z)")).to.satisfy(isFailure);
            expect(p('(set! "1" y)')).to.satisfy(isFailure);
            expect(p('(set! 1 y)')).to.satisfy(isFailure);
        });

        it("returns a Failure for an empty CExp", () => {
            expect(p("(+ ())")).to.satisfy(isFailure);
        });

        it("returns a Failure for an ill-formed special form", () => {
            expect(p("(if)")).to.satisfy(isFailure);
            expect(p("(if 1)")).to.satisfy(isFailure);
            expect(p("(lambda x x)")).to.satisfy(isFailure);
            expect(p("(let x x)")).to.satisfy(isFailure);
            expect(p("(let (x y) x)")).to.satisfy(isFailure);
            expect(p("(let ((1 y)) x)")).to.satisfy(isFailure);
        });
    });
});

describe('L4 Unparse', () => {
    const roundTrip = (x: string): Result<string> =>
        bind(p(x), (exp: Exp) => makeOk(unparse(exp)));

    it("doesn't change concrete values", () => {
        const concretes = ["1", "#t", "x", '"a"', ">", "=", "string=?", "eq?", "cons"];
        concretes.forEach(concrete => {
            expect(roundTrip(concrete)).to.deep.equal(makeOk(concrete));
        });
    });

    it('unparses programs', () => {
        const program = "(L4 (define x 1) (> (+ x 1) (* x x)))";
        expect(roundTrip(program)).to.deep.equal(makeOk(program));
    });

    it('unparses "define" expressions', () => {
        const define = "(define x 1)";
        expect(roundTrip(define)).to.deep.equal(makeOk(define));
    });

    it('unparses applications', () => {
        const app1 = "(> x 1)";
        const app2 = "(> (+ x x) (* x x))";
        expect(roundTrip(app1)).to.deep.equal(makeOk(app1));
        expect(roundTrip(app2)).to.deep.equal(makeOk(app2));
    });

    it('unparses "if" expressions', () => {
        const if1 = "(if #t 1 2)";
        const if2 = "(if (< x 2) x 2)";
        expect(roundTrip(if1)).to.deep.equal(makeOk(if1));
        expect(roundTrip(if2)).to.deep.equal(makeOk(if2));
    });

    it('unparses procedures', () => {
        const proc1 = "(lambda () 1)";
        const proc2 = "(lambda (x) x x)";
        expect(roundTrip(proc1)).to.deep.equal(makeOk(proc1));
        expect(roundTrip(proc2)).to.deep.equal(makeOk(proc2));
    });

    it('unparses "let" expressions', () => {
        const let1 = "(let ((a 1) (b #t)) (if b a (+ a 1)))";
        expect(roundTrip(let1)).to.deep.equal(makeOk(let1));
    });

    it('unparses literal expressions', () => {
        const lits = ["'a", "'()", "'(1)", "'(1 . 2)", "'(1 2 . 3)"];
        lits.forEach(lit => {
            expect(roundTrip(lit)).to.deep.equal(makeOk(lit));
        });
    });

    it('normalizes dotted pairs', () => {
        const dp1 = "'(1 . (2 . 3))";
        expect(roundTrip(dp1)).to.deep.equal(makeOk("'(1 2 . 3)"));
    });

    it('unparses "letrec" expressions', () => {
        const letrec1 = "(letrec ((f (lambda (x) x))) (f 2))";
        expect(roundTrip(letrec1)).to.deep.equal(makeOk(letrec1));
    });

    it('unparses "set!" expressions', () => {
        const set1 = "(set! x (+ 1 2))";
        expect(roundTrip(set1)).to.deep.equal(makeOk(set1));
    });
});