import { expect } from 'chai';
import { isBoolExp, isNumExp, isPrimOp, isStrExp, isVarDecl, isVarRef, isSetExp,
    isAppExp, isDefineExp, isIfExp, isLetrecExp, isLetExp, isLitExp, isProcExp, isProgram,
    parse, unparse } from '../../src/L4/L4-ast';

describe('L4 Parser', () => {
    it('parses atomic expressions', () => {
        expect(parse("1")).to.satisfy(isNumExp);
        expect(parse("#t")).to.satisfy(isBoolExp);
        expect(parse("x")).to.satisfy(isVarRef);
        expect(parse('"a"')).to.satisfy(isStrExp);
        expect(parse(">")).to.satisfy(isPrimOp);
        expect(parse("=")).to.satisfy(isPrimOp);
        expect(parse("string=?")).to.satisfy(isPrimOp);
        expect(parse("eq?")).to.satisfy(isPrimOp);
        expect(parse("cons")).to.satisfy(isPrimOp);
    });

    it('parses programs', () => {
        expect(parse("(L4 (define x 1) (> (+ x 1) (* x x)))")).to.satisfy(isProgram);
    });

    it('parses "define" expressions', () => {
        const def = parse("(define x 1)");
        expect(def).to.satisfy(isDefineExp);
        if (isDefineExp(def)) {
            expect(def.var).to.satisfy(isVarDecl);
            expect(def.val).to.satisfy(isNumExp);
        }
    });

    it('parses applications', () => {
        expect(parse("(> x 1)")).to.satisfy(isAppExp);
        expect(parse("(> (+ x x) (* x x))")).to.satisfy(isAppExp);
    });

    it('parses "if" expressions', () => {
        expect(parse("(if #t 1 2)")).to.satisfy(isIfExp);
        expect(parse("(if (< x 2) x 2)")).to.satisfy(isIfExp);
    });

    it('parses procedures', () => {
        expect(parse("(lambda () 1)")).to.satisfy(isProcExp);
        expect(parse("(lambda (x) x x)")).to.satisfy(isProcExp);
    });

    it('parses "let" expressions', () => {
        expect(parse("(let ((a 1) (b #t)) (if b a (+ a 1)))")).to.satisfy(isLetExp);
    });

    it('parses literal expressions', () => {
        expect(parse("'a")).to.satisfy(isLitExp);
        expect(parse("'()")).to.satisfy(isLitExp);
        expect(parse("'(1)")).to.satisfy(isLitExp);
    });

    it('parses "letrec" expressions', () => {
        expect(parse("(letrec ((e (lambda (x) x))) (e 2))")).to.satisfy(isLetrecExp);
    });

    it('parses "set!" expressions', () => {
        expect(parse("(set! x (+ 1 2))")).to.satisfy(isSetExp);
    });
});

describe('L4 Unparse', () => {
    const roundTrip: (x: string) => string = x => unparse(parse(x));

    it('doesn\'t change concrete values', () => {
        const concretes = ["1", "#t", "x", '"a"', ">", "=", "string=?", "eq?", "cons"];
        concretes.forEach(concrete => {
            expect(roundTrip(concrete)).to.equal(concrete);
        });
    });

    it('unparses programs', () => {
        const program = "(L4 (define x 1) (> (+ x 1) (* x x)))";
        expect(roundTrip(program)).to.equal(program);
    });

    it('unparses "define" expressions', () => {
        const define = "(define x 1)";
        expect(roundTrip(define)).to.equal(define);
    });

    it('unparses applications', () => {
        const app1 = "(> x 1)";
        const app2 = "(> (+ x x) (* x x))";
        expect(roundTrip(app1)).to.equal(app1);
        expect(roundTrip(app2)).to.equal(app2);
    });

    it('unparses "if" expressions', () => {
        const if1 = "(if #t 1 2)";
        const if2 = "(if (< x 2) x 2)";
        expect(roundTrip(if1)).to.equal(if1);
        expect(roundTrip(if2)).to.equal(if2);
    });

    it('unparses procedures', () => {
        const proc1 = "(lambda () 1)";
        const proc2 = "(lambda (x) x x)";
        expect(roundTrip(proc1)).to.equal(proc1);
        expect(roundTrip(proc2)).to.equal(proc2);
    });

    it('unparses "let" expressions', () => {
        const let1 = "(let ((a 1) (b #t)) (if b a (+ a 1)))";
        expect(roundTrip(let1)).to.equal(let1);
    });

    it('unparses literal expressions', () => {
        const lits = ["'a", "'()", "'(1)", "'(1 . 2)", "'(1 2 . 3)"];
        lits.forEach(lit => {
            expect(roundTrip(lit)).to.equal(lit);
        });
    });

    it('normalizes dotted pairs', () => {
        const dp1 = "'(1 . (2 . 3))";
        expect(roundTrip(dp1)).to.equal("'(1 2 . 3)");
    });

    it('unparses "letrec" expressions', () => {
        const letrec1 = "(letrec ((f (lambda (x) x))) (f 2))";
        expect(roundTrip(letrec1)).to.equal(letrec1);
    });

    it('unparses "set!" expressions', () => {
        const set1 = "(set! x (+ 1 2))";
        expect(roundTrip(set1)).to.equal(set1);
    });
});