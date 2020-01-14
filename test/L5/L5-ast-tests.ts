import { expect } from "chai";
import { isNumExp, isBoolExp, isVarRef, isPrimOp, isProgram, isDefineExp, isVarDecl,
         isAppExp, isStrExp, isIfExp, isProcExp, isLetExp, isLitExp, isLetrecExp, isSetExp,
         parse, unparse } from "../../src/L5/L5-ast";

describe('L5 Parser', () => {
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
        expect(parse("(L5 (define x 1) (> (+ x 1) (* x x)))")).to.satisfy(isProgram)
    });

    it('parses "define" expressions', () => {
        const def = parse("(define x 1)");
        expect(def).to.satisfy(isDefineExp);
        if (isDefineExp(def)) {
            expect(def.var).to.satisfy(isVarDecl);
            expect(def.val).to.satisfy(isNumExp);
        }
    });

    it('parses "define" expressions with type annotations', () => {
        const define = "(define (a : number) 1)";
        expect(parse(define)).to.satisfy(isDefineExp);
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

    it('parses procedures with type annotations', () => {
        expect(parse("(lambda ((x : number)) : number (* x x))")).to.satisfy(isProcExp);
    });

    it('parses "let" expressions', () => {
        expect(parse("(let ((a 1) (b #t)) (if b a (+ a 1)))")).to.satisfy(isLetExp);
    });

    it('parses "let" expressions with type annotations', () => {
        expect(parse("(let (((a : boolean) #t) ((b : number) 2)) (if a b (+ b b)))")).to.satisfy(isLetExp);
    });

    it('parses literal expressions', () => {
        expect(parse("'a")).to.satisfy(isLitExp);
        expect(parse("'()")).to.satisfy(isLitExp);
        expect(parse("'(1)")).to.satisfy(isLitExp);
    });

    it('parses "letrec" expressions', () => {
        expect(parse("(letrec ((e (lambda (x) x))) (e 2))")).to.satisfy(isLetrecExp);
    });

    it('parses "letrec" expressions with type annotations', () => {
        expect(parse("(letrec (((p : (number * number -> number)) (lambda ((x : number) (y : number)) (+ x y)))) (p 1 2))")).to.satisfy(isLetrecExp);
    });

    it('parses "set!" expressions', () => {
        expect(parse("(set! x 1)")).to.satisfy(isSetExp);
    });
});

describe('L5 Unparse', () => {
    const roundTrip: (x: string) => string | Error = x => unparse(parse(x));

    it('unparses "define" expressions with type annotations', () => {
        const define = "(define (a : number) 1)";
        expect(roundTrip(define)).to.equal(define);
    });

    it('unparses procedures with type annotations', () => {
        const lambda = "(lambda ((x : number)) : number (* x x))";
        expect(roundTrip(lambda)).to.equal(lambda);
    });

    it('unparses "let" expressions with type annotations', () => {
        const let1 = "(let (((a : boolean) #t) ((b : number) 2)) (if a b (+ b b)))";
        expect(roundTrip(let1)).to.equal(let1);
    });

    it('unparses "letrec" expressions', () => {
        const letrec = "(letrec (((p : (number * number -> number)) (lambda ((x : number) (y : number)) (+ x y)))) (p 1 2))";
        expect(roundTrip(letrec)).to.equal(letrec);
    });
});
