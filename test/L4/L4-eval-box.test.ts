import { expect } from 'chai';
import { makeVarDecl, makeVarRef } from '../../src/L4/L4-ast';
import { isEnv, makeExtEnv, applyEnv, theGlobalEnv, globalEnvAddBinding } from '../../src/L4/L4-env-box';
import { makeClosure, makeCompoundSExp, makeEmptySExp, makeSymbolSExp } from '../../src/L4/L4-value-box';
import { evalParse } from '../../src/L4/L4-eval-box';

describe('L4 Box Environment', () => {
    const env1 = makeExtEnv(["a", "b"], [1, 2], theGlobalEnv);
    const env2 = makeExtEnv(["a"], [3], env1);

    it('identifies the environment', () => {
        expect(isEnv(theGlobalEnv)).to.be.true;
        expect(isEnv(env1)).to.be.true;
    });

    it('applies the environment', () => {
        expect(applyEnv(env1, "a")).to.equal(1);
        expect(applyEnv(env2, "a")).to.equal(3);
        expect(applyEnv(env2, "b")).to.equal(2);
    });
});

describe('L4 Box Eval', () => {
    it('mutates the global environment', () => {
        globalEnvAddBinding("m", 1);
        expect(applyEnv(theGlobalEnv, "m")).to.equal(1);
    });

    it('evaluates data type literals', () => {
        expect(evalParse("1")).to.deep.equal(1);
        expect(evalParse("#t")).to.be.true;
        expect(evalParse("#f")).to.be.false;
        expect(evalParse("'a")).to.deep.equal(makeSymbolSExp("a"));
        expect(evalParse('"a"')).to.equal("a");
        expect(evalParse("'()")).to.deep.equal(makeEmptySExp());
        expect(evalParse("'(1 2)")).to.deep.equal(makeCompoundSExp(1, makeCompoundSExp(2, makeEmptySExp())));
        expect(evalParse("'(1 (2))")).to.deep.equal(makeCompoundSExp(1, makeCompoundSExp(makeCompoundSExp(2, makeEmptySExp()), makeEmptySExp())));
    });

    describe('Primitive Procedures', () => {
        it('evaluates "+"', () => {
            expect(evalParse("(+ 1 2)")).to.equal(3);
        });

        it('evaluates "-"', () => {
            expect(evalParse("(- 2 1)")).to.equal(1);
        });

        it('evaluates "*"', () => {
            expect(evalParse("(* 2 3)")).to.equal(6);
        });

        it('evaluates "/"', () => {
            expect(evalParse("(/ 4 2)")).to.equal(2);
        });

        it('evaluates "<"', () => {
            expect(evalParse("(< 4 2)")).to.be.false;
        });

        it('evaluates ">"', () => {
            expect(evalParse("(> 4 2)")).to.be.true;
        });

        it('evaluates "="', () => {
            expect(evalParse("(= 4 2)")).to.be.false;
        });

        it('evaluates "not"', () => {
            expect(evalParse("(not #t)")).to.be.false;
        });

        it('evaluates "eq?"', () => {
            expect(evalParse("(eq? 'a 'a)")).to.be.true;
        });

        it('evaluates "string=?"', () => {
            expect(evalParse('(string=? "a" "a")')).to.be.true;
        });

        it('evaluates "cons"', () => {
            expect(evalParse("(cons 1 '())")).to.deep.equal(makeCompoundSExp(1, makeEmptySExp()));
            expect(evalParse("(cons 1 '(2))")).to.deep.equal(makeCompoundSExp(1, makeCompoundSExp(2, makeEmptySExp())));
        });

        it('evaluates "car"', () => {
            expect(evalParse("(car '(1 2))")).to.equal(1);
        });

        it('evaluates "cdr"', () => {
            expect(evalParse("(cdr '(1 2))")).to.deep.equal(makeCompoundSExp(2, makeEmptySExp()));
            expect(evalParse("(cdr '(1))")).to.deep.equal(makeEmptySExp());
        });

        it('evaluates "list?"', () => {
            expect(evalParse("(list? '(1))")).to.be.true;
            expect(evalParse("(list? '())")).to.be.true;
        });

        it('evaluates "number?"', () => {
            expect(evalParse("(number? 1)")).to.be.true;
            expect(evalParse("(number? #t)")).to.be.false;
        });

        it('evaluates "boolean?"', () => {
            expect(evalParse("(boolean? #t)")).to.be.true;
            expect(evalParse("(boolean? 0)")).to.be.false;
        });

        it('evaluates "symbol?"', () => {
            expect(evalParse("(symbol? 'a)")).to.be.true;
            expect(evalParse('(symbol? "a")')).to.be.false;
        });

        it('evaluates "string?"', () => {
            expect(evalParse("(string? 'a)")).to.be.false;
            expect(evalParse('(string? "a")')).to.be.true;
        });

        it('evaluates "pair?"', () => {
            expect(evalParse('(pair? "a")')).to.be.false;
            expect(evalParse('(pair? (cons 1 2))')).to.be.true;
            expect(evalParse("(pair? '(1))")).to.be.true;
        });
    });

    it('evaluates "define" expressions', () => {
        expect(evalParse("(L4 (define x 1) (+ x x))")).to.equal(2);
        expect(evalParse("(L4 (define x 1) (define y (+ x x)) (* y y))")).to.equal(4);
    });

    it('evaluates "if" expressions', () => {
        expect(evalParse('(if (string? "a") 1 2)')).to.equal(1);
        expect(evalParse('(if (not (string? "a")) 1 2)')).to.equal(2);
    });

    it('evaluates procedures', () => {
        expect(evalParse("(lambda (x) x)")).to.deep.equal(makeClosure([makeVarDecl("x")], [makeVarRef("x")], theGlobalEnv));
    });

    it('evaluates "letrec" expressions', () => {
        const letrec = "(letrec ((a 1) (b #t)) (if b a (+ a 1)))";
        expect(evalParse(letrec)).to.equal(1);
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).to.equal(4);
        expect(evalParse("(L4 (define square (lambda (x) (* x x))) (square 3))")).to.equal(9);
        expect(evalParse("(L4 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))")).to.equal(3);
    });

    it('evaluates recursive procedures without "letrec"', () => {
        expect(evalParse("(L4 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))")).to.equal(6);
    });

    it('evaluates recursive procedures with "letrec"', () => {
        expect(evalParse(`(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`)).to.equal(120);
    });

    it('evaluates the examples', () => {
        // Preserve bound variables
        expect(evalParse(`
            (L4 (define fact
                  (letrec ((f (lambda (n)
                                (if (= n 0)
                                    1
                                    (* n (f (- n 1)))))))
                    f))
                (fact 5))
        `)).to.equal(120);

        // Accidental capture of the z variable if no renaming - works without renaming in env eval.
        expect(evalParse(`
            (L4 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`)).to.equal(4);

        // Y-combinator
        expect(evalParse(`
            (L4 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1))))))) 6))`)).to.equal(720);
    });

    it('evaluates higher-order functions', () => {
        expect(evalParse(`
            (L4 (define map
                  (lambda (f l)
                    (if (eq? l '())
                        l
                        (cons (f (car l)) (map f (cdr l))))))
                (map (lambda (x) (* x x)) '(1 2 3)))`)).to.deep.equal(evalParse("'(1 4 9)"));

        expect(evalParse(`
            (L4 (define empty? (lambda (x) (eq? x '())))
                (define filter (lambda (pred l)
                                 (if (empty? l)
                                     l
                                     (if (pred (car l))
                                         (cons (car l) (filter pred (cdr l)))
                                         (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`)).to.deep.equal(evalParse("'(1 3)"))

        expect(evalParse(`
            (L4 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`)).to.be.false;
    });

    it('properly captures variables in closures', () => {
        expect(evalParse(`
            (L4 (define makeAdder (lambda (n) (lambda (y) (+ y n))))
                (define a6 (makeAdder 6))
                (define a7 (makeAdder 7))
                (+ (a6 1) (a7 1)))`)).to.equal(15);

        expect(evalParse(`
            (L4 (define makeCounter (lambda () (let ((c 0)) (lambda () (set! c (+ c 1)) c))))
                (define c1 (makeCounter))
                (define c2 (makeCounter))
                (+ (+ (c1) (c1)) (+ (c2) (c2))))`)).to.equal(6);
    });

    it('evaluates mutual recursion', () => {
        expect(evalParse(`
            (L4 (define odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                (define even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                (and (odd? 5) (even? 6)))`)).to.be.true;
    });
});
