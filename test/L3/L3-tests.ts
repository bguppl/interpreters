import { expect } from 'chai';
import { makeNumExp, parseL3, unparseL3 } from '../../src/L3/L3-ast';
import { makeVarDecl, makeVarRef } from '../../src/L3/L3-ast';
import { isAppExp, isBoolExp, isCExp, isDefineExp, isIfExp, isLetExp, isLitExp, isNumExp, isPrimOp,
         isProcExp, isProgram, isStrExp, isVarDecl, isVarRef } from '../../src/L3/L3-ast';
import { evalParse, renameExps, substitute, listPrim  } from '../../src/L3/L3-eval';
import { makeClosure, makeCompoundSExp, makeEmptySExp, makeSymbolSExp } from '../../src/L3/L3-value';
import { isError } from '../../src/shared/error';
import { isEnv, makeEnv, makeEmptyEnv, applyEnv } from '../../src/L3/L3-env';


describe('L3 Environment', () => {
    it('applies the environment correctly', () => {
        const emptyEnv = makeEmptyEnv();
        const env1 = makeEnv("a", 1, emptyEnv);
        const env2 = makeEnv("b", 2, env1);
        const env3 = makeEnv("a", 3, env2);
    
        expect(isEnv(emptyEnv)).to.be.true;
        expect(isEnv(env1)).to.be.true;
        expect(applyEnv(env1, "a")).to.equal(1);
        expect(applyEnv(env2, "a")).to.equal(1);
        expect(applyEnv(env2, "b")).to.equal(2);
        expect(applyEnv(env3, "a")).to.equal(3);
        expect(applyEnv(env3, "b")).to.equal(2);
    });
});

describe('L3 Parsing', () => {
    it('parses atomic expressions', () => {
        expect(parseL3("1")).to.satisfy(isNumExp);
        expect(parseL3("#t")).to.satisfy(isBoolExp);
        expect(parseL3("x")).to.satisfy(isVarRef);
        expect(parseL3('"a"')).to.satisfy(isStrExp);
        expect(parseL3(">")).to.satisfy(isPrimOp);
        expect(parseL3("=")).to.satisfy(isPrimOp);
        expect(parseL3("string=?")).to.satisfy(isPrimOp);
        expect(parseL3("eq?")).to.satisfy(isPrimOp);
        expect(parseL3("cons")).to.satisfy(isPrimOp);
    });

    it('parses programs', () => {
        expect(parseL3("(L3 (define x 1) (> (+ x 1) (* x x)))")).to.satisfy(isProgram);
    });

    it('parses define expressions', () => {
        const parsed = parseL3("(define x 1)");
        expect(parsed).to.satisfy(isDefineExp);
        if (isDefineExp(parsed)) {
            expect(parsed.var).to.satisfy(isVarDecl);
            expect(parsed.val).to.satisfy(isNumExp);
        }
    });

    it('parses applications', () => {
        expect(parseL3("(> x 1)")).to.satisfy(isAppExp);
        expect(parseL3("(> (+ x x) (* x x))")).to.satisfy(isAppExp);
    });

    it('parses "if" expressions', () => {
        expect(parseL3("(if #t 1 2)")).to.satisfy(isIfExp);
        expect(parseL3("(if (< x 2) x 2)")).to.satisfy(isIfExp);
    });

    it('parses procedures', () => {
        expect(parseL3("(lambda () 1)")).to.satisfy(isProcExp);
        expect(parseL3("(lambda (x) x x)")).to.satisfy(isProcExp);
    });

    it('parses "let" expressions', () => {
        expect(parseL3("(let ((a 1) (b #t)) (if b a (+ a 1)))")).to.satisfy(isLetExp);
    });

    it('parses literal expressions', () => {
        expect(parseL3("'a")).to.satisfy(isLitExp);
        expect(parseL3("'()")).to.satisfy(isLitExp);
        expect(parseL3("'(1)")).to.satisfy(isLitExp);
        expect(parseL3("'(1 . 2)")).to.satisfy(isLitExp);
        expect(parseL3("'(1 . (2 . 3))")).to.satisfy(isLitExp);
    });

    it('returns an error for an invalid literal', () => {
        expect(parseL3("'(1 . 2 3)")).to.satisfy(isError);
    });
});

describe('L3 Unparse', () => {
    const roundTrip: (x: string) => string = x => unparseL3(parseL3(x));

    it('doesn\'t change concrete values', () => {
        const concretes = ["1", "#t", "x", '"a"', ">", "=", "string=?", "eq?", "cons"];
        concretes.forEach(concrete => {
            expect(roundTrip(concrete)).to.equal(concrete);
        });
    });

    it('unparses a program', () => {
        const program = "(L3 (define x 1) (> (+ x 1) (* x x)))"
        expect(roundTrip(program)).to.equal(program);
    });

    it('unparses "define" expressions', () => {
        const define = "(define x 1)"
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
        expect(roundTrip("'(1 . (2 . 3))")).to.equal("'(1 2 . 3)");
    });
});

describe('L3 Eval', () => {
    it('evaluates data type literals', () => {
        expect(evalParse("1")).to.equal(1);
        expect(evalParse("#t")).to.be.true;
        expect(evalParse("#f")).to.be.false;
        expect(evalParse("'a")).to.deep.equal(makeSymbolSExp("a"));
        expect(evalParse('"a"')).to.equal("a");
        expect(evalParse("'()")).to.deep.equal(makeEmptySExp());
        expect(evalParse("'(1 2)")).to.deep.equal(listPrim([1, 2]));
        expect(evalParse("'(1 (2))")).to.deep.equal(listPrim([1, listPrim([2])]));
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
            expect(evalParse("(cons 1 '())")).to.deep.equal(listPrim([1]));
            expect(evalParse("(cons 1 '(2))")).to.deep.equal(listPrim([1, 2]));
        });

        it('evaluates "car"', () => {
            expect(evalParse("(car '(1 2))")).to.equal(1);
        });

        it('evaluates "cdr"', () => {
            expect(evalParse("(cdr '(1 2))")).to.deep.equal(listPrim([2]));
            expect(evalParse("(cdr '(1))")).to.deep.equal(makeEmptySExp());
        });

        it('evaluates "pair?"', () => {
            expect(evalParse("(pair? '(1))")).to.be.true;
            expect(evalParse("(pair? '())")).to.be.false;
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
    });

    it('evaluates dotted pairs', () => {
        expect(evalParse("(cons 1 2)")).to.deep.equal(makeCompoundSExp(1, 2));
        expect(evalParse("(car (cons 1 2))")).to.equal(1);
        expect(evalParse("(cdr (cons 1  2))")).to.equal(2);
        expect(evalParse("(pair? (cons 1  2))")).to.be.true;
        expect(evalParse("(car '(1 . 2))")).to.equal(1);
        expect(evalParse("(cdr '(1 . 2))")).to.equal(2);
        expect(evalParse("(car (cdr '(1 . (2 . 3))))")).to.equal(2);
        expect(evalParse("(cdr (cdr '(1 . (2 . 3))))")).to.equal(3);
        expect(evalParse("(car (cdr (list 1 2 3 4)))")).to.equal(2);
    });

    it('evaluates programs with "define"', () => {
        expect(evalParse("(L3 (define x 1) (+ x x))")).to.equal(2);
        expect(evalParse("(L3 (define x 1) (define y (+ x x)) (* y y))")).to.equal(4);
    });

    it('evaluates "if"', () => {
        expect(evalParse('(if (string? "a") 1 2)')).to.equal(1);
        expect(evalParse('(if (not (string? "a")) 1 2)')).to.equal(2);
    });

    it('evaluates procedures', () => {
        expect(evalParse("(lambda (x) x)")).to.deep.equal(makeClosure([makeVarDecl("x")], [makeVarRef("x")]));
    });

    it('substitutes', () => {
        const cexp1 = parseL3("((lambda (x) (* x x)) x)");
        const expected1 = parseL3("((lambda (x) (* x x)) 3)");
        if (isCExp(cexp1)) {
            expect(substitute([cexp1], ["x"], [makeNumExp(3)])).to.deep.equal([expected1]);
        }

        const e1 = `
            ((if (= n 0)
                 (lambda (x) x)
                 (if (= n 1)
                     f
                     (lambda (x) (f ((nf f (- n 1)) x)))))
             '(f n))`;
        const vn = parseL3("2");
        const vf = parseL3("(lambda (x) (* x x))");
        const e2 = `
            ((if (= 2 0)
                 (lambda (x) x)
                 (if (= 2 1)
                     (lambda (x) (* x x))
                     (lambda (x) ((lambda (x) (* x x)) ((nf (lambda (x) (* x x)) (- 2 1)) x)))))
             '(f n))`;
        const parsed1 = parseL3(e1);
        const parsed2 = parseL3(e2);
        if (isCExp(parsed1) && isCExp(parsed2) && isCExp(vn) && isCExp(vf)) {
            expect(substitute([parsed1], ["n", "f"], [vn, vf])).to.deep.equal([parsed2]);
        }

        const lzxz = parseL3("(lambda (z) (x z))");
        const lwzw = parseL3("(lambda (w) (z w))");
        const lzlwzwz = parseL3("(lambda (z) ((lambda (w) (z w)) z))");
        if (isCExp(lzxz) && isCExp(lwzw)) {
            expect(substitute([lzxz], ["x"], [lwzw])).to.deep.equal([lzlwzwz]);
        }
    });

    it('renames', () => {
        const lxx = parseL3("(lambda (x) x)");
        const lx1x1 = parseL3("(lambda (x__1) x__1)");
        if (isCExp(lxx) && isCExp(lx1x1)) {
            expect(renameExps([lxx])).to.deep.equal([lx1x1]);
        }
        const l1 = parseL3(`(((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2)`);
        const rl1 = parseL3(`(((lambda (x__1) (lambda (z__2) (x__1 z__2))) (lambda (w__3) (z w__3))) 2)`);
        if (isCExp(l1) && isCExp(rl1)) {
            expect(renameExps([l1])).to.deep.equal([rl1]);
        }
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).to.equal(4);
        expect(evalParse("(L3 (define square (lambda (x) (* x x))) (square 3))")).to.equal(9);
        expect(evalParse("(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))")).to.equal(3);
    });

    it('applies recursive procedures', () => {
        expect(evalParse("(L3 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))")).to.equal(6);
    });

    it('applies higher-order functions', () => {
        // L3 higher order functions
        expect(evalParse(`
            (L3 (define map
                  (lambda (f l)
                    (if (eq? l '())
                        l
                        (cons (f (car l)) (map f (cdr l))))))
                (map (lambda (x) (* x x)) '(1 2 3)))`)).to.deep.equal(listPrim([1, 4, 9]));

        expect(evalParse(`
            (L3 (define empty? (lambda (x) (eq? x '())))
                (define filter
                  (lambda (pred l)
                    (if (empty? l)
                        l
                        (if (pred (car l))
                            (cons (car l) (filter pred (cdr l)))
                            (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`)).to.deep.equal(listPrim([1, 3]));

        expect(evalParse(`
            (L3 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`)).to.be.false;

        expect(evalParse(`
            (L3 (define equal? (lambda (e1 e2)
                                 (if (eq? e1 e2)
                                     #t
                                     (if (and (pair? e1) (pair? e2))
                                         (and (equal? (car e1) (car e2)) (equal? (cdr e1) (cdr e2)))
                                         #f))))
                (and (equal? '(1 . (2 . 3)) '(1 2 . 3)) (equal? '(1 . (2)) '(1 2))))`)).to.be.true;
    });

    it('evaluates the examples', () => {
        // Preserve bound variables in subst
        expect(evalParse(`
            (L3 (define nf
                  (lambda (f n)
                    (if (= n 0)
                        (lambda (x) x)
                        (if (= n 1)
                            f
                            (lambda (x) (f ((nf f (- n 1)) x)))))))
                ((nf (lambda (x) (* x x)) 2) 3))`)).to.equal(81);

        // Accidental capture of the z variable if no renaming
        expect(evalParse(`
            (L3 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`)).to.equal(4);

        // Y-combinator
        expect(evalParse(`
            (L3 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1))))))) 6))`)).to.equal(720);
    });
});
