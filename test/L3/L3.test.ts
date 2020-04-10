import p from "s-expression";
import { expect } from 'chai';
import { makeNumExp, parseL3Exp, unparseL3, parseL3, Exp } from '../../src/L3/L3-ast';
import { makeVarDecl, makeVarRef } from '../../src/L3/L3-ast';
import { isAppExp, isBoolExp, isCExp, isDefineExp, isIfExp, isLetExp, isLitExp, isNumExp, isPrimOp,
         isProcExp, isProgram, isStrExp, isVarDecl, isVarRef } from '../../src/L3/L3-ast';
import { renameExps, substitute, listPrim, evalParse, evalL3program } from '../../src/L3/L3-eval';
import { makeClosure, makeCompoundSExp, makeEmptySExp, makeSymbolSExp } from '../../src/L3/L3-value';
import { isEnv, makeEnv, makeEmptyEnv, applyEnv } from '../../src/L3/L3-env';
import { isOk, Result, isFailure, bind, makeOk } from "../../src/shared/result";

const isOkT = <T>(pred: (x: T) => boolean): (r: Result<T>) => boolean =>
    (r: Result<T>) => isOk(r) && pred(r.value);

describe('L3 Environment', () => {
    it('applies the environment correctly', () => {
        const emptyEnv = makeEmptyEnv();
        const env1 = makeEnv("a", 1, emptyEnv);
        const env2 = makeEnv("b", 2, env1);
        const env3 = makeEnv("a", 3, env2);
    
        expect(isEnv(emptyEnv)).to.be.true;
        expect(isEnv(env1)).to.be.true;
        expect(applyEnv(env1, "a")).to.deep.equal(makeOk(1));
        expect(applyEnv(env2, "a")).to.deep.equal(makeOk(1));
        expect(applyEnv(env2, "b")).to.deep.equal(makeOk(2));
        expect(applyEnv(env3, "a")).to.deep.equal(makeOk(3));
        expect(applyEnv(env3, "b")).to.deep.equal(makeOk(2));
    });
});

describe('L3 Parsing', () => {
    it('parses atomic expressions', () => {
        expect(parseL3Exp(p("1"))).to.satisfy(isOkT(isNumExp));
        expect(parseL3Exp(p("#t"))).to.satisfy(isOkT(isBoolExp));
        expect(parseL3Exp(p("x"))).to.satisfy(isOkT(isVarRef));
        expect(parseL3Exp(p('"a"'))).to.satisfy(isOkT(isStrExp));
        expect(parseL3Exp(p(">"))).to.satisfy(isOkT(isPrimOp));
        expect(parseL3Exp(p("="))).to.satisfy(isOkT(isPrimOp));
        expect(parseL3Exp(p("string=?"))).to.satisfy(isOkT(isPrimOp));
        expect(parseL3Exp(p("eq?"))).to.satisfy(isOkT(isPrimOp));
        expect(parseL3Exp(p("cons"))).to.satisfy(isOkT(isPrimOp));
    });

    it('parses programs', () => {
        expect(parseL3("(L3 (define x 1) (> (+ x 1) (* x x)))")).to.satisfy(isOkT(isProgram));
    });

    it('parses define expressions', () => {
        const parsed = parseL3Exp(p("(define x 1)"));
        expect(parsed).to.satisfy(isOkT(isDefineExp));
        if (isOk(parsed) && isDefineExp(parsed.value)) {
            expect(parsed.value.var).to.satisfy(isVarDecl);
            expect(parsed.value.val).to.satisfy(isNumExp);
        }
    });

    it('parses applications', () => {
        expect(parseL3Exp(p("(> x 1)"))).to.satisfy(isOkT(isAppExp));
        expect(parseL3Exp(p("(> (+ x x) (* x x))"))).to.satisfy(isOkT(isAppExp));
    });

    it('parses "if" expressions', () => {
        expect(parseL3Exp(p("(if #t 1 2)"))).to.satisfy(isOkT(isIfExp));
        expect(parseL3Exp(p("(if (< x 2) x 2)"))).to.satisfy(isOkT(isIfExp));
    });

    it('parses procedures', () => {
        expect(parseL3Exp(p("(lambda () 1)"))).to.satisfy(isOkT(isProcExp));
        expect(parseL3Exp(p("(lambda (x) x x)"))).to.satisfy(isOkT(isProcExp));
    });

    it('parses "let" expressions', () => {
        expect(parseL3Exp(p("(let ((a 1) (b #t)) (if b a (+ a 1)))"))).to.satisfy(isOkT(isLetExp));
    });

    it('parses literal expressions', () => {
        expect(parseL3Exp(p("'a"))).to.satisfy(isOkT(isLitExp));
        expect(parseL3Exp(p("'()"))).to.satisfy(isOkT(isLitExp));
        expect(parseL3Exp(p("'(1)"))).to.satisfy(isOkT(isLitExp));
        expect(parseL3Exp(p("'(1 . 2)"))).to.satisfy(isOkT(isLitExp));
        expect(parseL3Exp(p("'(1 . (2 . 3))"))).to.satisfy(isOkT(isLitExp));
    });

    it('returns an error for an invalid literal', () => {
        expect(parseL3Exp(p("'(1 . 2 3)"))).to.satisfy(isFailure);
    });
});

describe('L3 Unparse', () => {
    const roundTrip: (x: string) => Result<string> =
        x => bind(parseL3Exp(p(x)), (exp: Exp) => makeOk(unparseL3(exp)));

    it("doesn't change concrete values", () => {
        const concretes = ["1", "#t", "x", '"a"', ">", "=", "string=?", "eq?", "cons"];
        concretes.forEach(concrete => {
            expect(roundTrip(concrete)).to.deep.equal(makeOk(concrete));
        });
    });

    it('unparses a program', () => {
        const program = "(L3 (define x 1) (> (+ x 1) (* x x)))";
        const result = bind(parseL3(program), program => makeOk(unparseL3(program)));
        expect(result).to.deep.equal(makeOk(program));
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
        expect(roundTrip("'(1 . (2 . 3))")).to.deep.equal(makeOk("'(1 2 . 3)"));
    });
});

describe('L3 Eval', () => {
    it('evaluates data type literals', () => {
        expect(evalParse("1")).to.deep.equal(makeOk(1));
        expect(evalParse("#t")).to.deep.equal(makeOk(true));
        expect(evalParse("#f")).to.deep.equal(makeOk(false));
        expect(evalParse("'a")).to.deep.equal(makeOk(makeSymbolSExp("a")));
        expect(evalParse('"a"')).to.deep.equal(makeOk("a"));
        expect(evalParse("'()")).to.deep.equal(makeOk(makeEmptySExp()));
        expect(evalParse("'(1 2)")).to.deep.equal(makeOk(listPrim([1, 2])));
        expect(evalParse("'(1 (2))")).to.deep.equal(makeOk(listPrim([1, listPrim([2])])));
    });

    describe('Primitive Procedures', () => {
        it('evaluates "+"', () => {
            expect(evalParse("(+ 1 2)")).to.deep.equal(makeOk(3));
        });

        it('evaluates "-"', () => {
            expect(evalParse("(- 2 1)")).to.deep.equal(makeOk(1));
        });

        it('evaluates "*"', () => {
            expect(evalParse("(* 2 3)")).to.deep.equal(makeOk(6));
        });

        it('evaluates "/"', () => {
            expect(evalParse("(/ 4 2)")).to.deep.equal(makeOk(2));
        });

        it('evaluates "<"', () => {
            expect(evalParse("(< 4 2)")).to.deep.equal(makeOk(false));
        });

        it('evaluates ">"', () => {
            expect(evalParse("(> 4 2)")).to.deep.equal(makeOk(true));
        });

        it('evaluates "="', () => {
            expect(evalParse("(= 4 2)")).to.deep.equal(makeOk(false));
        });

        it('evaluates "not"', () => {
            expect(evalParse("(not #t)")).to.deep.equal(makeOk(false));
        });

        it('evaluates "eq?"', () => {
            expect(evalParse("(eq? 'a 'a)")).to.deep.equal(makeOk(true));
        });

        it('evaluates "string=?"', () => {
            expect(evalParse('(string=? "a" "a")')).to.deep.equal(makeOk(true));
        });

        it('evaluates "cons"', () => {
            expect(evalParse("(cons 1 '())")).to.deep.equal(makeOk(listPrim([1])));
            expect(evalParse("(cons 1 '(2))")).to.deep.equal(makeOk(listPrim([1, 2])));
        });

        it('evaluates "car"', () => {
            expect(evalParse("(car '(1 2))")).to.deep.equal(makeOk(1));
        });

        it('evaluates "cdr"', () => {
            expect(evalParse("(cdr '(1 2))")).to.deep.equal(makeOk(listPrim([2])));
            expect(evalParse("(cdr '(1))")).to.deep.equal(makeOk(makeEmptySExp()));
        });

        it('evaluates "pair?"', () => {
            expect(evalParse("(pair? '(1))")).to.deep.equal(makeOk(true));
            expect(evalParse("(pair? '())")).to.deep.equal(makeOk(false));
        });

        it('evaluates "number?"', () => {
            expect(evalParse("(number? 1)")).to.deep.equal(makeOk(true));
            expect(evalParse("(number? #t)")).to.deep.equal(makeOk(false));
        });

        it('evaluates "boolean?"', () => {
            expect(evalParse("(boolean? #t)")).to.deep.equal(makeOk(true));
            expect(evalParse("(boolean? 0)")).to.deep.equal(makeOk(false));
        });

        it('evaluates "symbol?"', () => {
            expect(evalParse("(symbol? 'a)")).to.deep.equal(makeOk(true));
            expect(evalParse('(symbol? "a")')).to.deep.equal(makeOk(false));
        });

        it('evaluates "string?"', () => {
            expect(evalParse('(string? "a")')).to.deep.equal(makeOk(true));
            expect(evalParse("(string? 'a)")).to.deep.equal(makeOk(false));
        });
    });

    it('evaluates dotted pairs', () => {
        expect(evalParse("(cons 1 2)")).to.deep.equal(makeOk(makeCompoundSExp(1, 2)));
        expect(evalParse("(car (cons 1 2))")).to.deep.equal(makeOk(1));
        expect(evalParse("(cdr (cons 1  2))")).to.deep.equal(makeOk(2));
        expect(evalParse("(pair? (cons 1  2))")).to.deep.equal(makeOk(true));
        expect(evalParse("(car '(1 . 2))")).to.deep.equal(makeOk(1));
        expect(evalParse("(cdr '(1 . 2))")).to.deep.equal(makeOk(2));
        expect(evalParse("(car (cdr '(1 . (2 . 3))))")).to.deep.equal(makeOk(2));
        expect(evalParse("(cdr (cdr '(1 . (2 . 3))))")).to.deep.equal(makeOk(3));
        expect(evalParse("(car (cdr (list 1 2 3 4)))")).to.deep.equal(makeOk(2));
    });

    it('evaluates programs with "define"', () => {
        const program1 = parseL3("(L3 (define x 1) (+ x x))");
        if (isOk(program1)) {
            expect(evalL3program(program1.value)).to.deep.equal(makeOk(2));
        }

        const program2 = parseL3("(L3 (define x 1) (define y (+ x x)) (* y y))");
        if (isOk(program2)) {
            expect(evalL3program(program2.value)).to.deep.equal(makeOk(4));
        }
    });

    it('evaluates "if"', () => {
        expect(evalParse('(if (string? "a") 1 2)')).to.deep.equal(makeOk(1));
        expect(evalParse('(if (not (string? "a")) 1 2)')).to.deep.equal(makeOk(2));
    });

    it('evaluates procedures', () => {
        expect(evalParse("(lambda (x) x)")).to.deep.equal(makeOk(makeClosure([makeVarDecl("x")], [makeVarRef("x")])));
    });

    it('substitutes', () => {
        const cexp1 = parseL3Exp(p("((lambda (x) (* x x)) x)"));
        const expected1 = parseL3Exp(p("((lambda (x) (* x x)) 3)"));
        if (isOk(cexp1) && isCExp(cexp1.value) && isOk(expected1)) {
            expect(substitute([cexp1.value], ["x"], [makeNumExp(3)])).to.deep.equal([expected1.value]);
        }

        const e1 = `
            ((if (= n 0)
                 (lambda (x) x)
                 (if (= n 1)
                     f
                     (lambda (x) (f ((nf f (- n 1)) x)))))
             '(f n))`;
        const vn = parseL3Exp(p("2"));
        const vf = parseL3Exp(p("(lambda (x) (* x x))"));
        const e2 = `
            ((if (= 2 0)
                 (lambda (x) x)
                 (if (= 2 1)
                     (lambda (x) (* x x))
                     (lambda (x) ((lambda (x) (* x x)) ((nf (lambda (x) (* x x)) (- 2 1)) x)))))
             '(f n))`;
        const parsed1 = parseL3Exp(p(e1));
        const parsed2 = parseL3Exp(p(e2));
        if (isOk(parsed1) && isCExp(parsed1.value) &&
            isOk(parsed2) && isCExp(parsed2.value) &&
            isOk(vn) && isCExp(vn.value) &&
            isOk(vf) && isCExp(vf.value)) {
            expect(substitute([parsed1.value], ["n", "f"], [vn.value, vf.value])).to.deep.equal([parsed2.value]);
        }

        const lzxz = parseL3Exp(p("(lambda (z) (x z))"));
        const lwzw = parseL3Exp(p("(lambda (w) (z w))"));
        const lzlwzwz = parseL3Exp(p("(lambda (z) ((lambda (w) (z w)) z))"));
        if (isOk(lzxz) && isCExp(lzxz.value) &&
            isOk(lwzw) && isCExp(lwzw.value) &&
            isOk(lzlwzwz)) {
            expect(substitute([lzxz.value], ["x"], [lwzw.value])).to.deep.equal([lzlwzwz.value]);
        }
    });

    it('renames', () => {
        const lxx = parseL3Exp(p("(lambda (x) x)"));
        const lx1x1 = parseL3Exp(p("(lambda (x__1) x__1)"));
        if (isOk(lxx) && isCExp(lxx.value) &&
            isOk(lx1x1) && isCExp(lx1x1.value)) {
            expect(renameExps([lxx.value])).to.deep.equal([lx1x1.value]);
        }
        const l1 = parseL3Exp(p(`(((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2)`));
        const rl1 = parseL3Exp(p(`(((lambda (x__1) (lambda (z__2) (x__1 z__2))) (lambda (w__3) (z w__3))) 2)`));
        if (isOk(l1) && isCExp(l1.value) &&
            isOk(rl1) && isCExp(rl1.value)) {
            expect(renameExps([l1.value])).to.deep.equal([rl1.value]);
        }
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).to.deep.equal(makeOk(4));
        const program1 = parseL3("(L3 (define square (lambda (x) (* x x))) (square 3))");
        if (isOk(program1)) {
            expect(evalL3program(program1.value)).to.deep.equal(makeOk(9));
        }
        const program2 = parseL3("(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))");
        if (isOk(program2)) {
            expect(evalL3program(program2.value)).to.deep.equal(makeOk(3));
        }
    });

    it('applies recursive procedures', () => {
        const program = parseL3("(L3 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))");
        if (isOk(program)) {
            expect(evalL3program(program.value)).to.deep.equal(makeOk(6));
        }
    });

    it('applies higher-order functions', () => {
        // L3 higher order functions

        const program1 = parseL3(`
            (L3 (define map
                  (lambda (f l)
                    (if (eq? l '())
                        l
                        (cons (f (car l)) (map f (cdr l))))))
                (map (lambda (x) (* x x)) '(1 2 3)))`);

        if (isOk(program1)) {
            expect(evalL3program(program1.value)).to.deep.equal(makeOk(listPrim([1, 4, 9])));
        }

        const program2 = parseL3(`
            (L3 (define empty? (lambda (x) (eq? x '())))
                (define filter
                  (lambda (pred l)
                    (if (empty? l)
                        l
                        (if (pred (car l))
                            (cons (car l) (filter pred (cdr l)))
                            (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`);
        
        if (isOk(program2)) {
            expect(evalL3program(program2.value)).to.deep.equal(makeOk(listPrim([1, 3])));
        }

        const program3 = parseL3(`
            (L3 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`);
        
        if (isOk(program3)) {
            expect(evalL3program(program3.value)).to.deep.equal(makeOk(false));
        }

        const program4 = parseL3(`
            (L3 (define equal? (lambda (e1 e2)
                                 (if (eq? e1 e2)
                                     #t
                                     (if (and (pair? e1) (pair? e2))
                                         (and (equal? (car e1) (car e2)) (equal? (cdr e1) (cdr e2)))
                                         #f))))
                (and (equal? '(1 . (2 . 3)) '(1 2 . 3)) (equal? '(1 . (2)) '(1 2))))`);
        
        if (isOk(program4)) {
            expect(evalL3program(program4.value)).to.deep.equal(makeOk(true));
        }
    });

    it('evaluates the examples', () => {
        // Preserve bound variables in subst
        const program1 = parseL3(`
            (L3 (define nf
                  (lambda (f n)
                    (if (= n 0)
                        (lambda (x) x)
                        (if (= n 1)
                            f
                            (lambda (x) (f ((nf f (- n 1)) x)))))))
                ((nf (lambda (x) (* x x)) 2) 3))`);
        
        if (isOk(program1)) {
            expect(evalL3program(program1.value)).to.deep.equal(makeOk(81));
        }

        // Accidental capture of the z variable if no renaming
        const program2 = parseL3(`
            (L3 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`);

        if (isOk(program2)) {
            expect(evalL3program(program2.value)).to.deep.equal(makeOk(4));
        }

        // Y-combinator
        const program3 = parseL3(`
            (L3 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1))))))) 6))`);
        
        if (isOk(program3)) {
            expect(evalL3program(program3.value)).to.deep.equal(makeOk(720));
        }
    });
});
