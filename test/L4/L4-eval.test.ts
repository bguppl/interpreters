import { expect } from 'chai';
import { makeVarDecl, makeVarRef, parseL4 } from '../../src/L4/L4-ast';
import { isEnv, makeEmptyEnv, makeExtEnv, applyEnv } from '../../src/L4/L4-env';
import { makeClosure, makeCompoundSExp, makeEmptySExp, makeSymbolSExp } from '../../src/L4/L4-value';
import { evalParse, evalProgram } from '../../src/L4/L4-eval';
import { makeOk, bind, isFailure } from '../../src/shared/result';


describe('L4 Environment', () => {
    const emptyEnv = makeEmptyEnv();
    const env1 = makeExtEnv(["a", "b"], [1, 2], emptyEnv);
    const env2 = makeExtEnv(["a"], [3], env1);

    it('identifies the environment', () => {
        expect(isEnv(emptyEnv)).to.be.true;
        expect(isEnv(env1)).to.be.true;
    });

    it('applies the environment', () => {
        expect(applyEnv(env1, "a")).to.deep.equal(makeOk(1));
        expect(applyEnv(env2, "a")).to.deep.equal(makeOk(3));
        expect(applyEnv(env2, "b")).to.deep.equal(makeOk(2));
    });
});

describe('L4 Eval', () => {
    it('evaluates data type literals', () => {
        expect(evalParse("1")).to.deep.equal(makeOk(1));
        expect(evalParse("#t")).to.deep.equal(makeOk(true));
        expect(evalParse("#f")).to.deep.equal(makeOk(false));
        expect(evalParse("'a")).to.deep.equal(makeOk(makeSymbolSExp("a")));
        expect(evalParse('"a"')).to.deep.equal(makeOk("a"));
        expect(evalParse("'()")).to.deep.equal(makeOk(makeEmptySExp()));
        expect(evalParse("'(1 2)")).to.deep.equal(makeOk(makeCompoundSExp(1, makeCompoundSExp(2, makeEmptySExp()))));
        expect(evalParse("'(1 (2))")).to.deep.equal(makeOk(makeCompoundSExp(1, makeCompoundSExp(makeCompoundSExp(2, makeEmptySExp()), makeEmptySExp()))));
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
            expect(evalParse("(cons 1 '())")).to.deep.equal(makeOk(makeCompoundSExp(1, makeEmptySExp())));
            expect(evalParse("(cons 1 '(2))")).to.deep.equal(makeOk(makeCompoundSExp(1, makeCompoundSExp(2, makeEmptySExp()))));
        });

        it('evaluates "car"', () => {
            expect(evalParse("(car '(1 2))")).to.deep.equal(makeOk(1));
        });

        it('evaluates "cdr"', () => {
            expect(evalParse("(cdr '(1 2))")).to.deep.equal(makeOk(makeCompoundSExp(2, makeEmptySExp())));
            expect(evalParse("(cdr '(1))")).to.deep.equal(makeOk(makeEmptySExp()));
        });

        it('evaluates "list?"', () => {
            expect(evalParse("(list? '(1))")).to.deep.equal(makeOk(true));
            expect(evalParse("(list? '())")).to.deep.equal(makeOk(true));
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

    it('evaluates "define" expressions', () => {
        expect(bind(parseL4("(L4 (define x 1) (+ x x))"), evalProgram)).to.deep.equal(makeOk(2));
        expect(bind(parseL4("(L4 (define x 1) (define y (+ x x)) (* y y))"), evalProgram)).to.deep.equal(makeOk(4));
    });

    it('evaluates "if" expressions', () => {
        expect(evalParse('(if (string? "a") 1 2)')).to.deep.equal(makeOk(1));
        expect(evalParse('(if (not (string? "a")) 1 2)')).to.deep.equal(makeOk(2));
    });

    it('evaluates procedures', () => {
        expect(evalParse("(lambda (x) x)")).to.deep.equal(makeOk(makeClosure([makeVarDecl("x")], [makeVarRef("x")], makeEmptyEnv())));
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).to.deep.equal(makeOk(4));
        expect(bind(parseL4("(L4 (define square (lambda (x) (* x x))) (square 3))"), evalProgram)).to.deep.equal(makeOk(9));
        expect(bind(parseL4("(L4 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))"), evalProgram)).to.deep.equal(makeOk(3));
    });

    it('returns an error for recursive procedures', () => {
        expect(bind(parseL4("(L4 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))"), evalProgram)).to.satisfy(isFailure);
    });

    it('evaluates recursion with "letrec"', () => {
        expect(evalParse(`(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`)).to.deep.equal(makeOk(120));
        expect(bind(parseL4(`
            (L4 (define equal? 
                    (letrec ((equal? (lambda (e1 e2)
                                       (if (eq? e1 e2)
                                           #t
                                           (if (and (pair? e1) (pair? e2))
                                               (and (equal? (car e1) (car e2))
                                               (equal? (cdr e1) (cdr e2)))
                                           #f)))))
                       equal?))
                (and (equal? '(1 . (2 . 3)) '(1 2 . 3)) (equal? '(1 . (2)) '(1 2))))`), evalProgram)).to.deep.equal(makeOk(true));
    });

    it('evaluates the examples', () => {
        // Preserve bound variables
        expect(bind(parseL4(`
            (L4 (define fact
                  (letrec ((f (lambda (n)
                                (if (= n 0)
                                    1
                                    (* n (f (- n 1)))))))
                    f))
                (fact 5))`), evalProgram)).to.deep.equal(makeOk(120));

        // Accidental capture of the z variable if no renaming - works without renaming in env eval.
        expect(bind(parseL4(`
            (L4 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`), evalProgram)).to.deep.equal(makeOk(4));

        // Y-combinator
        expect(bind(parseL4(`
            (L4 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1))))))) 6))`), evalProgram)).to.deep.equal(makeOk(720));
    });

    it('evaluates higher-order functions', () => {
        expect(bind(parseL4(`
            (L4 (define map
                  (letrec ((map (lambda (f l)
                                  (if (eq? l '())
                                      l
                                      (cons (f (car l)) (map f (cdr l)))))))
                    map))
                (map (lambda (x) (* x x)) '(1 2 3)))`), evalProgram)).to.deep.equal(evalParse("'(1 4 9)"));
        
        expect(bind(parseL4(`
            (L4 (define empty? (lambda (x) (eq? x '())))
                (define filter
                  (letrec ((filter (lambda (pred l)
                                     (if (empty? l)
                                         l
                                         (if (pred (car l))
                                             (cons (car l) (filter pred (cdr l)))
                                             (filter pred (cdr l)))))))
                    filter))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`), evalProgram)).to.deep.equal(evalParse("'(1 3)"));
        
        expect(bind(parseL4(`
            (L4 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`), evalProgram)).to.deep.equal(makeOk(false));
    });

    it('properly captures variables in closures', () => {
        expect(bind(parseL4(`
            (L4 (define makeAdder (lambda (n) (lambda (y) (+ y n))))
                (define a6 (makeAdder 6))
                (define a7 (makeAdder 7))
                (+ (a6 1) (a7 1)))`), evalProgram)).to.deep.equal(makeOk(15));
    });
});
