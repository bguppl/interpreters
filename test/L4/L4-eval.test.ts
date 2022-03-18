import * as E from "fp-ts/Either";
import { and } from "fp-ts/Predicate";
import { pipe } from "fp-ts/function";
import { makeVarDecl, makeVarRef, parseL4 } from '../../src/L4/L4-ast';
import { isEnv, makeEmptyEnv, makeExtEnv, applyEnv } from '../../src/L4/L4-env';
import { makeClosure, makeCompoundSExp, makeEmptySExp, makeSymbolSExp } from '../../src/L4/L4-value';
import { evalParse, evalProgram } from '../../src/L4/L4-eval';


describe('L4 Environment', () => {
    const emptyEnv = makeEmptyEnv();
    const env1 = makeExtEnv(["a", "b"], [1, 2], emptyEnv);
    const env2 = makeExtEnv(["a"], [3], env1);

    it('identifies the environment', () => {
        expect(isEnv(emptyEnv)).toBe(true);
        expect(isEnv(env1)).toBe(true);
    });

    it('applies the environment', () => {
        expect(applyEnv(env1, "a")).toEqual(E.of(1));
        expect(applyEnv(env2, "a")).toEqual(E.of(3));
        expect(applyEnv(env2, "b")).toEqual(E.of(2));
    });
});

describe('L4 Eval', () => {
    it('evaluates data type literals', () => {
        expect(evalParse("1")).toEqual(E.of(1));
        expect(evalParse("#t")).toEqual(E.of(true));
        expect(evalParse("#f")).toEqual(E.of(false));
        expect(evalParse("'a")).toEqual(E.of(makeSymbolSExp("a")));
        expect(evalParse('"a"')).toEqual(E.of("a"));
        expect(evalParse("'()")).toEqual(E.of(makeEmptySExp()));
        expect(evalParse("'(1 2)")).toEqual(E.of(makeCompoundSExp(1, makeCompoundSExp(2, makeEmptySExp()))));
        expect(evalParse("'(1 (2))")).toEqual(
            E.of(makeCompoundSExp(1, makeCompoundSExp(makeCompoundSExp(2, makeEmptySExp()), makeEmptySExp())))
        );
    });

    describe('Primitive Procedures', () => {
        it('evaluates "+"', () => {
            expect(evalParse("(+ 1 2)")).toEqual(E.of(3));
        });

        it('evaluates "-"', () => {
            expect(evalParse("(- 2 1)")).toEqual(E.of(1));
        });

        it('evaluates "*"', () => {
            expect(evalParse("(* 2 3)")).toEqual(E.of(6));
        });

        it('evaluates "/"', () => {
            expect(evalParse("(/ 4 2)")).toEqual(E.of(2));
        });

        it('evaluates "<"', () => {
            expect(evalParse("(< 4 2)")).toEqual(E.of(false));
        });

        it('evaluates ">"', () => {
            expect(evalParse("(> 4 2)")).toEqual(E.of(true));
        });

        it('evaluates "="', () => {
            expect(evalParse("(= 4 2)")).toEqual(E.of(false));
        });

        it('evaluates "not"', () => {
            expect(evalParse("(not #t)")).toEqual(E.of(false));
        });

        it('evaluates "eq?"', () => {
            expect(evalParse("(eq? 'a 'a)")).toEqual(E.of(true));
        });

        it('evaluates "string=?"', () => {
            expect(evalParse('(string=? "a" "a")')).toEqual(E.of(true));
        });

        it('evaluates "cons"', () => {
            expect(evalParse("(cons 1 '())")).toEqual(E.of(makeCompoundSExp(1, makeEmptySExp())));
            expect(evalParse("(cons 1 '(2))")).toEqual(E.of(makeCompoundSExp(1, makeCompoundSExp(2, makeEmptySExp()))));
        });

        it('evaluates "car"', () => {
            expect(evalParse("(car '(1 2))")).toEqual(E.of(1));
        });

        it('evaluates "cdr"', () => {
            expect(evalParse("(cdr '(1 2))")).toEqual(E.of(makeCompoundSExp(2, makeEmptySExp())));
            expect(evalParse("(cdr '(1))")).toEqual(E.of(makeEmptySExp()));
        });

        it('evaluates "list?"', () => {
            expect(evalParse("(list? '(1))")).toEqual(E.of(true));
            expect(evalParse("(list? '())")).toEqual(E.of(true));
        });

        it('evaluates "number?"', () => {
            expect(evalParse("(number? 1)")).toEqual(E.of(true));
            expect(evalParse("(number? #t)")).toEqual(E.of(false));
        });

        it('evaluates "boolean?"', () => {
            expect(evalParse("(boolean? #t)")).toEqual(E.of(true));
            expect(evalParse("(boolean? 0)")).toEqual(E.of(false));
        });

        it('evaluates "symbol?"', () => {
            expect(evalParse("(symbol? 'a)")).toEqual(E.of(true));
            expect(evalParse('(symbol? "a")')).toEqual(E.of(false));
        });

        it('evaluates "string?"', () => {
            expect(evalParse('(string? "a")')).toEqual(E.of(true));
            expect(evalParse("(string? 'a)")).toEqual(E.of(false));
        });
    });

    it('evaluates "define" expressions', () => {
        expect(pipe(parseL4("(L4 (define x 1) (+ x x))"), E.chain(evalProgram))).toEqual(E.of(2));
        expect(pipe(parseL4("(L4 (define x 1) (define y (+ x x)) (* y y))"), E.chain(evalProgram))).toEqual(E.of(4));
    });

    it('evaluates "if" expressions', () => {
        expect(evalParse('(if (string? "a") 1 2)')).toEqual(E.of(1));
        expect(evalParse('(if (not (string? "a")) 1 2)')).toEqual(E.of(2));
    });

    it('evaluates procedures', () => {
        expect(evalParse("(lambda (x) x)")).toEqual(E.of(makeClosure([makeVarDecl("x")], [makeVarRef("x")], makeEmptyEnv())));
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).toEqual(E.of(4));
        expect(pipe(parseL4("(L4 (define square (lambda (x) (* x x))) (square 3))"), E.chain(evalProgram))).toEqual(E.of(9));
        expect(pipe(parseL4("(L4 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))"), E.chain(evalProgram))).toEqual(E.of(3));
    });

    it('returns an error for recursive procedures', () => {
        expect(pipe(parseL4("(L4 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))"), E.chain(evalProgram))).toSatisfy(E.isLeft);
    });

    it('evaluates recursion with "letrec"', () => {
        expect(evalParse(`(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`)).toEqual(E.of(120));
        expect(pipe(parseL4(`
            (L4 (define equal? 
                    (letrec ((equal? (lambda (e1 e2)
                                       (if (eq? e1 e2)
                                           #t
                                           (if (and (pair? e1) (pair? e2))
                                               (and (equal? (car e1) (car e2))
                                               (equal? (cdr e1) (cdr e2)))
                                           #f)))))
                       equal?))
                (and (equal? '(1 . (2 . 3)) '(1 2 . 3)) (equal? '(1 . (2)) '(1 2))))`), E.chain(evalProgram))).toEqual(E.of(true));
    });

    it('evaluates the examples', () => {
        // Preserve bound variables
        expect(pipe(parseL4(`
            (L4 (define fact
                  (letrec ((f (lambda (n)
                                (if (= n 0)
                                    1
                                    (* n (f (- n 1)))))))
                    f))
                (fact 5))`), E.chain(evalProgram))).toEqual(E.of(120));

        // Accidental capture of the z variable if no renaming - works without renaming in env eval.
        expect(pipe(parseL4(`
            (L4 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`), E.chain(evalProgram))).toEqual(E.of(4));

        // Y-combinator
        expect(pipe(parseL4(`
            (L4 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1))))))) 6))`), E.chain(evalProgram))).toEqual(E.of(720));
    });

    it('evaluates higher-order functions', () => {
        expect(pipe(parseL4(`
            (L4 (define map
                  (letrec ((map (lambda (f l)
                                  (if (eq? l '())
                                      l
                                      (cons (f (car l)) (map f (cdr l)))))))
                    map))
                (map (lambda (x) (* x x)) '(1 2 3)))`), E.chain(evalProgram))).toEqual(evalParse("'(1 4 9)"));
        
        expect(pipe(parseL4(`
            (L4 (define empty? (lambda (x) (eq? x '())))
                (define filter
                  (letrec ((filter (lambda (pred l)
                                     (if (empty? l)
                                         l
                                         (if (pred (car l))
                                             (cons (car l) (filter pred (cdr l)))
                                             (filter pred (cdr l)))))))
                    filter))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`), E.chain(evalProgram))).toEqual(evalParse("'(1 3)"));
        
        expect(pipe(parseL4(`
            (L4 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`), E.chain(evalProgram))).toEqual(E.of(false));
    });

    it('properly captures variables in closures', () => {
        expect(pipe(parseL4(`
            (L4 (define makeAdder (lambda (n) (lambda (y) (+ y n))))
                (define a6 (makeAdder 6))
                (define a7 (makeAdder 7))
                (+ (a6 1) (a7 1)))`), E.chain(evalProgram))).toEqual(E.of(15));
    });
});
