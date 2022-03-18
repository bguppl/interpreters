import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { isVarDecl, isVarRef, parseL5 } from '../../src/L5/L5-ast';
import { evalParse, evalProgram } from '../../src/L5/L5-eval';
import { makeEmptySExp, makeSymbolSExp } from '../../src/L5/L5-value';
import { isClosure, makeCompoundSExp } from '../../src/L5/L5-value';
import { isRightT } from "../shared/test-helpers";

describe('L5 Eval', () => {
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
            expect(evalParse("(string? 'a)")).toEqual(E.of(false));
            expect(evalParse('(string? "a")')).toEqual(E.of(true));
        });
    });

    it('evaluates "define" expressions', () => {
        expect(pipe(parseL5("(L5 (define x 1) (+ x x))"), E.chain(evalProgram))).toEqual(E.of(2));
        expect(pipe(parseL5("(L5 (define x 1) (define y (+ x x)) (* y y))"), E.chain(evalProgram))).toEqual(E.of(4));
    });

    it('evaluates "if" expressions', () => {
        expect(evalParse('(if (string? "a") 1 2)')).toEqual(E.of(1));
        expect(evalParse('(if (not (string? "a")) 1 2)')).toEqual(E.of(2));
    });

    it('evaluates procedures', () => {
        const closure = evalParse("(lambda (x) x)");
        expect(closure).toSatisfy(isRightT(isClosure));
        if (isRightT(isClosure)(closure)) {
            expect(closure.right.params[0]).toSatisfy(isVarDecl);
            expect(closure.right.body[0]).toSatisfy(isVarRef);
        }
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).toEqual(E.of(4));
        expect(pipe(parseL5("(L5 (define square (lambda (x) (* x x))) (square 3))"), E.chain(evalProgram))).toEqual(E.of(9));
        expect(pipe(parseL5("(L5 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))"), E.chain(evalProgram))).toEqual(E.of(3));
    });

    it('evaluates recursive procedures', () => {
        expect(pipe(parseL5("(L5 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))"), E.chain(evalProgram))).toEqual(E.of(6));
    });

    it('evaluates recursion with "letrec"', () => {
        expect(evalParse("(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))")).toEqual(E.of(120));
    });

    it('returns a Failure in "letrec" if a binding is invalid', () => {
        expect(evalParse(`(letrec ((a (1 2))) a)`)).toSatisfy(E.isLeft);
    });

    it('evaluates the examples', () => {
        // Preserve bound variables
        expect(pipe(parseL5(`
            (L5 (define fact
                  (letrec ((f (lambda (n)
                                (if (= n 0)
                                    1
                                    (* n (f (- n 1)))))))
                    f))
                (fact 5))`), E.chain(evalProgram))).toEqual(E.of(120));

        // Accidental capture of the z variable if no renaming - works without renaming in env eval.
        expect(pipe(parseL5(`
            (L5 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`), E.chain(evalProgram))).toEqual(E.of(4));

        // Y-combinator
        expect(pipe(parseL5(`
            (L5 (((lambda (f) (f f))
                  (lambda (fact)
                    (lambda (n)
                      (if (= n 0)
                          1
                          (* n ((fact fact) (- n 1))))))) 6))`), E.chain(evalProgram))).toEqual(E.of(720));
    });

    it('evaluates higher-order functions', () => {
        expect(pipe(parseL5(`
            (L5 (define map
                  (lambda (f l)
                    (if (eq? l '())
                        l
                        (cons (f (car l)) (map f (cdr l))))))
                (map (lambda (x) (* x x)) '(1 2)))`), E.chain(evalProgram))).toEqual(E.of(makeCompoundSExp(1, makeCompoundSExp(4, makeEmptySExp()))));
        
        expect(pipe(parseL5(`
            (L5 (define empty? (lambda (x) (eq? x '())))
                (define filter
                  (lambda (pred l)
                    (if (empty? l)
                        l
                        (if (pred (car l))
                            (cons (car l) (filter pred (cdr l)))
                            (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`), E.chain(evalProgram))).toEqual(E.of(makeCompoundSExp(1, makeCompoundSExp(3, makeEmptySExp()))));
        
        expect(pipe(parseL5(`
            (L5 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`), E.chain(evalProgram))).toEqual(E.of(false));
    });

    it('properly captures variables in closures', () => {
        expect(pipe(parseL5(`
            (L5 (define makeAdder (lambda (n) (lambda (y) (+ y n))))
                (define a6 (makeAdder 6))
                (define a7 (makeAdder 7))
                (+ (a6 1) (a7 1)))`), E.chain(evalProgram))).toEqual(E.of(15));

        expect(pipe(parseL5(`
            (L5 (define makeCounter (lambda () (let ((c 0)) (lambda () (set! c (+ c 1)) c))))
                (define c1 (makeCounter))
                (define c2 (makeCounter))
                (+ (+ (c1) (c1)) (+ (c2) (c2))))`), E.chain(evalProgram))).toEqual(E.of(6));
    });

    it('evaluates type-annotated expressions', () => {
        expect(pipe(parseL5("(L5 (define (a : number) 1) a)"), E.chain(evalProgram))).toEqual(E.of(1));
        expect(evalParse("((lambda ((x : T)) : T x) #t)")).toEqual(E.of(true));
        expect(evalParse("(let (((a : boolean) #t) ((b : number) 2)) (if a b (+ b b)))")).toEqual(E.of(2));
        expect(evalParse(`
            (letrec (((p : (number * number -> number)) (lambda ((x : number) (y : number)) (+ x y))))
                (p 1 2))`)).toEqual(E.of(3));
    });
});
