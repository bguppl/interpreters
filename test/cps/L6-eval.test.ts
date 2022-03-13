import { isVarDecl, isVarRef, parseL5 } from '../../src/L5/L5-ast';
import { makeEmptySExp, makeSymbolSExp } from '../../src/L5/L5-value';
import { isClosure, makeCompoundSExp } from '../../src/L5/L5-value';
import { evalParse, evalProgram } from '../../src/cps/L6-eval';
import { makeOk, bind, isOkT } from "../../src/shared/result";

describe('L6 Eval', () => {
    it('evaluates data type literals', () => {
        expect(evalParse("1")).toEqual(makeOk(1));
        expect(evalParse("#t")).toEqual(makeOk(true));
        expect(evalParse("#f")).toEqual(makeOk(false));
        expect(evalParse("'a")).toEqual(makeOk(makeSymbolSExp("a")));
        expect(evalParse('"a"')).toEqual(makeOk("a"));
        expect(evalParse("'()")).toEqual(makeOk(makeEmptySExp()));
        expect(evalParse("'(1 2)")).toEqual(makeOk(makeCompoundSExp(1, makeCompoundSExp(2, makeEmptySExp()))));
    });

    describe('Primitive Procedures', () => {
        it('evaluates "+"', () => {
            expect(evalParse("(+ 1 2)")).toEqual(makeOk(3));
        });

        it('evaluates "-"', () => {
            expect(evalParse("(- 2 1)")).toEqual(makeOk(1));
        });

        it('evaluates "*"', () => {
            expect(evalParse("(* 2 3)")).toEqual(makeOk(6));
        });

        it('evaluates "/"', () => {
            expect(evalParse("(/ 4 2)")).toEqual(makeOk(2));
        });

        it('evaluates "<"', () => {
            expect(evalParse("(< 4 2)")).toEqual(makeOk(false));
        });

        it('evaluates ">"', () => {
            expect(evalParse("(> 4 2)")).toEqual(makeOk(true));
        });

        it('evaluates "="', () => {
            expect(evalParse("(= 4 2)")).toEqual(makeOk(false));
        });

        it('evaluates "not"', () => {
            expect(evalParse("(not #t)")).toEqual(makeOk(false));
        });

        it('evaluates "eq?"', () => {
            expect(evalParse("(eq? 'a 'a)")).toEqual(makeOk(true));
        });

        it('evaluates "string=?"', () => {
            expect(evalParse('(string=? "a" "a")')).toEqual(makeOk(true));
        });

        it('evaluates "cons"', () => {
            expect(evalParse("(cons 1 '())")).toEqual(makeOk(makeCompoundSExp(1, makeEmptySExp())));
        });

        it('evaluates "car"', () => {
            expect(evalParse("(car '(1 2))")).toEqual(makeOk(1));
        });

        it('evaluates "cdr"', () => {
            expect(evalParse("(cdr '(1 2))")).toEqual(makeOk(makeCompoundSExp(2, makeEmptySExp())));
            expect(evalParse("(cdr '(1))")).toEqual(makeOk(makeEmptySExp()));
        });

        it('evaluates "list?"', () => {
            expect(evalParse("(list? '(1))")).toEqual(makeOk(true));
            expect(evalParse("(list? '())")).toEqual(makeOk(true));
        });

        it('evaluates "number?"', () => {
            expect(evalParse("(number? 1)")).toEqual(makeOk(true));
            expect(evalParse("(number? #t)")).toEqual(makeOk(false));
        });

        it('evaluates "boolean?"', () => {
            expect(evalParse("(boolean? #t)")).toEqual(makeOk(true));
            expect(evalParse("(boolean? 0)")).toEqual(makeOk(false));
        });

        it('evaluates "symbol?"', () => {
            expect(evalParse("(symbol? 'a)")).toEqual(makeOk(true));
            expect(evalParse('(symbol? "a")')).toEqual(makeOk(false));
        });

        it('evaluates "string?"', () => {
            expect(evalParse("(string? 'a)")).toEqual(makeOk(false));
            expect(evalParse('(string? "a")')).toEqual(makeOk(true));
        });
    });

    it('evalautes "define" expressions', () => {
        expect(bind(parseL5("(L5 (define x 1) (+ x x))"), evalProgram)).toEqual(makeOk(2));
        expect(bind(parseL5("(L5 (define x 1) (define y (+ x x)) (* y y))"), evalProgram)).toEqual(makeOk(4));
    });

    it('evaluates "if" expressions', () => {
        expect(evalParse('(if (string? "a") 1 2)')).toEqual(makeOk(1));
        expect(evalParse('(if (not (string? "a")) 1 2)')).toEqual(makeOk(2));
    });

    it('evaluates procedures', () => {
        const closure = evalParse("(lambda (x) x)");
        expect(closure).toSatisfy(isOkT(isClosure));
        if (isOkT(isClosure)(closure)) {
            expect(closure.value.params[0]).toSatisfy(isVarDecl);
            expect(closure.value.body[0]).toSatisfy(isVarRef);
        }
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).toEqual(makeOk(4));
        expect(bind(parseL5("(L5 (define square (lambda (x) (* x x))) (square 3))"), evalProgram)).toEqual(makeOk(9));
        expect(bind(parseL5("(L5 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))"), evalProgram)).toEqual(makeOk(3));
    });

    it('evaluates recursive procedures', () => {
        expect(bind(parseL5("(L5 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))"), evalProgram)).toEqual(makeOk(6));
    });

    it('evaluates recursion with "letrec"', () => {
        expect(evalParse(`
            (letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))
              (f 5))`)).toEqual(makeOk(120));
    });

    it('evaluates the examples', () => {
        // Preserve bound variables
        expect(bind(parseL5(`
            (L5 (define fact (letrec ((f (lambda (n)
                                           (if (= n 0)
                                               1
                                               (* n (f (- n 1)))))))
                               f))
                (fact 5))`), evalProgram)).toEqual(makeOk(120));

        // Accidental capture of the z variable if no renaming - works without renaming in env eval.
        expect(bind(parseL5(`
            (L5 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`), evalProgram)).toEqual(makeOk(4));

        // Y-combinator
        expect(bind(parseL5(`
            (L5 (((lambda (f) (f f))
                  (lambda (fact)
                    (lambda (n)
                      (if (= n 0)
                          1
                          (* n ((fact fact) (- n 1))))))) 6))`), evalProgram)).toEqual(makeOk(720));
    });

    it('evaluates higher-order functions', () => {
        expect(bind(parseL5(`
            (L5 (define map (lambda (f l)
                              (if (eq? l '())
                                  l
                                  (cons (f (car l)) (map f (cdr l))))))
                (map (lambda (x) (* x x)) '(1 2 3)))`), evalProgram)).toEqual(evalParse("'(1 4 9)"));
        
        expect(bind(parseL5(`
            (L5 (define empty? (lambda (x) (eq? x '())))
                (define filter (lambda (pred l)
                                 (if (empty? l)
                                     l
                                     (if (pred (car l))
                                         (cons (car l) (filter pred (cdr l)))
                                         (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`), evalProgram)).toEqual(evalParse("'(1 3)"));
        
        expect(bind(parseL5(`
            (L5 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`), evalProgram)).toEqual(makeOk(false));
    });

    it('properly captures variables in closures', () => {
        expect(bind(parseL5(`
            (L5 (define makeAdder (lambda (n) (lambda (y) (+ y n))))
                (define a6 (makeAdder 6))
                (define a7 (makeAdder 7))
                (+ (a6 1) (a7 1)))`), evalProgram)).toEqual(makeOk(15));
        expect(bind(parseL5("(L5 (define f (lambda () 1)) (f))"), evalProgram)).toEqual(makeOk(1));
        expect(bind(parseL5(`
            (L5 (define makeCounter (lambda () (let ((c 0)) (lambda () (set! c (+ c 1)) c))))
                (define c1 (makeCounter))
                (define c2 (makeCounter))
                (+ (+ (c1) (c1)) (+ (c2) (c2))))`), evalProgram)).toEqual(makeOk(6));
    });

    it('evaluates type-annotated expressions', () => {
        expect(bind(parseL5("(L5 (define (a : number) 1) a)"), evalProgram)).toEqual(makeOk(1));
        expect(evalParse("((lambda ((x : T)) : T x) #t)")).toEqual(makeOk(true));
        expect(evalParse("(let (((a : boolean) #t) ((b : number) 2)) (if a b (+ b b)))")).toEqual(makeOk(2));
        expect(evalParse(`
            (letrec (((p : (number * number -> number)) (lambda ((x : number) (y : number)) (+ x y))))
                (p 1 2))`)).toEqual(makeOk(3));
    });
});
