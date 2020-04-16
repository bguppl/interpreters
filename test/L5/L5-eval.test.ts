import { expect } from 'chai';
import { isVarDecl, isVarRef, parseL5 } from '../../src/L5/L5-ast';
import { evalParse, evalProgram } from '../../src/L5/L5-eval';
import { makeEmptySExp, makeSymbolSExp } from '../../src/L5/L5-value';
import { isClosure, makeCompoundSExp } from '../../src/L5/L5-value';
import { makeOk, bind, isOkT } from '../../src/shared/result';

describe('L5 Eval', () => {
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
            expect(evalParse("(string? 'a)")).to.deep.equal(makeOk(false));
            expect(evalParse('(string? "a")')).to.deep.equal(makeOk(true));
        });
    });

    it('evaluates "define" expressions', () => {
        expect(bind(parseL5("(L5 (define x 1) (+ x x))"), evalProgram)).to.deep.equal(makeOk(2));
        expect(bind(parseL5("(L5 (define x 1) (define y (+ x x)) (* y y))"), evalProgram)).to.deep.equal(makeOk(4));
    });

    it('evaluates "if" expressions', () => {
        expect(evalParse('(if (string? "a") 1 2)')).to.deep.equal(makeOk(1));
        expect(evalParse('(if (not (string? "a")) 1 2)')).to.deep.equal(makeOk(2));
    });

    it('evaluates procedures', () => {
        let closure = evalParse("(lambda (x) x)");
        expect(closure).to.satisfy(isOkT(isClosure));
        if (isOkT(isClosure)(closure)) {
            expect(closure.value.params[0]).to.satisfy(isVarDecl);
            expect(closure.value.body[0]).to.satisfy(isVarRef);
        }
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).to.deep.equal(makeOk(4));
        expect(bind(parseL5("(L5 (define square (lambda (x) (* x x))) (square 3))"), evalProgram)).to.deep.equal(makeOk(9));
        expect(bind(parseL5("(L5 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))"), evalProgram)).to.deep.equal(makeOk(3));
    });

    it('evaluates recursive procedures', () => {
        expect(bind(parseL5("(L5 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))"), evalProgram)).to.deep.equal(makeOk(6));
    });

    it('evaluates recursion with "letrec"', () => {
        expect(evalParse("(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))")).to.deep.equal(makeOk(120));
    });

    it('evaluates the examples', () => {
        // Preserve bound variables
        expect(bind(parseL5(`
            (L5 (define fact
                  (letrec ((f (lambda (n)
                                (if (= n 0)
                                    1
                                    (* n (f (- n 1)))))))
                    f))
                (fact 5))`), evalProgram)).to.deep.equal(makeOk(120));

        // Accidental capture of the z variable if no renaming - works without renaming in env eval.
        expect(bind(parseL5(`
            (L5 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`), evalProgram)).to.deep.equal(makeOk(4));

        // Y-combinator
        expect(bind(parseL5(`
            (L5 (((lambda (f) (f f))
                  (lambda (fact)
                    (lambda (n)
                      (if (= n 0)
                          1
                          (* n ((fact fact) (- n 1))))))) 6))`), evalProgram)).to.deep.equal(makeOk(720));
    });

    it('evaluates higher-order functions', () => {
        expect(bind(parseL5(`
            (L5 (define map
                  (lambda (f l)
                    (if (eq? l '())
                        l
                        (cons (f (car l)) (map f (cdr l))))))
                (map (lambda (x) (* x x)) '(1 2)))`), evalProgram)).to.deep.equal(makeOk(makeCompoundSExp(1, makeCompoundSExp(4, makeEmptySExp()))));
        
        expect(bind(parseL5(`
            (L5 (define empty? (lambda (x) (eq? x '())))
                (define filter
                  (lambda (pred l)
                    (if (empty? l)
                        l
                        (if (pred (car l))
                            (cons (car l) (filter pred (cdr l)))
                            (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`), evalProgram)).to.deep.equal(makeOk(makeCompoundSExp(1, makeCompoundSExp(3, makeEmptySExp()))));
        
        expect(bind(parseL5(`
            (L5 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`), evalProgram)).to.deep.equal(makeOk(false));
    });

    it('properly captures variables in closures', () => {
        expect(bind(parseL5(`
            (L5 (define makeAdder (lambda (n) (lambda (y) (+ y n))))
                (define a6 (makeAdder 6))
                (define a7 (makeAdder 7))
                (+ (a6 1) (a7 1)))`), evalProgram)).to.deep.equal(makeOk(15));

        expect(bind(parseL5(`
            (L5 (define makeCounter (lambda () (let ((c 0)) (lambda () (set! c (+ c 1)) c))))
                (define c1 (makeCounter))
                (define c2 (makeCounter))
                (+ (+ (c1) (c1)) (+ (c2) (c2))))`), evalProgram)).to.deep.equal(makeOk(6));
    });

    it('evaluates type-annotated expressions', () => {
        expect(bind(parseL5("(L5 (define (a : number) 1) a)"), evalProgram)).to.deep.equal(makeOk(1));
        expect(evalParse("((lambda ((x : T)) : T x) #t)")).to.deep.equal(makeOk(true));
        expect(evalParse("(let (((a : boolean) #t) ((b : number) 2)) (if a b (+ b b)))")).to.deep.equal(makeOk(2));
        expect(evalParse(`
            (letrec (((p : (number * number -> number)) (lambda ((x : number) (y : number)) (+ x y))))
                (p 1 2))`)).to.deep.equal(makeOk(3));
    });
});
