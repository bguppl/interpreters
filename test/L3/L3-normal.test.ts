import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { parseL3, makePrimOp } from '../../src/L3/L3-ast';
import { listPrim } from "../../src/L3/evalPrimitive";
import { evalNormalParse, evalNormalProgram } from '../../src/L3/L3-normal';
import { isClosure } from '../../src/L3/L3-value';

describe('L3 Normal Eval', () => {
    it('evaluates atoms', () => {
        expect(evalNormalParse("1")).toEqual(E.of(1));
        expect(evalNormalParse("#t")).toEqual(E.of(true));
        expect(evalNormalParse('"a"')).toEqual(E.of("a"));
        expect(evalNormalParse("+")).toEqual(E.of(makePrimOp("+")));
    });

    it('evaluates primitive procedures', () => {
        expect(evalNormalParse("(+ 1 2)")).toEqual(E.of(3));
        expect(evalNormalParse("(< 1 2)")).toEqual(E.of(true));
        expect(evalNormalParse("(not (> 1 2))")).toEqual(E.of(true));
        expect(evalNormalParse("(+ (* 2 2) 3)")).toEqual(E.of(7));
        expect(evalNormalParse("(- 2 1)")).toEqual(E.of(1));
        expect(evalNormalParse("(/ 4 2)")).toEqual(E.of(2));
    });

    it('evaluates L2 syntactic forms', () => {
        expect(evalNormalParse("(if (< 1 2) 3 -3)")).toEqual(E.of(3));
        expect(evalNormalParse("(lambda (x) x)")).toSatisfy(e => E.isRight(e) && isClosure(e.right));
    });

    it('evaluates L3 syntactic forms', () => {
        expect(evalNormalParse("(cons 1 '())")).toEqual(E.of(listPrim([1])));
        expect(evalNormalParse("(car '(1 2))")).toEqual(E.of(1));
        expect(evalNormalParse("(cdr '(1 2))")).toEqual(E.of(listPrim([2])));
        expect(evalNormalParse("(number? 'x)")).toEqual(E.of(false));
        expect(evalNormalParse("(number? 1)")).toEqual(E.of(true));
        expect(evalNormalParse("(symbol? 'x)")).toEqual(E.of(true));
        expect(evalNormalParse("(symbol? 1)")).toEqual(E.of(false));
        expect(evalNormalParse("(pair? 1)")).toEqual(E.of(false));
        expect(evalNormalParse("(pair? '(1 2))")).toEqual(E.of(true));
        expect(evalNormalParse("(boolean? 1)")).toEqual(E.of(false));
        expect(evalNormalParse("(boolean? #t)")).toEqual(E.of(true));
        expect(evalNormalParse("(eq? 'x 'x)")).toEqual(E.of(true));
    });

    it('evaluates programs', () => {
        expect(pipe(parseL3(`(L3 (define x (+ 3 2)) (* x x))`), E.chain(evalNormalProgram))).toEqual(E.of(25));
        expect(pipe(parseL3(`(L3 (define x (+ 3 2)) (* x x) (+ x x))`), E.chain(evalNormalProgram))).toEqual(E.of(10));
    });

    it('evaluates procedures', () => {
        expect(pipe(parseL3(`(L3 (define f (lambda (x) (* x x))) (f 3))`), E.chain(evalNormalProgram))).toEqual(E.of(9));
        expect(pipe(parseL3(`(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))`), E.chain(evalNormalProgram))).toEqual(E.of(3));
    });

    it('evaluates closure', () => {
        expect(pipe(parseL3(`(L3 (define make-adder (lambda (c) (lambda (x) (* c c) (+ x x) (+ x c)))) (define a2 (make-adder 2)) (a2 7))`), E.chain(evalNormalProgram))).toEqual(E.of(9));
    });

    it('evaluates recursive procedures', () => {
       expect(pipe(parseL3(`(L3 (define f
                                  (lambda (x)
                                    (if (= x 0)
                                        1
                                        (* x (f (- x 1))))))
                                (f 5))`), E.chain(evalNormalProgram))).toEqual(E.of(120));
    });

    it('evaluates higher-order functions', () => {
        expect(pipe(parseL3(`
            (L3 (define map
                  (lambda (f l)
                    (if (eq? l '())
                        l
                        (cons (f (car l)) (map f (cdr l))))))
                (map (lambda (x) (* x x)) '(1 2 3)))`), E.chain(evalNormalProgram))).toEqual(E.of(listPrim([1, 4, 9])));

        expect(pipe(parseL3(`
            (L3 (define empty? (lambda (x) (eq? x '())))
                (define filter (lambda (pred l)
                                 (if (empty? l)
                                     l
                                     (if (pred (car l))
                                         (cons (car l) (filter pred (cdr l)))
                                         (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`), E.chain(evalNormalProgram))).toEqual(E.of(listPrim([1, 3])));

        expect(pipe(parseL3(`
            (L3 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`), E.chain(evalNormalProgram))).toEqual(E.of(false));
    });

    it('evaluates the examples', () => {
        // Preserve bound variables in subst
        expect(pipe(parseL3(`
            (L3 (define nf
                  (lambda (f n)
                    (if (= n 0)
                        (lambda (x) x)
                        (if (= n 1)
                            f
                            (lambda (x) (f ((nf f (- n 1)) x)))))))
                ((nf (lambda (x) (* x x)) 2) 3))`), E.chain(evalNormalProgram))).toEqual(E.of(81));

        // Accidental capture of the z variable if no renaming
        expect(pipe(parseL3(`
            (L3 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`), E.chain(evalNormalProgram))).toEqual(E.of(4));

        // Y-combinator
        expect(pipe(parseL3(`
            (L3 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1))))))) 6))`), E.chain(evalNormalProgram))).toEqual(E.of(720));
    });

    it('evaluates programs which would loop in applicative order, but complete in normal order', () => {
        expect(pipe(parseL3(`
            (L3 (define loop (lambda () (loop)))
                (define f (lambda (x y z) (if (= x 1) y z)))
                (f 1 2 (loop)))`), E.chain(evalNormalProgram))).toEqual(E.of(2));

        expect(pipe(parseL3(`
        (L3 (define loop (lambda (x) (loop x)))
            (define g (lambda (x) 5))
            (g (loop 0)))`), E.chain(evalNormalProgram))).toEqual(E.of(5));
    });

    it('evaluates programs which would give an error in applicative order, but not in normal order', () => {
        expect(pipe(parseL3(`
            (L3 (define try
                  (lambda (a b)
                    (if (= a 0)
                        1
                        b)))
                (try 0 (/ 1 0)))`), E.chain(evalNormalProgram))).toEqual(E.of(1));
    });

    it('evaluates programs which would cause side-effects (or failure) in applicative order, but not in normal order', () => {
        expect(pipe(parseL3(`
            (L3 (define f (lambda (x) (display x) (+ x 1)))
                (define g (lambda (x) 5))
                (g (f 0)))`), E.chain(evalNormalProgram))).toEqual(E.of(5));
    });
});
