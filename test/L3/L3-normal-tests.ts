import { expect } from 'chai';
import { makePrimOp } from '../../src/L3/L3-ast';
import { listPrim } from '../../src/L3/L3-eval';
import { evalNormalParse } from '../../src/L3/L3-normal';
import { isClosure } from '../../src/L3/L3-value';

describe('L3 Normal Eval', () => {
    it('evaluates atoms', () => {
        expect(evalNormalParse("1")).to.equal(1);
        expect(evalNormalParse("#t")).to.be.true;
        expect(evalNormalParse("+")).to.deep.equal(makePrimOp("+"));
    });

    it('evaluates primitive procedures', () => {
        expect(evalNormalParse("(+ 1 2)")).to.equal(3);
        expect(evalNormalParse("(< 1 2)")).to.be.true;
        expect(evalNormalParse("(not (> 1 2))")).to.be.true;
        expect(evalNormalParse("(+ (* 2 2) 3)")).to.equal(7);
    });

    it('evaluates L2 syntactic forms', () => {
        expect(evalNormalParse("(if (< 1 2) 3 -3)")).to.equal(3);
        expect(isClosure(evalNormalParse("(lambda (x) x)"))).to.be.true;
    });

    it('evaluates L3 syntactic forms', () => {
        expect(evalNormalParse("(cons 1 '())")).to.deep.equal(listPrim([1]));
        expect(evalNormalParse("(car '(1 2))")).to.equal(1);
        expect(evalNormalParse("(cdr '(1 2))")).to.deep.equal(listPrim([2]));
        expect(evalNormalParse("(number? 'x)")).to.be.false;
        expect(evalNormalParse("(number? 1)")).to.be.true;
        expect(evalNormalParse("(symbol? 'x)")).to.be.true;
        expect(evalNormalParse("(symbol? 1)")).to.be.false;
        expect(evalNormalParse("(pair? 1)")).to.be.false;
        expect(evalNormalParse("(pair? '(1 2))")).to.be.true;
        expect(evalNormalParse("(boolean? 1)")).to.be.false;
        expect(evalNormalParse("(boolean? #t)")).to.be.true;
        expect(evalNormalParse("(eq? 'x 'x)")).to.be.true;
    });

    it('evaluates programs', () => {
        expect(evalNormalParse(`(L3 (define x (+ 3 2)) (* x x))`)).to.equal(25);
        expect(evalNormalParse(`(L3 (define x (+ 3 2)) (* x x) (+ x x))`)).to.equal(10);
    });

    it('evaluates procedures', () => {
        expect(evalNormalParse(`(L3 (define f (lambda (x) (* x x))) (f 3))`)).to.equal(9);
        expect(evalNormalParse(`(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))`)).to.equal(3);
    });

    it('evaluates recursive procedures', () => {
       expect(evalNormalParse(`(L3 (define f
                                     (lambda (x)
                                       (if (= x 0)
                                           1
                                           (* x (f (- x 1))))))
                                   (f 5))`)).to.equal(120);
    });

    it('evaluates higher-order functions', () => {
        expect(evalNormalParse(`
            (L3 (define map
                  (lambda (f l)
                    (if (eq? l '())
                        l
                        (cons (f (car l)) (map f (cdr l))))))
                (map (lambda (x) (* x x)) '(1 2 3)))`)).to.deep.equal(listPrim([1, 4, 9]));

        expect(evalNormalParse(`
            (L3 (define empty? (lambda (x) (eq? x '())))
                (define filter (lambda (pred l)
                                 (if (empty? l)
                                     l
                                     (if (pred (car l))
                                         (cons (car l) (filter pred (cdr l)))
                                         (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`)).to.deep.equal(listPrim([1, 3]));

        expect(evalNormalParse(`
            (L3 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`)).to.be.false;
    });

    it('evaluates the examples', () => {
        // Preserve bound variables in subst
        expect(evalNormalParse(`
            (L3 (define nf
                  (lambda (f n)
                    (if (= n 0)
                        (lambda (x) x)
                        (if (= n 1)
                            f
                            (lambda (x) (f ((nf f (- n 1)) x)))))))
                ((nf (lambda (x) (* x x)) 2) 3))`)).to.equal(81);

        // Accidental capture of the z variable if no renaming
        expect(evalNormalParse(`
            (L3 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`)).to.equal(4);

        // Y-combinator
        expect(evalNormalParse(`
            (L3 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1))))))) 6))`)).to.equal(720);
    });

    it('evaluates programs which would loop in applicative order, but complete in normal order', () => {
        expect(evalNormalParse(`
            (L3 (define loop (lambda () (loop)))
                (define f (lambda (x y z) (if (= x 1) y z)))
                (f 1 2 (loop)))`)).to.equal(2);

        expect(evalNormalParse(`
        (L3 (define loop (lambda (x) (loop x)))
            (define g (lambda (x) 5))
            (g (loop 0)))`)).to.equal(5);
    });

    it('evaluates programs which would give an error in applicative order, but not in normal order', () => {
        expect(evalNormalParse(`
            (L3 (define try
                  (lambda (a b)
                    (if (= a 0)
                        1
                        b)))
                (try 0 (/ 1 0)))`)).to.equal(1);
    });

    it('evaluates programs which would cause side-effects in applicative order, but not in normal order', () => {
        expect(evalNormalParse(`
            (L3 (define f (lambda (x) (display x) (newline) (+ x 1)))
                (define g (lambda (x) 5))
                (g (f 0)))`)).to.equal(5);
    });
});
