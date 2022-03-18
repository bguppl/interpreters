import * as E from "fp-ts/Either";
import { pipe } from "fp-ts/function";
import { makeNumExp, parseL3Exp, unparseL3, parseL3, Exp } from '../../src/L3/L3-ast';
import { makeVarDecl, makeVarRef } from '../../src/L3/L3-ast';
import { isAppExp, isBoolExp, isCExp, isDefineExp, isIfExp, isLetExp, isLitExp, isNumExp, isPrimOp,
         isProcExp, isProgram, isStrExp, isVarDecl, isVarRef } from '../../src/L3/L3-ast';
import { evalParse, evalL3program } from '../../src/L3/L3-eval';
import { listPrim } from "../../src/L3/evalPrimitive";
import { renameExps, substitute } from "../../src/L3/substitute";
import { makeClosure, makeCompoundSExp, makeEmptySExp, makeSymbolSExp } from '../../src/L3/L3-value';
import { isEnv, makeEnv, makeEmptyEnv, applyEnv } from '../../src/L3/L3-env';
import { parse as p } from "../../src/shared/parser";
import { isRightT } from "../shared/test-helpers";

describe('L3 Environment', () => {
    it('applies the environment correctly', () => {
        const emptyEnv = makeEmptyEnv();
        const env1 = makeEnv("a", 1, emptyEnv);
        const env2 = makeEnv("b", 2, env1);
        const env3 = makeEnv("a", 3, env2);
    
        expect(isEnv(emptyEnv)).toBe(true);
        expect(isEnv(env1)).toBe(true);
        expect(applyEnv(env1, "a")).toEqual(E.of(1));
        expect(applyEnv(env2, "a")).toEqual(E.of(1));
        expect(applyEnv(env2, "b")).toEqual(E.of(2));
        expect(applyEnv(env3, "a")).toEqual(E.of(3));
        expect(applyEnv(env3, "b")).toEqual(E.of(2));
    });
});

describe('L3 Parsing', () => {
    it('parses atomic expressions', () => {
        expect(pipe(p("1"), E.chain(parseL3Exp))).toSatisfy(isRightT(isNumExp));
        expect(pipe(p("#t"), E.chain(parseL3Exp))).toSatisfy(isRightT(isBoolExp));
        expect(pipe(p("x"), E.chain(parseL3Exp))).toSatisfy(isRightT(isVarRef));
        expect(pipe(p('"a"'), E.chain(parseL3Exp))).toSatisfy(isRightT(isStrExp));
        expect(pipe(p(">"), E.chain(parseL3Exp))).toSatisfy(isRightT(isPrimOp));
        expect(pipe(p("="), E.chain(parseL3Exp))).toSatisfy(isRightT(isPrimOp));
        expect(pipe(p("string=?"), E.chain(parseL3Exp))).toSatisfy(isRightT(isPrimOp));
        expect(pipe(p("eq?"), E.chain(parseL3Exp))).toSatisfy(isRightT(isPrimOp));
        expect(pipe(p("cons"), E.chain(parseL3Exp))).toSatisfy(isRightT(isPrimOp));
    });

    it('parses programs', () => {
        expect(parseL3("(L3 (define x 1) (> (+ x 1) (* x x)))")).toSatisfy(isRightT(isProgram));
    });

    it('parses define expressions', () => {
        const parsed = pipe(p("(define x 1)"), E.chain(parseL3Exp));
        expect(parsed).toSatisfy(isRightT(isDefineExp));
        if (isRightT(isDefineExp)(parsed)) {
            expect(parsed.right.var).toSatisfy(isVarDecl);
            expect(parsed.right.val).toSatisfy(isNumExp);
        }
    });

    it('parses applications', () => {
        expect(pipe(p("(> x 1)"), E.chain(parseL3Exp))).toSatisfy(isRightT(isAppExp));
        expect(pipe(p("(> (+ x x) (* x x))"), E.chain(parseL3Exp))).toSatisfy(isRightT(isAppExp));
    });

    it('parses "if" expressions', () => {
        expect(pipe(p("(if #t 1 2)"), E.chain(parseL3Exp))).toSatisfy(isRightT(isIfExp));
        expect(pipe(p("(if (< x 2) x 2)"), E.chain(parseL3Exp))).toSatisfy(isRightT(isIfExp));
    });

    it('parses procedures', () => {
        expect(pipe(p("(lambda () 1)"), E.chain(parseL3Exp))).toSatisfy(isRightT(isProcExp));
        expect(pipe(p("(lambda (x) x x)"), E.chain(parseL3Exp))).toSatisfy(isRightT(isProcExp));
    });

    it('parses "let" expressions', () => {
        expect(pipe(p("(let ((a 1) (b #t)) (if b a (+ a 1)))"), E.chain(parseL3Exp))).toSatisfy(isRightT(isLetExp));
    });

    it('parses literal expressions', () => {
        expect(pipe(p("'a"), E.chain(parseL3Exp))).toSatisfy(isRightT(isLitExp));
        expect(pipe(p("'()"), E.chain(parseL3Exp))).toSatisfy(isRightT(isLitExp));
        expect(pipe(p("'(1)"), E.chain(parseL3Exp))).toSatisfy(isRightT(isLitExp));
        expect(pipe(p("'(1 . 2)"), E.chain(parseL3Exp))).toSatisfy(isRightT(isLitExp));
        expect(pipe(p("'(1 . (2 . 3))"), E.chain(parseL3Exp))).toSatisfy(isRightT(isLitExp));
    });

    it('returns an error for an invalid literal', () => {
        expect(pipe(p("'(1 . 2 3)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
    });

    describe("Failures", () => {
        it("returns a Failure when parsing a single-token program", () => {
            expect(parseL3("x")).toSatisfy(E.isLeft);
        });

        it("returns a Failure when parsing an empty program", () => {
            expect(parseL3("")).toSatisfy(E.isLeft);
        });

        it("returns a Failure if the program does not start with (L3 ...)", () => {
            expect(parseL3("(+ 1 2)")).toSatisfy(E.isLeft);
        });

        it("returns a Failure for a program with no Exps", () => {
            expect(parseL3("(L3)")).toSatisfy(E.isLeft);
        });
    
        it("returns a Failure if a program has an empty Exp", () => {
            expect(parseL3("(L3 ())")).toSatisfy(E.isLeft);
        });

        it('returns a Failure for an ill-formed "define"', () => {
            expect(pipe(p("(define)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(define x)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(define x y z)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p('(define "1" y)'), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p('(define 1 y)'), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
        });

        it("returns a Failure for an empty CExp", () => {
            expect(pipe(p("(+ ())"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
        });

        it("returns a Failure for an ill-formed special form", () => {
            expect(pipe(p("(if)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(if 1)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(lambda x x)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(let x x)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(let (x y) x)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
            expect(pipe(p("(let ((1 y)) x)"), E.chain(parseL3Exp))).toSatisfy(E.isLeft);
        });
    });
});

describe('L3 Unparse', () => {
    const roundTrip: (x: string) => E.Either<string, string> =
        x => pipe(
            p(x),
            E.chain(parseL3Exp),
            E.map(unparseL3)
        );

    it("doesn't change concrete values", () => {
        const concretes = ["1", "#t", "x", '"a"', ">", "=", "string=?", "eq?", "cons"];
        concretes.forEach(concrete => {
            expect(roundTrip(concrete)).toEqual(E.of(concrete));
        });
    });

    it('unparses a program', () => {
        const program = "(L3 (define x 1) (> (+ x 1) (* x x)))";
        const result = pipe(parseL3(program), E.map(unparseL3));
        expect(result).toEqual(E.of(program));
    });

    it('unparses "define" expressions', () => {
        const define = "(define x 1)";
        expect(roundTrip(define)).toEqual(E.of(define));
    });

    it('unparses applications', () => {
        const app1 = "(> x 1)";
        const app2 = "(> (+ x x) (* x x))";

        expect(roundTrip(app1)).toEqual(E.of(app1));
        expect(roundTrip(app2)).toEqual(E.of(app2));
    });

    it('unparses "if" expressions', () => {
        const if1 = "(if #t 1 2)";
        const if2 = "(if (< x 2) x 2)";
        expect(roundTrip(if1)).toEqual(E.of(if1));
        expect(roundTrip(if2)).toEqual(E.of(if2));
    });

    it('unparses procedures', () => {
        const proc1 = "(lambda () 1)";
        const proc2 = "(lambda (x) x x)";
        expect(roundTrip(proc1)).toEqual(E.of(proc1));
        expect(roundTrip(proc2)).toEqual(E.of(proc2));
    });

    it('unparses "let" expressions', () => {
        const let1 = "(let ((a 1) (b #t)) (if b a (+ a 1)))";
        expect(roundTrip(let1)).toEqual(E.of(let1));
    });

    it('unparses literal expressions', () => {
        const lits = ["'a", "'()", "'(1)", "'(1 . 2)", "'(1 2 . 3)"];
        lits.forEach(lit => {
            expect(roundTrip(lit)).toEqual(E.of(lit));
        });
    });

    it('normalizes dotted pairs', () => {
        expect(roundTrip("'(1 . (2 . 3))")).toEqual(E.of("'(1 2 . 3)"));
    });
});

describe('L3 Eval', () => {
    it('evaluates data type literals', () => {
        expect(evalParse("1")).toEqual(E.of(1));
        expect(evalParse("#t")).toEqual(E.of(true));
        expect(evalParse("#f")).toEqual(E.of(false));
        expect(evalParse("'a")).toEqual(E.of(makeSymbolSExp("a")));
        expect(evalParse('"a"')).toEqual(E.of("a"));
        expect(evalParse("'()")).toEqual(E.of(makeEmptySExp()));
        expect(evalParse("'(1 2)")).toEqual(E.of(listPrim([1, 2])));
        expect(evalParse("'(1 (2))")).toEqual(E.of(listPrim([1, listPrim([2])])));
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
            expect(evalParse("(cons 1 '())")).toEqual(E.of(listPrim([1])));
            expect(evalParse("(cons 1 '(2))")).toEqual(E.of(listPrim([1, 2])));
        });

        it('evaluates "car"', () => {
            expect(evalParse("(car '(1 2))")).toEqual(E.of(1));
        });

        it('evaluates "cdr"', () => {
            expect(evalParse("(cdr '(1 2))")).toEqual(E.of(listPrim([2])));
            expect(evalParse("(cdr '(1))")).toEqual(E.of(makeEmptySExp()));
        });

        it('evaluates "pair?"', () => {
            expect(evalParse("(pair? '(1))")).toEqual(E.of(true));
            expect(evalParse("(pair? '())")).toEqual(E.of(false));
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

    it('evaluates dotted pairs', () => {
        expect(evalParse("(cons 1 2)")).toEqual(E.of(makeCompoundSExp(1, 2)));
        expect(evalParse("(car (cons 1 2))")).toEqual(E.of(1));
        expect(evalParse("(cdr (cons 1  2))")).toEqual(E.of(2));
        expect(evalParse("(pair? (cons 1  2))")).toEqual(E.of(true));
        expect(evalParse("(car '(1 . 2))")).toEqual(E.of(1));
        expect(evalParse("(cdr '(1 . 2))")).toEqual(E.of(2));
        expect(evalParse("(car (cdr '(1 . (2 . 3))))")).toEqual(E.of(2));
        expect(evalParse("(cdr (cdr '(1 . (2 . 3))))")).toEqual(E.of(3));
        expect(evalParse("(car (cdr (list 1 2 3 4)))")).toEqual(E.of(2));
    });

    it('evaluates programs with "define"', () => {
        const program1 = parseL3("(L3 (define x 1) (+ x x))");
        if (E.isRight(program1)) {
            expect(evalL3program(program1.right)).toEqual(E.of(2));
        }

        const program2 = parseL3("(L3 (define x 1) (define y (+ x x)) (* y y))");
        if (E.isRight(program2)) {
            expect(evalL3program(program2.right)).toEqual(E.of(4));
        }
    });

    it('evaluates "if"', () => {
        expect(evalParse('(if (string? "a") 1 2)')).toEqual(E.of(1));
        expect(evalParse('(if (not (string? "a")) 1 2)')).toEqual(E.of(2));
    });

    it('evaluates procedures', () => {
        expect(evalParse("(lambda (x) x)")).toEqual(E.of(makeClosure([makeVarDecl("x")], [makeVarRef("x")])));
    });

    it('substitutes', () => {
        const cexp1 = pipe(p("((lambda (x) (* x x)) x)"), E.chain(parseL3Exp));
        const expected1 = pipe(p("((lambda (x) (* x x)) 3)"), E.chain(parseL3Exp));
        if (isRightT(isCExp)(cexp1) && E.isRight(expected1)) {
            expect(substitute([cexp1.right], ["x"], [makeNumExp(3)])).toEqual([expected1.right]);
        }

        const e1 = `
            ((if (= n 0)
                 (lambda (x) x)
                 (if (= n 1)
                     f
                     (lambda (x) (f ((nf f (- n 1)) x)))))
             '(f n))`;
        const vn = pipe(p("2"), E.chain(parseL3Exp));
        const vf = pipe(p("(lambda (x) (* x x))"), E.chain(parseL3Exp));
        const e2 = `
            ((if (= 2 0)
                 (lambda (x) x)
                 (if (= 2 1)
                     (lambda (x) (* x x))
                     (lambda (x) ((lambda (x) (* x x)) ((nf (lambda (x) (* x x)) (- 2 1)) x)))))
             '(f n))`;
        const parsed1 = pipe(p(e1), E.chain(parseL3Exp));
        const parsed2 = pipe(p(e2), E.chain(parseL3Exp));
        if (isRightT(isCExp)(parsed1) &&
            isRightT(isCExp)(parsed2) &&
            isRightT(isCExp)(vn) &&
            isRightT(isCExp)(vf)) {
            expect(substitute([parsed1.right], ["n", "f"], [vn.right, vf.right])).toEqual([parsed2.right]);
        }

        const lzxz = pipe(p("(lambda (z) (x z))"), E.chain(parseL3Exp));
        const lwzw = pipe(p("(lambda (w) (z w))"), E.chain(parseL3Exp));
        const lzlwzwz = pipe(p("(lambda (z) ((lambda (w) (z w)) z))"), E.chain(parseL3Exp));
        if (isRightT(isCExp)(lzxz) && isRightT(isCExp)(lwzw) && E.isRight(lzlwzwz)) {
            expect(substitute([lzxz.right], ["x"], [lwzw.right])).toEqual([lzlwzwz.right]);
        }
    });

    it('renames', () => {
        const lxx = pipe(p("(lambda (x) x)"), E.chain(parseL3Exp));
        const lx1x1 = pipe(p("(lambda (x__1) x__1)"), E.chain(parseL3Exp));
        if (isRightT(isCExp)(lxx) && isRightT(isCExp)(lx1x1)) {
            expect(renameExps([lxx.right])).toEqual([lx1x1.right]);
        }
        const l1 = pipe(p(`(((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2)`), E.chain(parseL3Exp));
        const rl1 = pipe(p(`(((lambda (x__1) (lambda (z__2) (x__1 z__2))) (lambda (w__3) (z w__3))) 2)`), E.chain(parseL3Exp));
        if (isRightT(isCExp)(l1) && isRightT(isCExp)(rl1)) {
            expect(renameExps([l1.right])).toEqual([rl1.right]);
        }
    });

    it('applies procedures', () => {
        expect(evalParse("((lambda (x) (* x x)) 2)")).toEqual(E.of(4));
        const program1 = parseL3("(L3 (define square (lambda (x) (* x x))) (square 3))");
        if (E.isRight(program1)) {
            expect(evalL3program(program1.right)).toEqual(E.of(9));
        }
        const program2 = parseL3("(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3))");
        if (E.isRight(program2)) {
            expect(evalL3program(program2.right)).toEqual(E.of(3));
        }
    });

    it('applies recursive procedures', () => {
        const program = parseL3("(L3 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3))");
        if (E.isRight(program)) {
            expect(evalL3program(program.right)).toEqual(E.of(6));
        }
    });

    it('applies higher-order functions', () => {
        // L3 higher order functions

        expect(pipe(parseL3(`
        (L3 (define map
              (lambda (f l)
                (if (eq? l '())
                    l
                    (cons (f (car l)) (map f (cdr l))))))
            (map (lambda (x) (* x x)) '(1 2 3)))`), E.chain(evalL3program))).toEqual(E.of(listPrim([1, 4, 9])));

        expect(pipe(parseL3(`
            (L3 (define empty? (lambda (x) (eq? x '())))
                (define filter
                  (lambda (pred l)
                    (if (empty? l)
                        l
                        (if (pred (car l))
                            (cons (car l) (filter pred (cdr l)))
                            (filter pred (cdr l))))))
                (filter (lambda (x) (not (= x 2))) '(1 2 3 2)))`), E.chain(evalL3program))).toEqual(E.of(listPrim([1, 3])));

        expect(pipe(parseL3(`
            (L3 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                ((compose not number?) 2))`), E.chain(evalL3program))).toEqual(E.of(false));

        expect(pipe(parseL3(`
            (L3 (define equal? (lambda (e1 e2)
                                 (if (eq? e1 e2)
                                     #t
                                     (if (and (pair? e1) (pair? e2))
                                         (and (equal? (car e1) (car e2)) (equal? (cdr e1) (cdr e2)))
                                         #f))))
                (and (equal? '(1 . (2 . 3)) '(1 2 . 3)) (equal? '(1 . (2)) '(1 2))))`), E.chain(evalL3program))).toEqual(E.of(true));
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
                ((nf (lambda (x) (* x x)) 2) 3))`), E.chain(evalL3program))).toEqual(E.of(81));

        // Accidental capture of the z variable if no renaming
        expect(pipe(parseL3(`
            (L3 (define z (lambda (x) (* x x)))
                (((lambda (x) (lambda (z) (x z))) (lambda (w) (z w))) 2))`), E.chain(evalL3program))).toEqual(E.of(4));

        // Y-combinator
        expect(pipe(parseL3(`
            (L3 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1))))))) 6))`), E.chain(evalL3program))).toEqual(E.of(720));
    });
});
