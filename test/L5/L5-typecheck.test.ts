import { expect } from 'chai';
import { parseL5Exp, Exp } from '../../src/L5/L5-ast';
import { typeofExp, L5typeof } from '../../src/L5/L5-typecheck';
import { makeEmptyTEnv, makeExtendTEnv } from '../../src/L5/TEnv';
import { makeBoolTExp, makeNumTExp, makeProcTExp, makeTVar, makeVoidTExp, parseTE, unparseTExp } from '../../src/L5/TExp';
import { makeOk, bind } from '../../src/shared/result';
import { parse as p } from "../../src/shared/parser";

describe('L5 Type Checker', () => {
    describe('parseTE', () => {
        it('parses atoms', () => {
            expect(parseTE("number")).to.deep.equal(makeOk(makeNumTExp()));
            expect(parseTE("boolean")).to.deep.equal(makeOk(makeBoolTExp()));
        });

        it('parses type variables', () => {
            expect(parseTE("T1")).to.deep.equal(makeOk(makeTVar("T1")));
        });

        it('parses procedures', () => {
            expect(parseTE("(T * T -> boolean)")).to.deep.equal(makeOk(makeProcTExp([makeTVar("T"), makeTVar("T")], makeBoolTExp())));
            expect(parseTE("(number -> (number -> number))")).to.deep.equal(makeOk(makeProcTExp([makeNumTExp()], makeProcTExp([makeNumTExp()], makeNumTExp()))));
        });

        it('parses "void" and "Empty"', () => {
            expect(parseTE("void")).to.deep.equal(makeOk(makeVoidTExp()));
            expect(parseTE("(Empty -> void)")).to.deep.equal(makeOk(makeProcTExp([], makeVoidTExp())));
        });
    });

    describe('unparseTExp', () => {
        it('unparses atoms', () => {
            expect(unparseTExp(makeNumTExp())).to.deep.equal(makeOk("number"));
            expect(unparseTExp(makeBoolTExp())).to.deep.equal(makeOk("boolean"));
        });

        it('unparses type variables', () => {
            expect(unparseTExp(makeTVar("T1"))).to.deep.equal(makeOk("T1"));
        });

        it('unparses procedures', () => {
            expect(unparseTExp(makeProcTExp([makeTVar("T"), makeTVar("T")], makeBoolTExp()))).to.deep.equal(makeOk("(T * T -> boolean)"));
            expect(unparseTExp(makeProcTExp([makeNumTExp()], makeProcTExp([makeNumTExp()], makeNumTExp())))).to.deep.equal(makeOk("(number -> (number -> number))"));
        });
    });

    describe('L5typeof', () => {
        it('returns the types of atoms', () => {
            expect(L5typeof("5")).to.deep.equal(makeOk("number"));
            expect(L5typeof("#t")).to.deep.equal(makeOk("boolean"));
        });

        it('returns the type of primitive procedures', () => {
            expect(L5typeof("+")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(L5typeof("-")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(L5typeof("*")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(L5typeof("/")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(L5typeof("=")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(L5typeof("<")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(L5typeof(">")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(L5typeof("not")).to.deep.equal(makeOk("(boolean -> boolean)"));
        });

        it("returns the type of primitive op applications", () => {
            expect(L5typeof("(+ 1 2)")).to.deep.equal(makeOk("number"));
            expect(L5typeof("(- 1 2)")).to.deep.equal(makeOk("number"));
            expect(L5typeof("(* 1 2)")).to.deep.equal(makeOk("number"));
            expect(L5typeof("(/ 1 2)")).to.deep.equal(makeOk("number"));

            expect(L5typeof("(= 1 2)")).to.deep.equal(makeOk("boolean"));
            expect(L5typeof("(< 1 2)")).to.deep.equal(makeOk("boolean"));
            expect(L5typeof("(> 1 2)")).to.deep.equal(makeOk("boolean"));

            expect(L5typeof("(not (< 1 2))")).to.deep.equal(makeOk("boolean"));
        });

        it.skip('type checking of generic functions is not supported', () => {
            // All of these fail in TypeCheck because we do not support generic functions
            // They do work in Type Inference.
            expect(L5typeof("(eq? 1 2)")).to.deep.equal(makeOk("boolean"));
            expect(L5typeof('(string=? "a" "b")')).to.deep.equal(makeOk("boolean"));
            expect(L5typeof('(number? 1)')).to.deep.equal(makeOk("boolean"));
            expect(L5typeof('(boolean? "a")')).to.deep.equal(makeOk("boolean"));
            expect(L5typeof('(string? "a")')).to.deep.equal(makeOk("boolean"));
            expect(L5typeof('(symbol? "a")')).to.deep.equal(makeOk("boolean"));
            expect(L5typeof('(list? "a")')).to.deep.equal(makeOk("boolean"));
            expect(L5typeof('(pair? "a")')).to.deep.equal(makeOk("boolean"));
        });

        it('returns the type of a VarRef in a given TEnv', () => {
            expect(bind(bind(p("x"), parseL5Exp), (exp: Exp) => typeofExp(exp, makeExtendTEnv(["x"], [makeNumTExp()], makeEmptyTEnv())))).to.deep.equal(makeOk(makeNumTExp()));
        });

        it('returns the type of "if" expressions', () => {
            expect(L5typeof("(if (> 1 2) 1 2)")).to.deep.equal(makeOk("number"));
            expect(L5typeof("(if (= 1 2) #t #f)")).to.deep.equal(makeOk("boolean"));
        });

        it('returns the type of procedures', () => {
            expect(L5typeof("(lambda ((x : number)) : number x)")).to.deep.equal(makeOk("(number -> number)"));
            expect(L5typeof("(lambda ((x : number)) : boolean (> x 1))")).to.deep.equal(makeOk("(number -> boolean)"));
            expect(L5typeof("(lambda((x : number)) : (number -> number) (lambda((y : number)) : number (* y x)))")).to.deep.equal(makeOk("(number -> (number -> number))"));
            expect(L5typeof("(lambda((f : (number -> number))) : number (f 2))")).to.deep.equal(makeOk("((number -> number) -> number)"));
            expect(L5typeof("(lambda((x : number)) : number (let (((y : number) x)) (+ x y)))")).to.deep.equal(makeOk("(number -> number)"));
        });

        it('returns the type of "let" expressions', () => {
            expect(L5typeof("(let (((x : number) 1)) (* x 2))")).to.deep.equal(makeOk("number"));
            expect(L5typeof("(let (((x : number) 1) ((y : number) 3)) (+ x y))")).to.deep.equal(makeOk("number"));
            expect(L5typeof("(let (((x : number) 1) ((y : number) 2)) (lambda((a : number)) : number (+ (* x a) y)))")).to.deep.equal(makeOk("(number -> number)"));
        });

        it('returns the type of "letrec" expressions', () => {
            expect(L5typeof("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) p1)")).to.deep.equal(makeOk("(number -> number)"));
            expect(L5typeof("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) (p1 2))")).to.deep.equal(makeOk("number"));
            expect(L5typeof(`
                (letrec (((odd? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #f (even? (- n 1)))))
                         ((even? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #t (odd? (- n 1))))))
                  (odd? 12))`)).to.deep.equal(makeOk("boolean"));
        });

        it('returns "void" as the type of "define" expressions', () => {
            expect(L5typeof("(define (foo : number) 5)")).to.deep.equal(makeOk("void"));
            expect(L5typeof("(define (foo : (number * number -> number)) (lambda((x : number) (y : number)) : number (+ x y)))")).to.deep.equal(makeOk("void"));
            expect(L5typeof("(define (x : (Empty -> number)) (lambda () : number 1))")).to.deep.equal(makeOk("void"));
        });

        it.skip('returns "literal" as the type for literal expressions', () => {
            expect(L5typeof("(quote ())")).to.deep.equal(makeOk("literal"));
        });

        describe.skip('Pairs', () => {
            it('returns the pair type for "cons" applications', () => {
                expect(L5typeof("(cons 1 '())")).to.deep.equal(makeOk("(Pair number literal)"));
                expect(L5typeof("(cons 1 1)")).to.deep.equal(makeOk("(Pair number number)"));
            });
    
            it('returns the correct type for applications of "car" and "cdr" on pairs', () => {
                expect(L5typeof("(car (cons 1 1))")).to.deep.equal(makeOk("number"));
                expect(L5typeof("(cdr (cons 1 #t))")).to.deep.equal(makeOk("boolean"));
                expect(L5typeof("(cdr (cons (cons 1 2) (cons 1 2)))")).to.deep.equal(makeOk("(Pair number number)"));
                expect(L5typeof("(cdr (cons (cons 1 2) (cons 1 #f)))")).to.deep.equal(makeOk("(Pair number boolean)"));
                expect(L5typeof("(car (cons (cons 1 2) (cons 1 #f)))")).to.deep.equal(makeOk("(Pair number number)"));
                expect(L5typeof("(car (cons (cons (cons #t #t) 2) (cons 1 #f)))")).to.deep.equal(makeOk("(Pair (Pair boolean boolean) number)"));
                expect(L5typeof("(cdr (cons (cons (cons #t #t) 2) (cons 1 #f)))")).to.deep.equal(makeOk("(Pair number boolean)"));
            });
            
            it('returns the correct type for procedures that return pairs', () => {
                expect(L5typeof("(lambda((a : number) (b : number)) : (Pair number number) (cons a b))")).to.deep.equal(makeOk("(number * number -> (Pair number number))"));
            });
    
            it('returns the correct type for procedures that take pairs as arguments', () => {
                expect(L5typeof("(lambda((a : number) (b : (Pair number boolean))) : (Pair number (Pair number boolean)) (cons a b))")).to.deep.equal(makeOk("(number * (Pair number boolean) -> (Pair number (Pair number boolean)))"));
            });

            it('returns the correct type for procedures that take and return pairs', () => {
                expect(L5typeof(`(lambda ((a : (Pair number number))
                                          (b : (Pair number boolean))) : (Pair (Pair number number) (Pair (Pair number number) (Pair number boolean)))
                                   (cons a (cons a b)))`)).to.deep.equal(makeOk("((Pair number number) * (Pair number boolean) -> (Pair (Pair number number) (Pair (Pair number number) (Pair number boolean))))"));
            });

            it('returns "void" when defining pairs', () => {
                expect(L5typeof("(define (x : (Pair number boolean)) (cons 1 #t))")).to.deep.equal(makeOk("void"));
                expect(L5typeof("(define (x : (Pair (T1 -> T1) number)) (cons (lambda ((y : T1)) : T1 y) 2))")).to.deep.equal(makeOk("void"));
            });
        });

        it('returns the type of polymorphic procedures', () => {
            expect(L5typeof("(lambda((x : T1)) : T1 x)")).to.deep.equal(makeOk("(T1 -> T1)"));
            expect(L5typeof(`(let (((x : number) 1))
                                         (lambda((y : T) (z : T)) : T
                                           (if (> x 2) y z)))`)).to.deep.equal(makeOk("(T * T -> T)"));
            expect(L5typeof("(lambda () : number 1)")).to.deep.equal(makeOk("(Empty -> number)"));
            expect(L5typeof(`(define (x : (T1 -> (T1 -> number)))
                                         (lambda ((x : T1)) : (T1 -> number)
                                           (lambda((y : T1)) : number 5)))`)).to.deep.equal(makeOk("void"));
        });
    });
});
