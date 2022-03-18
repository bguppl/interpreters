import { map } from "fp-ts/ReadonlyArray";
import { pipe } from "fp-ts/function";
import { AppExp, CExp, Exp, LetExp, Program }  from "./L3-ast";
import { isAppExp, isAtomicExp, isCExp, isDefineExp, isExp, isIfExp, isLetExp, isLitExp,
         isProcExp, isProgram }  from "./L3-ast";
import { makeAppExp, makeDefineExp, makeIfExp, makeProcExp, makeProgram } from "./L3-ast";

/*
Purpose: rewrite a single LetExp as a lambda-application form
Signature: rewriteLet(cexp)
Type: [LetExp => AppExp]
*/
const rewriteLet = (e: LetExp): AppExp => {
    const vars = pipe(e.bindings, map(b => b.var));
    const vals = pipe(e.bindings, map(b => b.val));
    return makeAppExp(makeProcExp(vars, e.body), vals);
}

/*
Purpose: rewrite all occurrences of let in an expression to lambda-applications.
Signature: rewriteAllLet(exp)
Type: [Program | Exp -> Program | Exp]
*/
export const rewriteAllLet = (exp: Program | Exp): Program | Exp =>
    isExp(exp) ? rewriteAllLetExp(exp) :
    isProgram(exp) ? pipe(exp.exps, map(rewriteAllLetExp), makeProgram) :
    exp;

const rewriteAllLetExp = (exp: Exp): Exp =>
    isCExp(exp) ? rewriteAllLetCExp(exp) :
    isDefineExp(exp) ? makeDefineExp(exp.var, rewriteAllLetCExp(exp.val)) :
    exp;

const rewriteAllLetCExp = (exp: CExp): CExp =>
    isAtomicExp(exp) ? exp :
    isLitExp(exp) ? exp :
    isIfExp(exp) ? makeIfExp(rewriteAllLetCExp(exp.test),
                             rewriteAllLetCExp(exp.then),
                             rewriteAllLetCExp(exp.alt)) :
    isAppExp(exp) ? makeAppExp(rewriteAllLetCExp(exp.rator),
                               map(rewriteAllLetCExp)(exp.rands)) :
    isProcExp(exp) ? makeProcExp(exp.args, map(rewriteAllLetCExp)(exp.body)) :
    isLetExp(exp) ? rewriteAllLetCExp(rewriteLet(exp)) :
    exp;
