import { filter, indexOf, map, contains, zip, KeyValuePair } from "ramda";
import { CExp, ProcExp, VarDecl, VarRef } from "./L3-ast";
import { isAppExp, isBoolExp, isIfExp, isLitExp, isNumExp, isPrimOp, isProcExp, isStrExp, isVarRef } from "./L3-ast";
import { makeAppExp, makeIfExp, makeProcExp, makeVarDecl, makeVarRef } from "./L3-ast";
import { first } from '../shared/list';

// For applicative eval - the type of exps should be ValueExp[] | VarRef[];
// where ValueExp is an expression which directly encodes a value:
// export type ValueExp = LitExp | NumExp | BoolExp | StrExp | PrimOp;
// In order to support normal eval as well - we generalize the types to CExp.
// @Pre: vars and exps have the same length
export const substitute = (body: CExp[], vars: string[], exps: CExp[]): CExp[] => {
    const subVarRef = (e: VarRef): CExp => {
        const pos = indexOf(e.var, vars);
        return ((pos > -1) ? exps[pos] : e);
    };
    
    const subProcExp = (e: ProcExp): ProcExp => {
        const argNames = map((x) => x.var, e.args);
        const subst = zip(vars, exps);
        const freeSubst = filter((ve) => !contains(first(ve), argNames), subst);
        return makeProcExp(e.args, substitute(e.body, map((x: KeyValuePair<string, CExp>) => x[0], freeSubst), map((x: KeyValuePair<string, CExp>) => x[1], freeSubst)));
    };
    
    const sub = (e: CExp): CExp => isNumExp(e) ? e :
        isBoolExp(e) ? e :
        isPrimOp(e) ? e :
        isLitExp(e) ? e :
        isStrExp(e) ? e :
        isVarRef(e) ? subVarRef(e) :
        isIfExp(e) ? makeIfExp(sub(e.test), sub(e.then), sub(e.alt)) :
        isProcExp(e) ? subProcExp(e) :
        isAppExp(e) ? makeAppExp(sub(e.rator), map(sub, e.rands)) :
        e;
    
    return map(sub, body);
};
/*
    Purpose: create a generator of new symbols of the form v__n
    with n incremented at each call.
*/
export const makeVarGen = (): (v: string) => string => {
    let count: number = 0;
    return (v: string) => {
        count++;
        return `${v}__${count}`;
    };
};
/*
Purpose: Consistently rename bound variables in 'exps' to fresh names.
         Start numbering at 1 for all new var names.
*/
export const renameExps = (exps: CExp[]): CExp[] => {
    const varGen = makeVarGen();
    const replace = (e: CExp): CExp =>
        isIfExp(e) ? makeIfExp(replace(e.test), replace(e.then), replace(e.alt)) :
        isAppExp(e) ? makeAppExp(replace(e.rator), map(replace, e.rands)) :
        isProcExp(e) ? replaceProc(e) :
        e;
    
    // Rename the params and substitute old params with renamed ones.
    //  First recursively rename all ProcExps inside the body.
    const replaceProc = (e: ProcExp): ProcExp => {
        const oldArgs = map((arg: VarDecl): string => arg.var, e.args);
        const newArgs = map(varGen, oldArgs);
        const newBody = map(replace, e.body);
        return makeProcExp(map(makeVarDecl, newArgs), substitute(newBody, oldArgs, map(makeVarRef, newArgs)));
    };
    
    return map(replace, exps);
};
