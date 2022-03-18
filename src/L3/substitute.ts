import { elem, filter, findIndex, map, unzip, zip } from "fp-ts/ReadonlyArray";
import { match } from "fp-ts/Option";
import { pipe } from "fp-ts/function";
import * as S from "fp-ts/string";
import { CExp, ProcExp, VarDecl, VarRef } from "./L3-ast";
import { isAppExp, isBoolExp, isIfExp, isLitExp, isNumExp, isPrimOp, isProcExp, isStrExp, isVarRef } from "./L3-ast";
import { makeAppExp, makeIfExp, makeProcExp, makeVarDecl, makeVarRef } from "./L3-ast";

// For applicative eval - the type of exps should be ValueExp[] | VarRef[];
// where ValueExp is an expression which directly encodes a value:
// export type ValueExp = LitExp | NumExp | BoolExp | StrExp | PrimOp;
// In order to support normal eval as well - we generalize the types to CExp.
// @Pre: vars and exps have the same length
export const substitute = (body: readonly CExp[], vars: readonly string[], exps: readonly CExp[]): readonly CExp[] => {
    const subVarRef = (e: VarRef): CExp => {
        const pos = findIndex(v => v === e.var)(vars);
        return pipe(
            pos,
            match(
                () => e,
                i => exps[i]
            )
        );
    };
    
    const subProcExp = (e: ProcExp): ProcExp => {
        const argNames = pipe(e.args, map(x => x.var));
        const subst = zip(vars, exps);
        const includes = elem(S.Eq);
        const freeSubst = pipe(subst, filter((ve) => !includes(ve[0], argNames)));
        const [newVars, newExps] = unzip(freeSubst);
        return makeProcExp(e.args, substitute(e.body, newVars, newExps));
    };
    
    const sub = (e: CExp): CExp => isNumExp(e) ? e :
        isBoolExp(e) ? e :
        isPrimOp(e) ? e :
        isLitExp(e) ? e :
        isStrExp(e) ? e :
        isVarRef(e) ? subVarRef(e) :
        isIfExp(e) ? makeIfExp(sub(e.test), sub(e.then), sub(e.alt)) :
        isProcExp(e) ? subProcExp(e) :
        isAppExp(e) ? pipe(sub(e.rator), rator => pipe(e.rands, map(sub), rands => makeAppExp(rator, rands))) :
        e;
    
    return pipe(body, map(sub));
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
export const renameExps = (exps: readonly CExp[]): readonly CExp[] => {
    const varGen = makeVarGen();
    const replace = (e: CExp): CExp =>
        isIfExp(e) ? makeIfExp(replace(e.test), replace(e.then), replace(e.alt)) :
        isAppExp(e) ?  pipe(replace(e.rator), rator => pipe(e.rands, map(replace), rands => makeAppExp(rator, rands))) :
        isProcExp(e) ? replaceProc(e) :
        e;
    
    // Rename the params and substitute old params with renamed ones.
    //  First recursively rename all ProcExps inside the body.
    const replaceProc = (e: ProcExp): ProcExp => {
        const oldArgs = pipe(e.args, map(arg => arg.var));
        const newArgs = pipe(oldArgs, map(varGen));
        const newBody = pipe(e.body, map(replace));
        return makeProcExp(map(makeVarDecl)(newArgs), substitute(newBody, oldArgs, map(makeVarRef)(newArgs)));
    };
    
    return pipe(exps, map(replace));
};
