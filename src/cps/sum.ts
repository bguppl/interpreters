// recursiveToIterative.ts
// Michael Elhadad - 31 May 2018
// Illustratrate how to transform a recursive function
// into an iterative one in a systematic transformation.
// 1. Convert the recursive function to CPS form
// 2. Convert the Continuation implementation from procedural to concrete data structures.
// 3. Registerization: convert all procedures in tail-position to zero-parameter procedures
//    with parameters passed in shared registers.
// 4. Iterative: transform all tail calls to a "GOTO" - with a virtual machine dispatching
//    function names to zero parameter / void procedures with register update side-effects.


// ============================================================
// regular recursive function: sum all numbers from 0 to n.
export const sum = (n: number): number =>
    (n === 0) ? 0 :
    n + sum(n - 1);

// iterative algorithm - this is a different implementation
// Our transformation does **not** generate this function -
// We generate a tail-call-optimized version of the recursive
// function instead.
export const sumIter = (n: number, acc: number): number =>
    (n === 0) ? acc :
    sumIter(n - 1, n + acc);

// ============================================================
// CPS Procedural transformation of the recursive algorithm
type Cont = (x: number) => number;

export const sumCPS = (n: number, cont: Cont): number =>
    (n === 0) ? cont(0) :
    sumCPS(n - 1, (sn1) => cont(n + sn1));

export const sumCPS1 = (n: number): number =>
    sumCPS(n, (x) => x);

// =============================================================
// CPS Concrete transformation of the CPS procedural implementation
// Continuations are implemented as data structures instead of procedures.

type CCont = IdCont | Cont1;

export interface Cont1 {tag: "Cont1"; n: number; cont: CCont}
export const makeCont1 = (n: number, cont: CCont): Cont1 => ({tag: "Cont1", n: n, cont: cont});
export const isCont1 = (x: any): x is Cont1 => x.tag === "Cont1";

export interface IdCont {tag: "IdCont"}
export const makeIdCont = (): IdCont => ({tag: "IdCont"});
export const isIdCont = (x: any): x is IdCont => x.tag === "IdCont";

export const applyCont = (cont: CCont, val: number): number =>
    isIdCont(cont) ? val :
    isCont1(cont) ? applyCont(cont.cont, cont.n + val) :
    -1;

export const sumCPSC = (n: number, cont: CCont): number =>
    (n === 0) ? applyCont(cont, 0) :
    sumCPSC(n - 1, makeCont1(n, cont));

export const sumCPS2 = (n: number): number =>
    sumCPSC(n, makeIdCont());

// =============================================================
// CPS Registerization transformation of the CPS concrete implementation
// Continuations are implemented as data structures and all procedure calls
// in tail position are without parameters - parameters are passed through registers.
// All the procedures in the implementation are defined in the scope of the "registers".
// We need one register for each possible argument type passed to one of the functions
// involved in the implementation of the CPS transformed system. In our case:
// applyCont: cont, val
// sumCPSC: n, cont
// (We could unify n and val since they have the same type - we'll keep them distinct for simplicity).
// We still invoke procedures in call position - so that this implementation in TypeScript
// *consumes* stack memory (because TS does not implement tail-call-optimization).
const sumREG1VM = (nREG: number, contREG: CCont, valREG: number): number => {
    const sumCPSCReg = (): number => {
        // console.log(`sumCPCReg n=${nREG} cont=${JSON.stringify(contREG)}`)
        if (nREG === 0) {
            valREG = 0;
            return applyContReg();
        } else {
            contREG = makeCont1(nREG, contREG);
            nREG = nREG - 1;
            return sumCPSCReg();
        }
    }

    const applyContReg = (): number => {
        // console.log(`applyContReg val=${valREG} cont=${JSON.stringify(contREG)}`)
        if (isIdCont(contREG)) {
            return valREG;
        } else if (isCont1(contREG)) {
            valREG = contREG.n + valREG;
            contREG = contREG.cont;
            return applyContReg();
        } else {
            console.error(`Bad continuation ${contREG}`);
            return -1;
        }
    }
    // entry point
    return sumCPSCReg();
}

// Bootstrap the virtual machine
export const sumREG1 = (n: number): number =>
    sumREG1VM(n, makeIdCont(), 0);

// =============================================================
// Iterative CPS transformation of the Registerized CPS version.
// Tail calls with no parameters are transformed into "GOTO" by
// explicitly representing the names of the procedures as "Instructions"
// dispatched by an instruction set virtual machine.
// Instead of calling a zero-parameter procedure, we set pcREG to the
// name of the procedure to be invoked.
// The VM() procedure dispatches the pcREG to the call of the appropriate
// procedure in a typical "fetch / decode / execute" loop.

// The set of instructions known by the "Sum Virtual Machine"
type InstructionSet = 'applyContReg2' | 'sumCPSCReg2' | 'halt';

// All procedures in the implementation are defined in the scope of the registers.
// We have one additional register for the "Program Counter" pcREG whose value
// corresponds to the instruction being executed.  To invoke a new procedure, we
// end each call by setting pcREG to the name of the procedure we want to execute next.
// Setting pcREG in tail position is equivalent to invoking a procedure in tail position.
// In this implementation - contREG is equivalent to the execution stack of the program.
// We manipulate the stack explicitly.
const sumREG2VM = (nREG2: number, contREG2: CCont, valREG2: number, pcREG2: InstructionSet): number => {
    const sumCPSCReg2 = (): void => {
        // console.log(`sumCPCReg n=${nREG2} cont=${JSON.stringify(contREG2)}`)
        if (nREG2 === 0) {
            valREG2 = 0;
            pcREG2 = 'applyContReg2';
        } else {
            contREG2 = makeCont1(nREG2, contREG2);
            nREG2 = nREG2 - 1;
            pcREG2 = 'sumCPSCReg2';
        }
    }

    const applyContReg2 = (): void => {
        // console.log(`applyContReg val=${valREG2} cont=${JSON.stringify(contREG2)}`)
        if (isIdCont(contREG2)) {
            pcREG2 = 'halt';
            // return valREG;
        } else if (isCont1(contREG2)) {
            valREG2 = contREG2.n + valREG2;
            contREG2 = contREG2.cont;
            pcREG2 = 'applyContReg2';
        } else {
            pcREG2 = 'halt';
        }
    }

    const VM = (): void => {
        while (pcREG2 !== 'halt') {
            if (pcREG2 === 'sumCPSCReg2') sumCPSCReg2();
            else if (pcREG2 === 'applyContReg2') applyContReg2();
            else {
                console.error(`Bad instruction ${pcREG2}`);
                pcREG2 = 'halt';
            }
        }
    }

    // entry point
    VM();
    return valREG2;
}

export const sumREG2 = (n: number): number =>
    sumREG2VM(n, makeIdCont(), 0, 'sumCPSCReg2');

// =============================================================
// TESTS
// sum(10000);     // Stack overflow
// sumCPS1(10000); // Stack overflow
// sumCPS2(10000); // Stack overflow
// sumREG1(10000); // Stack overflow
console.log(sumREG2(10000)); // 100 010 000 / 2 = 50 005 000
