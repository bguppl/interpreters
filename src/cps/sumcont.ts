type Cont = (x: number) => number;

export const sumCPS = (n: number, cont: Cont): number =>
    (n === 0) ? cont(0) :
    sumCPS(n - 1, (sn1) => cont(n + sn1));

// Driver function: same signature as the original function, passes a default cont.
export const sumCPS1 = (n: number): number =>
    sumCPS(n, (x) => x);

sumCPS1(10000)