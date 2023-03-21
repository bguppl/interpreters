// lazy-tree.ts
// Michael Elhadad - June 2020
// A lazy tree has a root, and a generator that generates the direct children of the root (a finite number of children).
// It can represent infinite trees (those that include infinite depth paths).
// The key constructor is expandLZT
// Compile with: tsc ./lzt.ts --downlevelIteration --strict  

import { chain } from 'ramda';

export const cons = <T>(x: T, xs: T[]): T[] => [x, ...xs];
export const first = <T>(x: T[]): T => x[0];
export const rest = <T>(x: T[]): T[] => x.slice(1);
const isEmpty = (x: any): boolean => x.length === 0;

export type LazyTree<Node> = EmptyLZT | LZT<Node>;

export type EmptyLZT = {
    tag: "EmptyLZT";
};
export const makeEmptyLZT = (): EmptyLZT => ({tag: "EmptyLZT"});
export const isEmptyLZT = (x: any): x is EmptyLZT => x.tag === "EmptyLZT";

// Low level representation of LZT
export type LZT<Node> = {
    tag: "LZT";
    root: Node;
    children$: Generator<LazyTree<Node>>;
};
export const makeLZT = <Node>(root: Node, children$: Generator<LazyTree<Node>>): LZT<Node> => ({tag: "LZT", root, children$});
export const isLZT = <Node>(x: any): x is LZT<Node> => x.tag === "LZT";

export const take = <Node>(gen: Generator<Node>, n: number): Array<Node> => {
    let res : Array<Node> = [];
    let next = gen.next();
    while (!next.done && n-- > 0) {
        res.push(next.value);
        next = gen.next();
    }
    return res;
};

// Like take til end of generator
export const genToList = <T>(g: Generator<T>): T[] => {
    const next = g.next();
    return  next.done ? [] :
            cons(next.value, genToList(g));
}

// lztChildren forces the evaluation of the children iterator
export const lztChildren = <Node>(lzt: LazyTree<Node>): Array<LazyTree<Node>> =>
    isEmptyLZT(lzt) ? [] :
    genToList(lzt.children$);

// Map from array to generator 
export function* map$<T1, T2>(f: (x: T1) => T2, a: Array<T1>): Generator<T2> {
    if (isEmpty(a)) return [];
    yield f(first(a));
    yield* map$(f, rest(a));
};

// This is the key LZT constructor: given a function that computes the direct children of a node (expander)
// return the transitive closure of the children relation as a potentially infinite LZT.
export const expandLZT = <Node>(root: Node, expander: (node: Node)=>Array<Node>): LazyTree<Node> =>
    makeLZT(root, map$((child: Node) => expandLZT(child, expander), expander(root)));


// ==============================================
// LZT Traversals and transformers

// Traverse tree in-order depth first - only works on finite trees
export const lztDfs = <Node>(lzt: LazyTree<Node>): Array<Node> =>
    isEmptyLZT(lzt) ? [] :
    cons(lzt.root, chain(lztDfs, lztChildren(lzt)));

// Maps a finite LZT (fully expands it)
export const lztMap = <Node1, Node2>(f: (n: Node1) => Node2, lzt: LazyTree<Node1>): LazyTree<Node2> =>
    isEmptyLZT(lzt) ? lzt :
    makeLZT(f(lzt.root), 
            map$((child: LazyTree<Node1>) => lztMap(f, child), lztChildren(lzt)));

// Filters a finite LZT (fully expands it)
export const lztFilter = <Node>(pred: (n: Node)=>boolean, lzt: LazyTree<Node>): Array<Node> =>
    isEmptyLZT(lzt) ? [] :
    pred(lzt.root) ? cons(lzt.root, chain((child: LazyTree<Node>) => lztFilter(pred, child), lztChildren(lzt))) :
    chain((child : LazyTree<Node>) => lztFilter(pred, child), lztChildren(lzt));

// =============================================================
// The following functions can operate over infinite LZTs

// Find the first node in lzt in depth-first traversal that satisfies filter - false if not found.
// Can loop forever if there is no node that satisfies pred on an infinite path in the DFS.
export const lztFindFirst = <Node>(pred: (n: Node)=>boolean, lzt: LazyTree<Node>): Node | false => {
    const collect = (lzt: LazyTree<Node>): Node | false => 
        isEmptyLZT(lzt) ? false :
        pred(lzt.root) ? lzt.root :
        findFirstInTrees(lztChildren(lzt));
    const findFirstInTrees = (trees: Array<LazyTree<Node>>): Node | false =>
        isEmpty(trees) ? false :
        checkFirstTree(collect(first(trees)), rest(trees));
    const checkFirstTree = (n: Node | false, trees: LazyTree<Node>[]): Node | false =>
        n ? n :
        findFirstInTrees(trees);
    return collect(lzt);
}

export function* lztFilter$<Node>(pred: (n: Node) => boolean, lzt: LazyTree<Node>): Generator<Node> {
    function* collectInTrees$(trees: Array<LazyTree<Node>>): Generator<Node> {
        if (isEmpty(trees))
            return [];
        yield* lztFilter$(pred, first(trees));
        yield* collectInTrees$(rest(trees));
    }
    if (isEmptyLZT(lzt))
        return [];
    if (pred(lzt.root))
        yield lzt.root;
    yield* collectInTrees$(lztChildren(lzt));
}

// In order traversal of a lzt as a generator
export function* lztInOrder$<Node>(lzt: LazyTree<Node>): Generator<Node> {
    function* traverseTrees$(trees: Array<LazyTree<Node>>): Generator<Node> {
        if (isEmpty(trees))
            return [];
        yield* lztInOrder$(first(trees));
        yield* traverseTrees$(rest(trees));
    }
    if (isEmptyLZT(lzt))
        return [];
    yield lzt.root;
    yield* traverseTrees$(lztChildren(lzt));
}

// Breadth-first traversal of a lzt as a generator
export function* lztBFS$<Node>(lzt: LazyTree<Node>): Generator<Node> {
    function* traverseTrees$(trees: Array<LazyTree<Node>>): Generator<Node> {
        if (isEmpty(trees))
            return [];
        for (const t of trees) {
            if (isLZT(t)) yield t.root;
        }
        yield* traverseTrees$(chain(lztChildren, trees));
    }
    if (isEmptyLZT(lzt))
        return [];
    yield* traverseTrees$([lzt]);
}


// ============================================
// Examples

// Tests for take / genToList
function* g1() {
    yield 1;
    yield 2;
}
console.log(take(g1(), 1));
console.log(take(g1(), 2));
console.log(take(g1(), 3));
console.log(genToList(g1()));

// Example: A finite lazy-tree 0 1 1 2 2 2 2 3 3 3 3 3 3 3 3 ...
const t1: (limit: number) => LazyTree<number> = (limit: number) => 
    expandLZT(0, (node: number) => node < limit ? [node+1, node+1] : []);

// Example: An infinite lazy-tree
const t2: () => LazyTree<number> = () => expandLZT(0, (node: number) => [node+1, node+1]);

// Only works on finite trees
console.log(lztDfs(t1(2)));
console.log(lztDfs(lztMap((x:number) => x+1, t1(2))));
const isEven = (n: number): boolean => (n % 2) === 0;
console.log(lztFilter(isEven, t1(2)));

console.log(lztFindFirst(isEven, t1(2)));
console.log(lztFindFirst((n) => n > 10, t2()));

console.log(take(lztFilter$(isEven, t1(2)), 10));
console.log(take(lztFilter$(isEven, t2()), 10));

console.log(take(lztInOrder$(t1(2)), 10));
console.log(take(lztInOrder$(t2()), 10));

console.log(take(lztBFS$(t1(2)), 10));
console.log(take(lztBFS$(t2()), 10));
