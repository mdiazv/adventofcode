export type FolderFn<A, B> = (a: A, b: B) => B;

export function fold<A, B>(arr: A[], def: B, fn: FolderFn<A, B>): B {
    let _fold = (acc: B, i: number): B => {
        if (i == arr.length) {
            return acc;
        }
        return _fold(fn(arr[i], acc), i+1);
    };
    return _fold(def, 0);
}

export function splitOn<T>(arr: T[], delim: T, acc: T[]=[]): Array<T[]> {
    let start: [T[], T[][]] = [[], []];
    let [c, r] = fold(arr, start, (x, [c, r]) => {
        if (x == delim) {
            r.push(c);
            c = [];
        } else {
            c.push(x);
        }
        return [c, r];
    });
    return r.concat([c]);
}

export function chunksOf<T>(arr: T[], k: number): T[][] {
    let r = new Array<T[]>;
    for (let i = 0; i < arr.length; i += k) {
        r.push(arr.slice(i, i+k));
    }
    return r;
}

export function sum(arr: number[]) {
    return arr.reduce((a, b) => a+b);
}