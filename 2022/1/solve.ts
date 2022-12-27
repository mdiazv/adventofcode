import * as rl from 'readline'
import * as fs from 'fs';
import {EventEmitter} from 'events';

let input = '1/test.in';

async function readLines(input: string): Promise<Array<string>> {
    let reader = rl.createInterface(fs.createReadStream(input));
    let lines: Array<string> = [];
    reader.on('line', (l: string) => {
        lines.push(l);
    });
    
    await EventEmitter.once(reader, 'close');
    return lines;
}

type FolderFn<A, B> = (a: A, b: B) => B;

function fold<A, B>(arr: A[], def: B, fn: FolderFn<A, B>): B {
    let _fold = (acc: B, i: number): B => {
        if (i == arr.length) {
            return acc;
        }
        return _fold(fn(arr[i], acc), i+1);
    };
    return _fold(def, 0);
}

function splitOn<T>(arr: T[], delim: T, acc: T[]=[]): Array<T[]> {
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

export async function main(input: string) {
    let lines = await readLines(input);
    let data = splitOn(lines, '');
    console.log(data);
    let cals = data.map(arr => arr.map(Number).reduce((a, b) => a+b));
    let m = Math.max(...cals);
    console.log(cals);
    console.log("Elf carrying the most calories carries: %d", m);
    cals.sort((a, b) => b - a);
    let t3 = cals.slice(0, 3).reduce((a, b) => a+b);
    console.log("Total of top 3 elf with most calories amounts to: %d", t3);
}