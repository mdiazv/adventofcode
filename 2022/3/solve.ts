import { readLines } from "../lib/io";
import { sum, chunksOf } from "../lib/functional";

type Rucksack = [string, string];

function priority(x: string) {
    return /[a-z]/.test(x) ? x.charCodeAt(0) - 'a'.charCodeAt(0) + 1
                           : x.charCodeAt(0) - 'A'.charCodeAt(0) + 26 + 1;
}

function common<T extends Iterable<T>>(arrs: T[]): T {
    let intersection = new Set(arrs[0]);
    for (let s of arrs.slice(1)) {
        let ss = new Set(s);
        intersection = new Set([...intersection].filter(x => ss.has(x)));
    }
    return intersection.keys().next().value;
}

function parse(line: string): Rucksack {
    let a = line.substring(0, line.length/2);
    let b = line.substring(line.length/2);
    return [a, b];
}

export async function main(input: string) {
    let lines = await readLines(input); 
    let rucksacks = lines.map(parse);
    let misp = rucksacks.map(common);
    let prio = misp.map(priority);
    console.log('Sum of priorities of misplaced items: %d', sum(prio));
    let chunks = chunksOf(lines, 3);
    let batches = chunks.map(common);
    prio = batches.map(priority);
    console.log('Sum of priorities of required batches: %d', sum(prio));
}