import { readLines } from '../lib/io';
import { splitOn, sum } from '../lib/functional'

export async function main(input: string) {
    let lines = await readLines(input);
    let data = splitOn(lines, '');
    let cals = data.map(arr => sum(arr.map(Number)));
    let m = Math.max(...cals);
    console.log("Elf carrying the most calories carries: %d", m);
    cals.sort((a, b) => b - a);
    let t3 = sum(cals.slice(0, 3));
    console.log("Total of top 3 elf with most calories amounts to: %d", t3);
}