import * as rl from 'readline'
import * as fs from 'fs';
import {EventEmitter} from 'events';

export async function readLines(input: string): Promise<Array<string>> {
    let reader = rl.createInterface(fs.createReadStream(input));
    let lines: Array<string> = [];
    reader.on('line', (l: string) => {
        lines.push(l);
    });
    
    await EventEmitter.once(reader, 'close');
    return lines;
}