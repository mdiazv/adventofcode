import { program, InvalidArgumentError } from 'commander';

function parseDay(value: string, _: number) {
    console.log('parsing day');
    let n = Number(value)
    if (isNaN(n) || n < 1 || n > 25) {
        throw new InvalidArgumentError('n should be a number in range [1, 25]');
    }
    return n;
} 

program
    .version('0.0.1')
    .description('Solutions to https://adventofcode.com/2022')
    .allowUnknownOption(false)
    .allowExcessArguments(false)
    .requiredOption('-d, --day <n>', 'Executes solution for day n', parseDay)
    .option('-t, --test', 'Execute with test input', false);

program.parse();
const options = program.opts();
console.log(options);

async function main() {
    let day = options.day
    let module = './'+day+'/solve';
    let input = options.test ? './'+day+'/test.in' : './'+day+'/in';

    console.log("Importing solution for day "+day);
    try {
        const solve = await import(module);
        await solve.main(input);
    } catch (e) {
        console.log(e);
    }
}

main().then(text => {}, err => {});