# Advent of Code 2022

Nodejs + Typescript solution to https://adventofcode.com/2022.

The approach taken is building a command line tool that is able to load the
module corresponding to each day by request. Such interface would be able to
run both test mode and with puzzle input data. This tool will evolve and
further options could be added to it as problems require.

The code is meant to be written in a very high level, creating abstractions
whenever deems fit and trying to explore language features and limitations.

### Requirements

* Nodejs

### Setup

```
npm install
```

### Command Line Interface

```
% npx ts-node advent.ts --help
Usage: advent [options]

Solutions to https://adventofcode.com/2022

Options:
  -V, --version  output the version number
  -d, --day <n>  Executes solution for day n
  -t, --test     Execute with test input (default: false)
  -h, --help     display help for command
  ```

### Running Day 1

  ```
  % npx ts-node advent.ts -d 1
  ```
