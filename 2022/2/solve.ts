import { readLines } from "../lib/io";
import { sum } from '../lib/functional'

abstract class Move {
    abstract get value(): number;
    static parse(c: string): Move {
        switch (c) {
            case 'A': case 'X': return new Rock;
            case 'B': case 'Y': return new Paper;
            case 'C': case 'Z': return new Scissors;
        }
        throw new Error("Unreachable statement reached");
    }
    public equals(other: Move) {
        return this.constructor === other.constructor;
    }
}
class Rock extends Move { value = 1; };
class Paper extends Move { value = 2; };
class Scissors extends Move { value = 3; };

abstract class Outcome {
    abstract get value(): number;
    static parse(c: string): Outcome {
        switch (c) {
            case 'X': return new Lose;
            case 'Y': return new Draw;
            case 'Z': return new Win;
        }
        throw new Error("Unreachable statement reached");
    }
    public equals(other: Move) {
        return this.constructor === other.constructor;
    }
}
class Lose extends Outcome { value = 0; }
class Draw extends Outcome { value = 3; }
class Win extends Outcome { value = 6; }

class Game {
    private rival: Move
    private player: Move;
    constructor(a: Move, b: Move) {
        this.rival = a;
        this.player = b;
    }
    public score(): number {
        let moveScore = this.player.value;
        let outcome = result(this.rival, this.player);
        let gameScore = outcome.value;
        return moveScore + gameScore;
    }
    static parse(line: string): Game {
        let [a, b] = line.split(' ');
        return new Game(Move.parse(a), Move.parse(b));
    }
}

class Instruction {
    private rival: Move
    private outcome: Outcome;
    constructor(a: Move, b: Outcome) {
        this.rival = a;
        this.outcome = b;
    }
    public score(): number {
        let gameScore = this.outcome.value;
        let player = howTo(this.outcome, this.rival);
        let moveScore = player.value;
        return moveScore + gameScore;
    }
    static parse(line: string): Instruction {
        let [a, b] = line.split(' ');
        return new Instruction(Move.parse(a), Outcome.parse(b));
    }
}

function result(a: Move, b: Move): Outcome {
    switch (Math.abs(a.value-b.value)) {
        case 0: return new Draw;
        case 1: return a.value > b.value ? new Lose : new Win;
        case 2: return a.value < b.value ? new Lose : new Win;
    }
    throw new Error("Unreachable statement reached");
}

function howTo(outcome: Outcome, against: Move): Move {
    let moves = [new Rock, new Paper, new Scissors];
    for (let move of moves) {
        if (result(against, move).equals(outcome)) {
            return move;
        }
    }
    throw new Error("Unreachable statement reached");
}

export async function main(input: string) {
    let lines = await readLines(input);
    let games = lines.map(Game.parse);
    let scores = games.map(g => g.score());
    console.log("Resulting score after following strategy: %d", sum(scores));
    let instructions = lines.map(Instruction.parse);
    scores = instructions.map(i => i.score());
    console.log("Resulting score after following strategy correctly: %d", sum(scores));
}