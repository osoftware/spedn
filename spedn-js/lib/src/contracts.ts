import { fromPairs, dropRight, reverse, zip, zipWith, toPairs } from "lodash/fp";
import { Script, opcodes } from "bitbox-sdk";

export interface Template {
  Left: any[];
  Right: {
    ast: {
      contractParams: string[][];
      contractChallenges: any[];
    };
    asm: any[];
  };
}

export type ParamType =
  | "bool"
  | "int"
  | "bin"
  | "PubKey"
  | "Sha1"
  | "Sha256"
  | "Ripemd160"
  | "Sig"
  | "DataSig"
  | "Time"
  | "TimeSpan";

export type ParamValue = number | boolean | string | Buffer;

export interface ParamTypes {
  [name: string]: ParamType;
}

export interface ParamValues {
  [name: string]: ParamValue;
}

export interface Contract {
  new (params: ParamValues): Instance;
  params: ParamTypes;
}

export interface Challenge {
  (params: ParamValues): Tx;
  params: ParamTypes;
}

export interface Challenges {
  [name: string]: Challenge;
}

export interface Tx {}

export interface Instance {
  paramValues: ParamValues;
  challenges: Challenges;
}

const defParams = (astParams: string[][]) => fromPairs(astParams.map(dropRight(1)).map(reverse));

function typeMatches(spednType: ParamType, arg: ParamValue) {
  switch (spednType) {
    case "bool":
      return typeof arg === "boolean";
    case "int":
    case "Time":
    case "TimeSpan":
      return typeof arg === "number" && Math.trunc(arg) === arg;
    case "bin":
    case "Sig":
    case "DataSig":
    case "PubKey":
      return arg instanceof Buffer;
    case "Sha1":
      return arg instanceof Buffer && arg.length === 16;
    case "Sha256":
      return arg instanceof Buffer && arg.length === 32;
    case "Ripemd160":
      return arg instanceof Buffer && arg.length === 20;
    default:
      return false;
  }
}

export function makeContractClass(template: Template): Contract {
  if (template.Left) throw template.Left;
  const ast = template.Right.ast;

  const challenges: Challenges = {};
  for (const [name, args] of ast.contractChallenges) {
    const f: any = (params: ParamValues) => ({} as Tx);
    f.params = defParams(args);
    challenges[name] = f;
  }

  const ops = new Script().opcodes;
  const script = template.Right.asm.map(op => {
    switch (op) {
      default:
        return ops.OP_0;
    }
  });

  const contract = class implements Instance {
    constructor(public paramValues: ParamValues) {
      for (const name in contract.params) {
        const t = contract.params[name];
        if (paramValues[name] === undefined) throw TypeError(`Missing parameter: ${t} ${name}`);
        if (!typeMatches(t, paramValues[name])) throw TypeError(`Incorrect value for ${t} ${name}`);
      }
    }
    static params: ParamTypes = defParams(ast.contractParams);
    challenges = challenges;
    script = script;
  };

  return contract;
}
