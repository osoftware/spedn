import { fromPairs, dropRight, reverse } from "lodash/fp";
import { Script } from "bitbox-sdk";

export interface Template {
  Left: any[];
  Right: {
    ast: {
      contractParams: string[][];
      contractChallenges: any[];
    };
    asm: Op[];
  };
}

interface Op {
  tag: string;
  contents: any;
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
  script: Buffer;
}

const bitcoinScript = new Script();

const defParams = (astParams: string[][]) => fromPairs(astParams.map(dropRight(1)).map(reverse));

function typeMatches(spednType: ParamType, arg: ParamValue): boolean {
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

function asmToBuffer(asm: Op[], paramValues: ParamValues): Buffer {
  const ops = bitcoinScript.opcodes as any;
  const chunks = asm.map(op => {
    switch (op.tag) {
      case "OP_N":
        return bitcoinScript.encodeNumber(op.contents);
      case "OP_PUSHDATA0":
      case "OP_PUSHDATA1":
      case "OP_PUSHDATA2":
      case "OP_PUSHDATA4":
        const buf = Buffer.alloc(op.contents[0]);
        (op.contents[1] as number[]).forEach((v, i) => buf.writeUInt8(v, i));
        return buf;
      case "OP_PUSH":
        const value = paramValues[op.contents];
        if (typeof value === "boolean") return bitcoinScript.encodeNumber(value ? 1 : 0);
        if (typeof value == "number") return bitcoinScript.encodeNumber(value);
        if (value instanceof Buffer) return value;
        throw TypeError("Invalid parameter type.");
      default:
        return ops[op.tag];
    }
  });
  return bitcoinScript.encode(chunks);
}

export function makeContractClass(template: Template): Contract {
  if (template.Left) throw template.Left;
  const ast = template.Right.ast;

  const challenges: Challenges = {};
  for (const [name, args] of ast.contractChallenges) {
    const f: any = () => ({} as Tx);
    f.params = defParams(args);
    challenges[name] = f;
  }

  const Class = class implements Instance {
    static params: ParamTypes = defParams(ast.contractParams);

    challenges: Challenges;
    script: Buffer;

    constructor(public paramValues: ParamValues) {
      for (const name in Class.params) {
        const t = Class.params[name];
        if (paramValues[name] === undefined) throw TypeError(`Missing parameter: ${t} ${name}`);
        if (!typeMatches(t, paramValues[name])) throw TypeError(`Incorrect value for ${t} ${name}`);
      }
      this.script = asmToBuffer(template.Right.asm, this.paramValues);
      this.challenges = challenges;
    }
  };

  return Class;
}
