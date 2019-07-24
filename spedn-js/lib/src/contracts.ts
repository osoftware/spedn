import { Address, BITBOX, tresturl } from "bitbox-sdk";
import { AddressUtxoResult } from "bitcoin-com-rest";
import { dropRight, fromPairs, nth, reverse, toPairs } from "lodash/fp";

export interface Template {
  asm: Op[];
  ast: {
    contractName: string;
    contractParams: string[][];
    contractChallenges: any[];
  };
}

export interface Op {
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

export type RedeemScript = Buffer;
export type ScriptSig = Buffer;
export type ScriptPubKey = Buffer;

export type Challenge = (params: ParamValues) => ScriptSig;

export interface ChallengeSpecs {
  [name: string]: ParamTypes;
}

export interface Challenges {
  [name: string]: Challenge;
}

export interface Utxo {
  txid: string;
  vout: number;
  amount: number;
  satoshis: number;
  height: number;
  confirmations: number;
}

export interface Coin {
  challenges: Challenges;
  redeemScript: Buffer;
  utxo: Utxo;
}

export interface Instance {
  paramValues: ParamValues;
  challengeSpecs: ChallengeSpecs;
  redeemScript: RedeemScript;
  getAddress(network: string): string;
  findCoins(network: string): Promise<Coin[]>;
}

export interface Contract {
  params: ParamTypes;
  new (params: ParamValues): Instance;
}

export const bitbox = {
  mainnet: new BITBOX(),
  testnet: new BITBOX({ restURL: tresturl })
} as { [net: string]: BITBOX };
export const addr = {
  mainnet: bitbox.mainnet.Address,
  testnet: bitbox.testnet.Address
} as { [net: string]: Address };
export const crypto = bitbox.mainnet.Crypto;
export const script = bitbox.mainnet.Script;

const defParams = (astParams: string[][]) => fromPairs(astParams.map(dropRight(1)).map(reverse)) as ParamTypes;

function typeMatches(spednType: ParamType, arg: ParamValue): boolean {
  switch (spednType) {
    case "bool":
      return typeof arg === "boolean";
    case "int":
    case "Time":
    case "TimeSpan":
      return typeof arg === "number" && Math.trunc(arg) === arg;
    case "bin":
      return arg instanceof Buffer;
    case "Sig":
      return arg instanceof Buffer && arg.length === 65;
    case "DataSig":
      return arg instanceof Buffer && arg.length === 64;
    case "PubKey":
      return arg instanceof Buffer && (arg.length === 33 || arg.length === 65);
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

export function validateParamValues(values: ParamValues, types: ParamTypes) {
  toPairs(types).forEach(([n, t]) => {
    if (values[n] === undefined) throw TypeError(`Missing parameter: ${t} ${n}`);
    if (!typeMatches(t, values[n])) throw TypeError(`Incorrect value for ${t} ${n}`);
  });
}

function asmToScript(asm: Op[], paramValues: ParamValues): RedeemScript {
  const ops = script.opcodes as any;
  const chunks = asm.map(op => {
    switch (op.tag) {
      case "OP_N":
        return script.encodeNumber(op.contents);
      case "OP_PUSHDATA0":
      case "OP_PUSHDATA1":
      case "OP_PUSHDATA2":
      case "OP_PUSHDATA4":
        const buf = Buffer.alloc(op.contents[0]);
        (op.contents[1] as number[]).forEach((v, i) => buf.writeUInt8(v, i));
        return buf;
      case "OP_PUSH":
        return encodeParam(paramValues[op.contents]);
      default:
        return ops[op.tag];
    }
  });
  return script.encode(chunks);
}

export function encodeParam(value: ParamValue) {
  if (typeof value === "boolean") return script.encodeNumber(value ? 1 : 0);
  if (typeof value === "number") return script.encodeNumber(value);
  if (value instanceof Buffer) return value;
  throw TypeError("Invalid parameter type.");
}

function makeChallengeFunc(types: ParamTypes, args: any, redeemScript: Buffer, i: number): Challenge {
  return (params: ParamValues) => {
    validateParamValues(params, types);

    const argStack = args.map(nth(1)).map((n: string) => params[n]).map(encodeParam);
    if (i > 0) argStack.push(encodeParam(i));
    argStack.push(encodeParam(redeemScript));

    return script.encode(argStack);
  };
}

function makeChallenges(astChallenges: any[], redeemScript: Buffer) {
  const multiChallenge = astChallenges.length !== 1;

  const challengeSpecs: ChallengeSpecs = {};
  const challenges: Challenges = {};

  let i = multiChallenge ? 1 : 0;
  for (const [name, args] of astChallenges) {
    challengeSpecs[name] = defParams(args);
    challenges[name] = makeChallengeFunc(challengeSpecs[name], args, redeemScript, i);
    i++;
  }

  return { challengeSpecs, challenges };
}

export function makeContractClass(template: Template): Contract {
  const ast = template.ast;

  const Class = class implements Instance {
    static params: ParamTypes = defParams(ast.contractParams);

    redeemScript: RedeemScript;

    challengeSpecs: ChallengeSpecs;
    challenges: Challenges;

    constructor(public paramValues: ParamValues) {
      validateParamValues(paramValues, Class.params);
      this.redeemScript = asmToScript(template.asm, paramValues);
      const defs = makeChallenges(ast.contractChallenges, this.redeemScript);
      this.challengeSpecs = defs.challengeSpecs;
      this.challenges = defs.challenges;
    }

    getAddress(network: string) {
      return addr[network].fromOutputScript(script.encodeP2SHOutput(crypto.hash160(this.redeemScript)), network);
    }

    async findCoins(network: string): Promise<Coin[]> {
      const results = (await addr[network].utxo(this.getAddress(network))) as AddressUtxoResult;
      return results.utxos.map(utxo => new ContractCoin(utxo, this.challenges, this.redeemScript));
    }
  };
  Object.defineProperty(Class, "name", { value: ast.contractName });

  return Class;
}

export class ContractCoin implements Coin {
  constructor(public utxo: Utxo, public challenges: Challenges, public redeemScript: Buffer) {}
}
