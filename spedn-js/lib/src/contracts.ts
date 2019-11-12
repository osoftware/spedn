import { Address, BITBOX, TREST_URL, TWS_URL } from "bitbox-sdk";
import { AddressUtxoResult } from "bitcoin-com-rest";
import { dropRight, flatten, fromPairs, mapValues, reverse, toPairs, zipWith } from "lodash/fp";

export interface PortableModule {
  types: { [name: string]: ParamType };
  templates: { [name: string]: Template };
}

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

export interface Arr {
  tag: "Array";
  contents: [ParamType, number];
}

export interface List {
  tag: "List";
  contents: [ParamType];
}

export interface Tuple {
  tag: "Tuple";
  contents: ParamType[];
}

export type ParamType = "bool" | "int" | "bit" | Arr | List | Tuple | string;

function isArr(arr: ParamType): arr is Arr {
  return (arr as Arr).tag === "Array";
}

function isList(list: ParamType): list is List {
  return (list as List).tag === "List";
}

function isTuple(tuple: ParamType): tuple is Tuple {
  return (tuple as Tuple).tag === "Tuple";
}

export type ParamValue = number | boolean | string | Buffer | any[];

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

export class ContractCoin implements Coin {
  constructor(public utxo: Utxo, public challenges: Challenges, public redeemScript: Buffer) {}
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

export interface Module {
  [name: string]: Contract;
}

export const bitbox: { [net: string]: BITBOX } = {
  mainnet: new BITBOX(),
  testnet: new BITBOX({ restURL: TREST_URL, wsURL: TWS_URL })
};
export const addr = {
  mainnet: bitbox.mainnet.Address,
  testnet: bitbox.testnet.Address
} as { [net: string]: Address };
export const crypto = bitbox.mainnet.Crypto;
export const script = bitbox.mainnet.Script;

export const stdlib: PortableModule = {
  templates: {},
  types: {
    DataSig: { tag: "Array", contents: ["byte", 64] },
    PubKey: { tag: "Array", contents: ["byte", 33] },
    Ripemd160: { tag: "Array", contents: ["byte", 20] },
    Sha1: { tag: "Array", contents: ["byte", 16] },
    Sha256: { tag: "Array", contents: ["byte", 32] },
    Sig: { tag: "Array", contents: ["byte", 65] },
    Time: "int",
    TimeSpan: "int"
  }
};

export class ModuleFactory {
  constructor(private mod: PortableModule) {}

  makeParams = (astParams: string[][]) => fromPairs(astParams.map(dropRight(1)).map(reverse)) as ParamTypes;

  typeMatches(spednType: ParamType, arg: ParamValue): boolean {
    if (spednType === "bool") return typeof arg === "boolean";

    if (spednType === "int") return typeof arg === "number" && Math.trunc(arg) === arg;

    if (isArr(spednType)) {
      const [subType, length] = spednType.contents;
      switch (subType) {
        case "byte":
          return arg instanceof Buffer && arg.length === length;
        case "bit":
          return arg instanceof Buffer && arg.length <= 3 || typeof arg === "number" && Math.trunc(arg) === arg;
        default:
          return arg instanceof Array && arg.length === length && arg.every(a => this.typeMatches(subType, a));
      }
    }
    if (isList(spednType)) {
      const [subType] = spednType.contents;
      if (subType === "byte") return arg instanceof Buffer;
      else throw Error("invalid type");
    }
    if (isTuple(spednType)) {
      return (
        arg instanceof Array &&
        arg.length === spednType.contents.length &&
        zipWith(this.typeMatches, spednType.contents, arg).every(v => v)
      );
    }

    const alias = this.mod.types[spednType];
    return alias ? this.typeMatches(alias, arg) : false;
  }

  validateParamValues(values: ParamValues, types: ParamTypes) {
    toPairs(types).forEach(([n, t]) => {
      if (values[n] === undefined) throw TypeError(`Missing parameter: ${t} ${n}`);
      if (!this.typeMatches(t, values[n])) throw TypeError(`Incorrect value for ${t} ${n}`);
    });
  }

  asmToScript(asm: Op[], paramValues: ParamValues): RedeemScript {
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
          const [name, index] = op.contents.split("$");
          let val = paramValues[name];
          if (val instanceof Array) val = val[parseInt(index, 10)];
          return this.encodeParam(val);
        default:
          return ops[op.tag];
      }
    });
    return script.encode(chunks);
  }

  encodeParam(value: ParamValue): Buffer {
    if (typeof value === "boolean") return script.encodeNumber(value ? 1 : 0);
    if (typeof value === "number") return script.encodeNumber(value);
    if (value instanceof Buffer) return value;

    throw TypeError("Invalid parameter type.");
  }

  encodeArrayParam(value: ParamValue[]): Buffer[] {
    return value.map(this.encodeParam);
  }

  makeChallengeFunc(types: ParamTypes, redeemScript: Buffer, i: number): Challenge {
    return (params: ParamValues) => {
      this.validateParamValues(params, types);
      const argStack = flatten(
        Object.keys(types).map((n: string) => {
          const p = params[n];
          if (p instanceof Array) return this.encodeArrayParam(p);
          else return this.encodeParam(p);
        })
      );
      if (i > 0) argStack.push(this.encodeParam(i));
      argStack.push(this.encodeParam(redeemScript));

      return script.encode(argStack);
    };
  }

  makeChallenges(astChallenges: any[], redeemScript: Buffer) {
    const multiChallenge = astChallenges.length !== 1;

    const challengeSpecs: ChallengeSpecs = {};
    const challenges: Challenges = {};

    let i = multiChallenge ? 1 : 0;
    for (const [name, args] of astChallenges) {
      challengeSpecs[name] = this.makeParams(args);
      challenges[name] = this.makeChallengeFunc(challengeSpecs[name], redeemScript, i);
      i++;
    }

    return { challengeSpecs, challenges };
  }

  makeContractClass(template: Template): Contract {
    const ast = template.ast;
    const that = this;

    const Class = class implements Instance {
      static params: ParamTypes = that.makeParams(ast.contractParams);

      redeemScript: RedeemScript;
      challengeSpecs: ChallengeSpecs;
      challenges: Challenges;

      constructor(public paramValues: ParamValues) {
        that.validateParamValues(paramValues, Class.params);
        this.redeemScript = that.asmToScript(template.asm, paramValues);
        const defs = that.makeChallenges(ast.contractChallenges, this.redeemScript);
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

  make(): Module {
    return mapValues(this.makeContractClass.bind(this), this.mod.templates);
  }
}
