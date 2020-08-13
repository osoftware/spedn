declare module "varuint-bitcoin" {
  interface IEncode {
    (_number: number, buffer: Buffer, offset: number): Buffer;
    bytes: number;
  }
  const encode: IEncode;
  function decode(buffer: Buffer, offset: number): number;
  function encodingLength(_number: number): number;
}

declare module "@chris.troutner/bch-js" {
  export class BCHJS {
    constructor();
    constructor(conf: any);
    Address: any;
    Crypto: any;
    Script: any;
    Mnemonic: any;
    HDNode: any;
    RawTransactions: any;
    TransactionBuilder: any;
  }
  export const TREST_URL: string;
  export const TWS_URL: string;
  export interface Address {
    utxo(addr: any): any;
    fromOutputScript(script: any, network: any);
    toHash160(addr: any);
  }
  export class Crypto {
    hash256(buffer: any);
    sha256(buffer: any);
  }
  export class TransactionBuilder {
    constructor(network: any);
    addInputScripts(scripts: any);
    addInput(txid: any, vout: number, sequence?: number);
    addOutput(scriptPubKey: any, amount: number);
    setLockTime(locktime: any);
    build();
    transaction: any;
  }
  export default BCHJS;
}

declare module "bitcoincashjs-lib" {
  export interface HDNode {
    keyPair: any;
    getAddress(): any;
    isNeutered(): any;
    getIdentifier(): any;
    verify(buffer: any, signature: any): any;
    deriveHardened(path: any): any;
    sign(buffer: any): any;
    toBase58(): any;
    neutered(): any;
    getPublicKeyBuffer(): any;
    derivePath(path: any): any;
    derive(path: any): any;
  }

  export interface ECPair {
    toWIF(): string;
    sign(buffer: Buffer, signatureAlgorithm?: number): boolean | ECSignature;
    verify(buffer: Buffer, signature: ECSignature): boolean;
    getPublicKeyBuffer(): Buffer;
    getAddress(): string;
  }

  export type ECSignature = any;

  export interface scriptNumber {
    encode(number: number): Buffer;
    decode(buffer: Buffer, maxLength?: number, minimal?: boolean): number;
  }

  export interface Script {
    number: scriptNumber;
    pubKey: any;
    pubKeyHash: any;
    scriptHash: any;
  }

  export const script: Script;
}

declare module "bitcoin-com-rest" {
  export interface AddressUtxoResult {
    legacyAddress: string;
    cashAddress: string;
    scriptPubKey: string;
    utxos: utxo[];
  }
}
