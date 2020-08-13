
export abstract class Rts {
  constructor(public readonly network: string = "mainnet") {}
  abstract utxo(addr: any): any;
  abstract ecPair(ecPair: any): RtsECPair;
  abstract get addresses(): Addresses;
  abstract get crypto(): Crypto;
  abstract get script(): Script;
  abstract transactionBuilder(): RtsTransactionBuilder;
  abstract sendTx(tx: any): Promise<string>;
}

export interface Addresses {
  fromOutputScript(script: Buffer, network: string): string;
  toHash160(addr: string): string;
}

export interface RtsECPair {
  sign(buffer: Buffer, signatureAlgorithm?: number): boolean | ECSignature;
  verify(buffer: Buffer, signature: ECSignature): boolean;
  getPublicKeyBuffer(): Buffer;
  getAddress(): string;
}

export type ECSignature = any;

export interface Crypto {
  hash160(buffer: any): Buffer;
  hash256(buffer: any): Buffer;
  sha256(buffer: any): Buffer;
}

export interface Script {
  encode(argStack: Buffer[]): Buffer;
  encodeNumber(integer: number): Buffer;
  decodeNumber(buffer: Buffer, maxLength?: number, minimal?: boolean): number;
  encodeScriptHashOutput(buffer: Buffer): Buffer;
  encodePubKeyHashOutput(buffer: Buffer): Buffer;
  encodePubKeyHashInput(sig: Buffer, pubKey: Buffer): Buffer;
  opcodes: any;
}

export interface RtsTransactionBuilder {
  addInputScripts(scripts: any): RtsTransactionBuilder;
  addInput(txid: any, vout: number, sequence?: number): RtsTransactionBuilder;
  addOutput(scriptPubKey: any, amount: number): RtsTransactionBuilder;
  setLockTime(locktime: any): RtsTransactionBuilder;
  build(): any;
  transaction: any;
}
