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
