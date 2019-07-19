import { Crypto, TransactionBuilder } from "bitbox-sdk";
import { ECPair, ECSignature } from "bitcoincashjs-lib";
import { castArray } from "lodash/fp";
import varuint from "varuint-bitcoin";
import { Challenges, Coin, ScriptSig } from "./contracts";

export interface SigningContext {
  vin: number;
  preimage(sighashFlag: number): Buffer;
  sign(key: ECPair, sighashFlag: number): Buffer;
  signData(key: ECPair, data: Buffer): Buffer;
}

export enum SigHash {
  SIGHASH_ALL = 0x01,
  SIGHASH_NONE = 0x02,
  SIGHASH_SINGLE = 0x03,
  SIGHASH_FORKID = 0x40,
  SIGHASH_ANYONECANPAY = 0x80
}

const crypto = new Crypto();
const ZERO = Buffer.from("0000000000000000000000000000000000000000000000000000000000000000", "hex");

class P2SHContext implements SigningContext {
  private tx: { ins: any[]; outs: any; version: number; locktime: number };

  constructor(
    private builder: TransactionBuilder,
    public vin: number,
    public satoshis: number,
    public redeemScript: Buffer
  ) {
    this.tx = this.builder.transaction.buildIncomplete();
  }

  sign(key: ECPair, hashType: SigHash = SigHash.SIGHASH_ALL): Buffer {
    hashType = hashType | SigHash.SIGHASH_FORKID;

    const buffer = Buffer.alloc(65);
    key.sign(crypto.hash256(this.preimage(hashType)), 1).copy(buffer);
    buffer.writeUInt8(hashType, 64);

    return buffer;
  }

  signData(key: ECPair, data: Buffer): Buffer {
    return key.sign(crypto.sha256(data));
  }

  preimage(hashType: SigHash) {
    let tbuffer: Buffer;
    let toffset: number;

    const writeSlice = (slice: Buffer) => (toffset += slice.copy(tbuffer, toffset));
    const writeUInt32 = (i: number) => (toffset = tbuffer.writeUInt32LE(i, toffset));
    const writeUInt64 = (i: number) => (toffset = writeUInt64LE(tbuffer, i, toffset));
    const writeUInt64LE = (buffer: Buffer, value: number, offset: number) => {
      buffer.writeInt32LE(value & -1, offset);
      buffer.writeUInt32LE(Math.floor(value / 0x100000000), offset + 4);
      return offset + 8;
    };
    const writeVarInt = (i: number) => {
      varuint.encode(i, tbuffer, toffset);
      toffset += varuint.encode.bytes;
    };
    const writeVarSlice = (slice: Buffer) => {
      writeVarInt(slice.length);
      writeSlice(slice);
    };
    const varSliceSize = (someScript: Buffer) => {
      const length = someScript.length;
      return varuint.encodingLength(length) + length;
    };

    let hashOutputs = ZERO;
    let hashPrevouts = ZERO;
    let hashSequence = ZERO;

    if (!(hashType & SigHash.SIGHASH_ANYONECANPAY)) {
      tbuffer = Buffer.allocUnsafe(36 * this.tx.ins.length);
      toffset = 0;

      this.tx.ins.forEach(txIn => {
        writeSlice(txIn.hash);
        writeUInt32(txIn.index);
      });

      hashPrevouts = crypto.hash256(tbuffer);
    }

    if (
      !(hashType & SigHash.SIGHASH_ANYONECANPAY) &&
      (hashType & 0x1f) !== SigHash.SIGHASH_SINGLE &&
      (hashType & 0x1f) !== SigHash.SIGHASH_NONE
    ) {
      tbuffer = Buffer.allocUnsafe(4 * this.tx.ins.length);
      toffset = 0;

      this.tx.ins.forEach(txIn => {
        writeUInt32(txIn.sequence);
      });

      hashSequence = crypto.hash256(tbuffer);
    }

    if ((hashType & 0x1f) !== SigHash.SIGHASH_SINGLE && (hashType & 0x1f) !== SigHash.SIGHASH_NONE) {
      const txOutsSize = this.tx.outs.reduce((sum: number, out: any) => {
        return sum + 8 + varSliceSize(out.script);
      }, 0);

      tbuffer = Buffer.allocUnsafe(txOutsSize);
      toffset = 0;

      this.tx.outs.forEach((out: any) => {
        writeUInt64(out.value);
        writeVarSlice(out.script);
      });

      hashOutputs = crypto.hash256(tbuffer);
    } else if ((hashType & 0x1f) === SigHash.SIGHASH_SINGLE && this.vin < this.tx.outs.length) {
      const output = this.tx.outs[this.vin];

      tbuffer = Buffer.allocUnsafe(8 + varSliceSize(output.script));
      toffset = 0;
      writeUInt64(output.value);
      writeVarSlice(output.script);

      hashOutputs = crypto.hash256(tbuffer);
    }

    const input = this.tx.ins[this.vin];

    tbuffer = Buffer.allocUnsafe(156 + varSliceSize(this.redeemScript));
    toffset = 0;

    writeUInt32(this.tx.version);
    writeSlice(hashPrevouts);
    writeSlice(hashSequence);
    writeSlice(input.hash);
    writeUInt32(input.index);
    writeVarSlice(this.redeemScript);
    writeUInt64(this.satoshis);
    writeUInt32(input.sequence);
    writeSlice(hashOutputs);
    writeUInt32(this.tx.locktime);
    writeUInt32(hashType | SigHash.SIGHASH_FORKID);

    return tbuffer;
  }
}

export type SigningCallback = (input: Challenges, context: SigningContext) => ScriptSig;

export class TxBuilder {
  private inputs: Coin[] = [];
  private callbacks: SigningCallback[] = [];
  private builder: TransactionBuilder;

  constructor(network?: string) {
    this.builder = new TransactionBuilder(network);
  }

  from(utxos: Coin | Coin[], onSigning: SigningCallback, sequence?: number) {
    castArray(utxos).forEach(coin => {
      this.inputs.push(coin);
      this.callbacks.push(onSigning);
      this.builder.addInput(coin.utxo.txid, coin.utxo.vout, sequence);
    });
    return this;
  }

  to(scriptPubKey: string | Buffer, amount: number) {
    this.builder.addOutput(scriptPubKey, amount);
    return this;
  }

  withTimelock(locktime: number) {
    this.builder.setLockTime(locktime);
    return this;
  }

  build() {
    const scripts = this.callbacks.map((cb, vin) => {
      const { challenges, utxo, redeemScript } = this.inputs[vin];
      return cb(challenges, new P2SHContext(this.builder, vin, utxo.satoshis, redeemScript));
    });
    this.builder.addInputScripts(scripts);
    return this.builder.build();
  }
}
