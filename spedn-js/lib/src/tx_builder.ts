import { Crypto, TransactionBuilder } from "bitbox-sdk";
import { ECPair } from "bitcoincashjs-lib";
import { castArray, last } from "lodash/fp";
import varuint from "varuint-bitcoin";
import { Challenges, Coin, ScriptSig } from "./contracts";

export interface SigningContext {
  vin: number;
  preimage(sighashFlag: number): Buffer;
  sign(key: ECPair, hashType?: SigHash): Buffer;
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

const varSliceSize = (someScript: Buffer) => {
  const length = someScript.length;
  return varuint.encodingLength(length) + length;
};

class SchnorrContext implements SigningContext {
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
    return key.sign(crypto.hash256(this.preimage(hashType)), 1).toScriptSignature(hashType, 1);
  }

  signData(key: ECPair, data: Buffer): Buffer {
    return key.sign(crypto.sha256(data));
  }

  preimage(hashType: SigHash = SigHash.SIGHASH_ALL) {
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

class SizeCalculationContext implements SigningContext {
  vin = 0;
  constructor(public redeemScript: Buffer) {}

  preimage = (sighashFlag: number) => Buffer.allocUnsafe(156 + varSliceSize(this.redeemScript));
  sign = (key: ECPair, hashType?: SigHash) => Buffer.alloc(65, SigHash.SIGHASH_ALL);
  signData = (key: ECPair, data: Buffer) => Buffer.allocUnsafe(64);
}

const FEE_RATE = 1;
const DUST_LIMIT = 546;

export type SigningCallback = (input: Challenges, context: SigningContext) => ScriptSig;

export class TxBuilder {
  private inputs: Coin[] = [];
  private callbacks: SigningCallback[] = [];
  private builder: TransactionBuilder;
  private change: any;
  private balance = 0;

  constructor(network?: string) {
    this.builder = new TransactionBuilder(network);
  }

  from(utxos: Coin | Coin[], onSigning: SigningCallback, sequence?: number) {
    castArray(utxos).forEach(coin => {
      this.inputs.push(coin);
      this.callbacks.push(onSigning);
      this.builder.addInput(coin.utxo.txid, coin.utxo.vout, sequence);
      this.balance += coin.utxo.satoshis;
    });
    return this;
  }

  to(scriptPubKey: string | Buffer, amount?: number) {
    this.builder.addOutput(scriptPubKey, amount || 0);
    this.balance -= amount || 0;
    if (amount === undefined) {
      if (this.change !== undefined) throw Error("Olny one output can automatically calculate amount.");
      this.change = last(this.builder.transaction.tx.outs);
    }
    return this;
  }

  withTimelock(locktime: number) {
    this.builder.setLockTime(locktime);
    return this;
  }

  build(forceHighFee = false) {
    const placeholders = this.inputs.map(({ challenges, redeemScript }, i) => ({
      vout: i,
      script: this.callbacks[i](challenges, new SizeCalculationContext(redeemScript))
    }));
    this.builder.addInputScripts(placeholders);
    const fee = this.builder.build().byteLength() * FEE_RATE;

    if (this.change) {
      if (this.balance > fee + DUST_LIMIT) this.change.value = this.balance - fee;
      else throw Error("Change output is below dust level.");
    } else if (this.balance > fee * 3 && !forceHighFee) {
      throw Error("Fee is unreasonably high.");
    }

    const scripts = this.inputs.map(({ challenges, utxo, redeemScript }, i) => ({
      vout: i,
      script: this.callbacks[i](challenges, new SchnorrContext(this.builder, i, utxo.satoshis, redeemScript))
    }));

    this.builder.addInputScripts(scripts);
    const tx = this.builder.build();

    return tx;
  }
}
