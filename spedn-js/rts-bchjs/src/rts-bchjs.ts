import BCHJS from "@psf/bch-js";
import { Addresses, Crypto, ModuleFactory, Rts, RtsECPair, RtsTransactionBuilder, Script, UtxoResult } from "@spedn/rts";
import Bitcoin from "@psf/bitcoincashjs-lib";

const defaultConfigs: { [network: string]: any } = {
  xec: { restURL: 'https://abc.fullstack.cash/v5/' },
  bch: { restURL: 'https://bchn.fullstack.cash/v5/' },
};

export class BchJsRts extends Rts {
  private readonly bchjs: BCHJS;
  constructor(public readonly network: string, bchjs?: BCHJS) {
    super(network);
    this.bchjs = bchjs || new BCHJS(defaultConfigs[network]);
  }

  async utxo(addr: string): Promise<UtxoResult> {
    const result = await this.bchjs.Electrumx.utxo(addr);
    return {
      ...result,
      utxos: result.utxos.map((o: any) => ({
        ...o,
        txid: o.tx_hash,
        vout: o.tx_pos,
        satoshis: o.value
      }))
    };
  }

  ecPair(ecPair: Bitcoin.ECPair): RtsECPair {
    return ecPair;
  }

  get addresses(): Addresses {
    return {
      fromOutputScript: (script: Buffer, network: string) => {
        const cashAddr = this.bchjs.Address.fromOutputScript(script, network);
        return this.network === "xec" ? this.bchjs.Address.toEcashAddress(cashAddr) : cashAddr;
      },
      toHash160: (addr: string) => {
        if (addr.startsWith("ecash:")) {
          addr = this.bchjs.Address.ecashtoCashAddress(addr);
        }
        return this.bchjs.Address.toHash160(addr);
      },
    };
  }

  get crypto(): Crypto {
    return this.bchjs.Crypto;
  }

  get script(): Script {
    return new BchJsScript(this.bchjs, Bitcoin.script);
  }

  transactionBuilder(): RtsTransactionBuilder {
    return new this.bchjs.TransactionBuilder(this.network);
  }

  async sendTx(tx: any): Promise<string> {
    return await this.bchjs.RawTransactions.sendRawTransaction(tx.toHex());
  }
}

class BchJsScript implements Script {
  opcodes: Map<string, number>;

  constructor(private readonly bchjs: BCHJS, private readonly script: Bitcoin.Script) {
    this.opcodes = bchjs.Script.opcodes;
  }

  encode(argStack: Buffer[]): Buffer {
    return this.bchjs.Script.encode(argStack);
  }

  encodeNumber(integer: number): Buffer {
    return this.script.number.encode(integer);
  }

  // Taken from @bitauth/libauth
  encodeBigNumber(integer: bigint): Buffer {
    if (integer === 0n) {
      return Buffer.from([]);;
    }

    const bytes: number[] = [];
    const isNegative = integer < 0;
    const byteStates = 0xff;
    const bitsPerByte = 8;
    // eslint-disable-next-line functional/no-let
    let remaining = isNegative ? -integer : integer;
    // eslint-disable-next-line functional/no-loop-statements
    while (remaining > 0) {
      // eslint-disable-next-line functional/no-expression-statements, functional/immutable-data, no-bitwise
      bytes.push(Number(remaining & BigInt(byteStates)));
      // eslint-disable-next-line functional/no-expression-statements, no-bitwise
      remaining >>= BigInt(bitsPerByte);
    }

    const signFlippingByte = 0x80;
    // eslint-disable-next-line no-bitwise, functional/no-conditional-statements, @typescript-eslint/no-non-null-assertion
    if ((bytes[bytes.length - 1]! & signFlippingByte) > 0) {
      // eslint-disable-next-line functional/no-expression-statements, functional/immutable-data
      bytes.push(isNegative ? signFlippingByte : 0x00);
      // eslint-disable-next-line functional/no-conditional-statements
    } else if (isNegative) {
      // eslint-disable-next-line functional/no-expression-statements, functional/immutable-data, no-bitwise
      bytes[bytes.length - 1] |= signFlippingByte;
    }
    return Buffer.from(bytes);
  }

  decodeNumber(buffer: Buffer, maxLength?: number | undefined, minimal?: boolean | undefined): number {
    return this.script.number.decode(buffer, maxLength, minimal);
  }

  encodeScriptHashOutput(buffer: Buffer): Buffer {
    return this.script.scriptHash.output.encode(buffer);
  }

  encodePubKeyHashOutput(buffer: Buffer): Buffer {
    return this.script.pubKeyHash.output.encode(buffer);
  }

  encodePubKeyHashInput(sig: Buffer, pubKey: Buffer): Buffer {
    return this.script.pubKeyHash.input.encode(sig, pubKey);
  }
}
