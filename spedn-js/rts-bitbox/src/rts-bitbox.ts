import { Addresses, Crypto, Rts, RtsECPair, RtsTransactionBuilder, Script, UtxoResult } from "@spedn/rts";
import { BITBOX, TREST_URL, TWS_URL } from "bitbox-sdk";
import { AddressUtxoResult } from "bitcoin-com-rest";
import Bitcoin from "bitcoincashjs-lib";

const defaultConfigs: { [network: string]: any } = {
  mainnet: undefined,
  testnet: { restURL: TREST_URL, wsURL: TWS_URL }
};

export class BitboxRts extends Rts {
  private readonly bitbox: BITBOX;

  constructor(public readonly network: string, bitbox?: BITBOX) {
    super(network);
    this.bitbox = bitbox || new BITBOX(defaultConfigs[network]);
  }

  utxo(addr: string): Promise<UtxoResult> {
    return this.bitbox.Address.utxo(addr) as any;
  }

  ecPair(ecPair: Bitcoin.ECPair): RtsECPair {
    return ecPair;
  }

  get addresses(): Addresses {
    const addr = this.bitbox.Address;
    return {
      toHash160: addr.cashToHash160.bind(addr),
      fromOutputScript: addr.fromOutputScript.bind(addr)
    };
  }

  get crypto(): Crypto {
    return this.bitbox.Crypto;
  }

  get script(): Script {
    return new BchJsScript(this.bitbox, Bitcoin.script);
  }

  transactionBuilder(): RtsTransactionBuilder {
    return new this.bitbox.TransactionBuilder(this.network);
  }

  async sendTx(tx: any): Promise<string> {
    return await this.bitbox.RawTransactions.sendRawTransaction(tx.toHex());
  }
}

class BchJsScript implements Script {
  opcodes: Map<string, number> = this.bitbox.Script.opcodes as any;

  constructor(private readonly bitbox: BITBOX, private readonly script: Bitcoin.Script) {}

  encode(argStack: Buffer[]): Buffer {
    return this.bitbox.Script.encode(argStack);
  }

  encodeNumber(integer: number): Buffer {
    return this.script.number.encode(integer);
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
