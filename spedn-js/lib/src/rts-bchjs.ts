import BCHJS, { Address, TransactionBuilder } from "@chris.troutner/bch-js";
import Bitcoin from "bitcoincashjs-lib";
import { Rts, RtsECPair, Addresses, Crypto, Script, RtsTransactionBuilder } from "./rts";
import { Dictionary, isString } from "lodash";

const defaultConfigs: Dictionary<any> = {
  mainnet: undefined,
  testnet: { restURL: "https://tapi.fullstack.cash/v3/" }
};

export class BchJsRts extends Rts {
  private readonly bchjs: BCHJS;

  constructor(network: string, bchjs?: BCHJS) {
    super(network);
    this.bchjs = bchjs || new BCHJS(defaultConfigs[network]);
  }

  utxo(addr: any) {
    return this.bchjs.Address.utxo(addr);
  }

  ecPair(ecPair: Bitcoin.ECPair): RtsECPair {
    return ecPair;
  }

  get addresses(): Addresses {
    return this.bchjs.Address;
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
  constructor(private readonly bchjs: BCHJS, private readonly script: Bitcoin.Script) {}

  encode(argStack: Buffer[]): Buffer {
    return this.bchjs.Script.encode(argStack);
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

  opcodes: Map<string, number> = this.bchjs.Script.opcodes;
}
