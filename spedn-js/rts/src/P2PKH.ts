import { SigningCallback } from ".";
import {
  ChallengeSpecs,
  Coin,
  Instance,
  ModuleFactory,
  ParamValues,
  SpednTypeChecker,
  stdlib,
  Utxo
} from "./contracts";
import { Rts, RtsECPair } from "./rts";

const checker = new SpednTypeChecker(stdlib.types);

export class P2PKHFactory {
  constructor(private rts: Rts) {}
  fromPubKey = (pubKey: Buffer) => new P2PKH(this.rts, this.rts.crypto.hash160(pubKey));
  fromKeyPair = (keyPair: RtsECPair) => this.fromPubKey(keyPair.getPublicKeyBuffer());
  fromAddress = (address: string, network = "mainnet") =>
    new P2PKH(this.rts, Buffer.from(this.rts.addresses.toHash160(address), "hex")); // tslint:disable-line: semicolon
}

export class P2PKH implements Instance {
  paramValues: ParamValues = {};
  challengeSpecs: ChallengeSpecs = { spend: { sig: "Sig", pubKey: "PubKey" } };
  redeemScript: Buffer;

  constructor(private rts: Rts, pubKeyHash: Buffer) {
    this.paramValues.pubKeyHash = pubKeyHash;
    this.redeemScript = this.rts.script.encodePubKeyHashOutput(pubKeyHash);
  }

  getAddress(network = "mainnet"): string {
    return this.rts.addresses.fromOutputScript(this.redeemScript, network);
  }

  async findCoins(network = "mainnet"): Promise<Coin[]> {
    const result = await this.rts.utxo(this.getAddress(network));
    return result.utxos.map((utxo: any) => new P2PKHCoin(this.rts, utxo, this.redeemScript));
  }
}

export class P2PKHCoin implements Coin {
  challenges = {
    spend: ({ sig, pubKey }: ParamValues) => {
      const std = new ModuleFactory(this.rts);
      checker.validateParamValues({ sig, pubKey }, { sig: "Sig", pubKey: "PubKey" });
      return this.rts.script.encodePubKeyHashInput(std.encodeParam(sig), std.encodeParam(pubKey));
    }
  };

  constructor(private rts: Rts, public utxo: Utxo, public redeemScript: Buffer) {}
}

export function signWith(key: RtsECPair): SigningCallback {
  return (i, c) => i.spend({ sig: c.sign(key), pubKey: key.getPublicKeyBuffer() });
}
