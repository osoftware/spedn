import { AddressUtxoResult } from "bitcoin-com-rest";
import { ECPair } from "bitcoincashjs-lib";
import { SigningCallback } from ".";
import {
  addr,
  ChallengeSpecs,
  Coin,
  crypto,
  encodeParam,
  Instance,
  ParamValues,
  script,
  Utxo,
  validateParamValues
} from "./contracts";

export class P2PKH implements Instance {
  static fromPubKey = (pubKey: Buffer) => new P2PKH(crypto.hash160(pubKey));
  static fromKeyPair = (keyPair: ECPair) => P2PKH.fromPubKey(keyPair.getPublicKeyBuffer());
  static fromAddress = (address: string, network = "mainnet") =>
    new P2PKH(Buffer.from(addr[network].cashToHash160(address), "hex"));

  paramValues: ParamValues = {};
  challengeSpecs: ChallengeSpecs = { spend: { sig: "Sig", pubKey: "PubKey" } };
  redeemScript: Buffer;

  constructor(pubKeyHash: Buffer) {
    this.paramValues.pubKeyHash = pubKeyHash;
    this.redeemScript = script.encodeP2PKHOutput(pubKeyHash);
  }

  getAddress(network = "mainnet"): string {
    return addr[network].fromOutputScript(this.redeemScript, network);
  }

  async findCoins(network = "mainnet"): Promise<Coin[]> {
    const result = (await addr[network].utxo(this.getAddress(network))) as AddressUtxoResult;
    return result.utxos.map(utxo => new P2PKHCoin(utxo, this.redeemScript));
  }
}

class P2PKHCoin implements Coin {
  challenges = {
    spend: ({ sig, pubKey }: ParamValues) => {
      validateParamValues({ sig, pubKey }, { sig: "Sig", pubKey: "PubKey" });
      return script.encodeP2PKHInput(encodeParam(sig), encodeParam(pubKey));
    }
  };

  constructor(public utxo: Utxo, public redeemScript: Buffer) {}
}

export function p2pkh(key: ECPair): SigningCallback {
  return (i, c) => i.spend({ sig: c.sign(key), pubKey: key.getPublicKeyBuffer() });
}
