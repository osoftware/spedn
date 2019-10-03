import { AddressUtxoResult } from "bitcoin-com-rest";
import {
  addr,
  Challenges,
  ChallengeSpecs,
  Coin,
  ContractCoin,
  crypto,
  Instance,
  ModuleFactory,
  ParamTypes,
  ParamValues,
  script,
  stdlib
} from "./contracts";

export class GenericP2SH implements Instance {
  paramValues: ParamValues = {};
  public challengeSpecs: ChallengeSpecs;
  public challenges: Challenges = {
    spend: params => {
      const std = new ModuleFactory(stdlib);
      std.validateParamValues(params, this.challengeSpecs.spend);
      const argStack = Object.keys(this.challengeSpecs.spend).map((n: string) => std.encodeParam(params[n]));
      argStack.push(std.encodeParam(this.redeemScript));
      return script.encode(argStack);
    }
  };

  constructor(public redeemScript: Buffer, public redeemArgs: ParamTypes) {
    this.challengeSpecs = {
      spend: redeemArgs
    };
  }

  getAddress(network: string): string {
    return addr[network].fromOutputScript(script.encodeP2SHOutput(crypto.hash160(this.redeemScript)), network);
  }

  async findCoins(network: string): Promise<Coin[]> {
    const results = (await addr[network].utxo(this.getAddress(network))) as AddressUtxoResult;
    return results.utxos.map(utxo => new ContractCoin(utxo, this.challenges, this.redeemScript));
  }
}
