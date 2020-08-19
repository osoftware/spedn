import {
  Challenges,
  ChallengeSpecs,
  Coin,
  ContractCoin,
  Instance,
  ModuleFactory,
  ParamTypes,
  ParamValues,
  SpednTypeChecker,
  stdlib
} from "./contracts";
import { Rts } from "./rts";

const checker = new SpednTypeChecker(stdlib.types);

export class GenericP2SH implements Instance {
  paramValues: ParamValues = {};
  public challengeSpecs: ChallengeSpecs;
  public challenges: Challenges = {
    spend: params => {
      const factory = new ModuleFactory(this.rts);
      checker.validateParamValues(params, this.challengeSpecs.spend);
      const argStack = Object.keys(this.challengeSpecs.spend).map((n: string) => factory.encodeParam(params[n]));
      argStack.push(factory.encodeParam(this.redeemScript));
      return this.rts.script.encode(argStack);
    }
  };

  constructor(private rts: Rts, public redeemScript: Buffer, public redeemArgs: ParamTypes) {
    this.challengeSpecs = {
      spend: redeemArgs
    };
  }

  getAddress(network: string): string {
    return this.rts.addresses.fromOutputScript(
      this.rts.script.encodeScriptHashOutput(this.rts.crypto.hash160(this.redeemScript)),
      network
    );
  }

  async findCoins(network: string): Promise<Coin[]> {
    const results = await this.rts.utxo(this.getAddress(network));
    return results.utxos.map((utxo: any) => new ContractCoin(utxo, this.challenges, this.redeemScript));
  }
}
