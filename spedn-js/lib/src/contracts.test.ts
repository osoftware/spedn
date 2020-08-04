import BCHJS from "@chris.troutner/bch-js";
import { Spedn } from "./compiler";
import {  Module } from "./contracts";
import { using } from "./disposable";
import { P2PKH } from "./P2PKH";

describe("ExpiringTip contract", () => {
  let mod: Module;
  beforeAll(
    async () =>
      await using(new Spedn(), async compiler => {
        mod = await compiler.compileFile("../../examples/ExpiringTip.spedn");
      })
  );

  describe("template", () => {
    it("should have 2 parameters", () => {
      expect(mod.ExpiringTip.params).toEqual({
        alice: "Ripemd160",
        bob: "Ripemd160"
      });
    });
  });

  describe("instance", () => {
    it("should throw on missing parameters", () => {
      expect(() => new mod.ExpiringTip({})).toThrow("Missing parameter: Ripemd160 alice");
    });
    it("should throw on invalid parameters", () => {
      expect(() => new mod.ExpiringTip({ alice: 1, bob: true })).toThrow("Incorrect value for Ripemd160 alice");
    });
    it("should have 2 challenges", () => {
      const instance = new mod.ExpiringTip({
        alice: Buffer.alloc(20),
        bob: Buffer.alloc(20)
      });
      expect(instance.challengeSpecs).toHaveProperty("receive");
      expect(instance.challengeSpecs).toHaveProperty("revoke");
    });
  });
});

describe("P2PKH", () => {
  describe("factory methods", () => {
    it("should be equivalent", async () => {
      const b = new BCHJS();
      const node = b.HDNode.fromSeed(await b.Mnemonic.toSeed(b.Mnemonic.generate(128)));
      const addr = b.Address.toCashAddress(node.keyPair.getAddress());
      expect(addr).toEqual(P2PKH.fromKeyPair(node.keyPair).getAddress("mainnet"));
      expect(addr).toEqual(P2PKH.fromAddress(node.getAddress()).getAddress("mainnet"));
      expect(addr).toEqual(P2PKH.fromPubKey(node.keyPair.getPublicKeyBuffer()).getAddress("mainnet"));
    });
  });
});
