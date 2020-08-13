import BCHJS from "@chris.troutner/bch-js";
import { Spedn, using, P2PKHFactory } from ".";
import { ContractCoin, Module } from "./contracts";
import { P2PKH, P2PKHCoin, signWith } from "./P2PKH";
import { SigHash, TxBuilder } from "./TxBuilder";
import { BchJsRts } from "./rts-bchjs";

const bchjs = new BCHJS({ restURL: "https://tapi.fullstack.cash/v3/" });
const rts = new BchJsRts("testnet", bchjs);
const addr = new P2PKHFactory(rts);
const mnemonic = "draw parade crater busy book swim soldier tragic exit feel top civil";

describe("TxBuilder", () => {
  let key0: any;
  let addr0: any;
  let key1: any;
  let addr1: any;
  let key2: any;
  let change2: any;
  let coins: any;
  beforeAll(async () => {
    const hdNode = bchjs.HDNode.fromSeed(await bchjs.Mnemonic.toSeed(mnemonic), "testnet");
    const wallet = bchjs.HDNode.derivePath(hdNode, "m/44'/145'/0'");
    key0 = bchjs.HDNode.derivePath(wallet, "0/0").keyPair;
    addr0 = addr.fromKeyPair(key0);
    key1 = bchjs.HDNode.derivePath(wallet, "0/1").keyPair;
    addr1 = addr.fromKeyPair(key1);
    key2 = bchjs.HDNode.derivePath(wallet, "1/0").keyPair;
    change2 = addr.fromKeyPair(key2);

    coins = [
      new P2PKHCoin(
        rts,
        {
          txid: "ad70c931d742d6903271d1d3047701fb25b6859c440aeacf774d242f74f10738",
          vout: 0,
          amount: 100000,
          satoshis: 100000,
          height: 12345,
          confirmations: 30
        },
        addr0.redeemScript
      ),
      new P2PKHCoin(
        rts,
        {
          txid: "ad70c931d742d6903271d1d3047701fb25b6859c440aeacf774d242f74f10738",
          vout: 1,
          amount: 100000,
          satoshis: 100000,
          height: 12345,
          confirmations: 30
        },
        addr1.redeemScript
      ),
      new P2PKHCoin(
        rts,
        {
          txid: "ad70c931d742d6903271d1d3047701fb25b6859c440aeacf774d242f74f10738",
          vout: 2,
          amount: 100000,
          satoshis: 100000,
          height: 12345,
          confirmations: 30
        },
        change2.redeemScript
      )
    ];
  });
  describe("type validation", () => {
    let mod: Module;
    let utxo: ContractCoin;

    beforeAll(async () => {
      await using(new Spedn(new BchJsRts("testnet", bchjs)), async compiler => {
        mod = await compiler.compileCode(`
            contract X() {
              challenge spend(Ripemd160 hash, [byte;4] bytes4, [byte] bytes, int integer) {
                fail;
              }
            }
          `);
        const address = new mod.X({});
        utxo = new ContractCoin(
          {
            txid: "6b5c8d90e8ac791d00c1d70bcc7a52fb4fd9077bf07387b0db9240a919cdabdf",
            vout: 0,
            amount: 5000000,
            satoshis: 5000000,
            confirmations: 10,
            height: 100
          },
          (address as any).challenges,
          address.redeemScript
        );
      });
    });

    it("should accept correct types", () => {
      expect(
        utxo.challenges.spend({ hash: Buffer.alloc(20), bytes4: Buffer.alloc(4), bytes: Buffer.alloc(15), integer: 5 })
      ).toBeDefined();
    });
    it("should convert strings to Buffers", () => {
      expect(
        utxo.challenges.spend({ hash: "12345678901234567890", bytes4: "abcd", bytes: "qwerty", integer: 5 })
      ).toBeDefined();
    });
    it("should throw on wrong sizes of arrays", () => {
      expect(() =>
        utxo.challenges.spend({ hash: "1234567890", bytes4: "abcde", bytes: "qwerty", integer: 5 })
      ).toThrow();
    });
  });

  describe("size calculation", () => {
    let builder: TxBuilder;
    beforeEach(
      () =>
        (builder = new TxBuilder(new BchJsRts("testnet")).from(coins[0], signWith(key0)).from(coins[1], signWith(key1)))
    );

    it("should calculate change output amount", () => {
      const tx = builder.to(addr0.getAddress("testnet"), 100000).to(change2.getAddress("testnet")).build();
      expect(tx.outs[1].value).toBe(100000 - tx.byteLength());
    });

    it("should protect from overpaying", () => {
      expect(() => builder.to(addr0.getAddress("testnet"), 100000).build()).toThrowError("Fee is unreasonably high.");
    });

    it("should allow overpaying if expicitly requested", () => {
      builder.to(addr0.getAddress("testnet"), 100000).build(true);
    });

    it("should protect from dust output", () => {
      expect(() =>
        builder.to(addr0.getAddress("testnet"), 199990).to(change2.getAddress("testnet")).build()
      ).toThrowError("Change output is below dust level.");
    });
  });

  describe("signing context", () => {
    it("should generate equivalent signatures for checkSig and checkDataSig", () => {
      let sig: Buffer;
      let datasig: Buffer;
      const flag = Buffer.alloc(1, SigHash.SIGHASH_ALL | SigHash.SIGHASH_FORKID);
      const tx = new TxBuilder(new BchJsRts("testnet"))
        .from(coins[0], (i, c) => {
          sig = c.sign(key0);
          datasig = c.signData(key0, bchjs.Crypto.sha256(c.preimage(SigHash.SIGHASH_ALL)));

          return i.spend({ pubKey: key0.getPublicKeyBuffer(), sig });
        })
        .to(addr1.getAddress("testnet"), 9999300)
        .build();

      // @ts-ignore
      expect(sig.toString("hex")).toEqual(Buffer.concat([datasig, flag]).toString("hex"));
    });
  });

  describe("build", () => {
    let mod: Module;
    beforeAll(
      async () =>
        await using(new Spedn(new BchJsRts("testnet", bchjs)), async compiler => {
          mod = await compiler.compileFile("../../examples/PayToPublicKeyHash.spedn");
        })
    );

    it("should generate valid signature", () => {
      const address = new mod.PayToPublicKeyHash({ pubKeyHash: bchjs.Crypto.sha256(key1.getPublicKeyBuffer()) });
      const utxo0 = new ContractCoin(
        {
          txid: "6b5c8d90e8ac791d00c1d70bcc7a52fb4fd9077bf07387b0db9240a919cdabdf",
          vout: 0,
          amount: 5000000,
          satoshis: 5000000,
          confirmations: 10,
          height: 100
        },
        (address as any).challenges,
        address.redeemScript
      );
      const utxo1 = new P2PKHCoin(
        rts,
        {
          txid: "6b5c8d90e8ac791d00c1d70bcc7a52fb4fd9077bf07387b0db9240a919cdabdf",
          vout: 1,
          amount: 4999700,
          satoshis: 4999700,
          confirmations: 10,
          height: 100
        },
        change2.redeemScript
      );

      const tx = new TxBuilder(rts)
        .from(utxo0, (i, c) => i.spend({ pubKey: key1.getPublicKeyBuffer(), sig: c.sign(key1) }))
        .from(utxo1, signWith(key2))
        .to(addr1.getAddress("testnet"), 9999300)
        .build();

      expect(tx.getId()).toEqual("ad70c931d742d6903271d1d3047701fb25b6859c440aeacf774d242f74f10738");
    });
  });
});
