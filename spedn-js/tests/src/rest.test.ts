import BCHJS from "@chris.troutner/bch-js";
import { P2PKHFactory } from "@spedn/rts";
import { BchJsRts } from "@spedn/rts-bchjs";
import { BitboxRts } from "@spedn/rts-bitbox";
import { BITBOX } from "bitbox-sdk";

const bchjs = new BCHJS({ restURL: "https://testnet3.fullstack.cash/v3/" });
const bitbox = new BITBOX({ restURL: "https://trest.bitcoin.com/v2/" });

describe.each([
  new BchJsRts("testnet", bchjs)
  // new BitboxRts("testnet", bitbox) // tREST seems to be dysfunctional
])("%s REST API", rts => {
  it("can find coins", async () => {
    const factory = new P2PKHFactory(rts);
    // testnet : draw parade crater busy book swim soldier tragic exit feel top civil : m/44'/145'/0'/0/3
    const account = factory.fromAddress("bchtest:qzca95r68adkek2we4jvws5dex6pwg3vt52mkhsqjg");
    const [coin] = await account.findCoins("testnet");
    expect(coin).toBeDefined();
    expect(coin).toHaveProperty("utxo.txid");
    expect(coin).toHaveProperty("utxo.vout");
    expect(coin).toHaveProperty("utxo.satoshis");
    expect(coin).toHaveProperty("utxo.height");
  });
});
