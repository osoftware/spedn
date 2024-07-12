import BCHJS from "@psf/bch-js";
import { P2PKHFactory } from "@spedn/rts";
import { BchJsRts } from "@spedn/rts-bchjs";


describe.each([
  new BchJsRts("bch"),
])("%s REST API", rts => {
  it("can find coins", async () => {
    const factory = new P2PKHFactory(rts);
    const account = factory.fromAddress("1LdRcdxfbSnmCYYNdeYpUnztiYzVfBEQeC");
    const [coin] = await account.findCoins("mainnet");
    expect(coin).toBeDefined();
    expect(coin).toHaveProperty("utxo.txid");
    expect(coin).toHaveProperty("utxo.vout");
    expect(coin).toHaveProperty("utxo.satoshis");
    expect(coin).toHaveProperty("utxo.height");
  });
});
