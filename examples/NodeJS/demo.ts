import { BITBOX } from "bitbox-sdk";
import { P2PKH, SigHash, signWith, Spedn, TxBuilder } from "spedn";

const bitbox = new BITBOX();
const mnemonic = "draw parade crater busy book swim soldier tragic exit feel top civil";
const wallet = bitbox.HDNode.fromSeed(bitbox.Mnemonic.toSeed(mnemonic));
const alice = bitbox.HDNode.derivePath(wallet, "m/44'/145'/0'/0/0");
const bob = bitbox.HDNode.derivePath(wallet, "m/44'/145'/1'/0/0");

async function main() {
  const compiler = new Spedn();
  const ExpiringTip = await compiler.compileCode(`
    contract ExpiringTip(Ripemd160 alice, Ripemd160 bob) {
      challenge receive(Sig sig, PubKey pubKey) {
          verify hash160(pubKey) == bob;
          verify checkSig(sig, pubKey);
      }
      challenge revoke(Sig sig, PubKey pubKey) {
          verify checkSequence(7d);
          verify hash160(pubKey) == alice;
          verify checkSig(sig, pubKey);
      }
    }
  `);
  compiler.dispose();

  console.log(ExpiringTip.params);

  const tip = new ExpiringTip({
    alice: alice.getIdentifier(),
    bob: bob.getIdentifier()
  });

  console.log(tip.getAddress("mainnet"));

  const coins = await tip.findCoins("mainnet");
  console.log(coins);

  console.log(tip.challengeSpecs);

  let addr = new P2PKH(bob.getIdentifier());

  const bobsCoins = await addr.findCoins("mainnet");

  const txid = await new TxBuilder("mainnet")
     .from(coins, (input, context) =>
        input.receive({
           sig: context.sign(bob.keyPair, SigHash.SIGHASH_ALL),
           pubKey: bob.getPublicKeyBuffer()
        })
     )
     .from(bobsCoins[14], signWith(bob.keyPair))
     .to("bitcoincash:qrc2jhalczuka8q3dvk0g8mnkqx79wxp9gvvqvg7qt", 500000)
     .to(alice.getAddress())
     .withTimelock(567654)
     .broadcast();

  console.log(txid);
}
main();
