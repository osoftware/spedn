==================
BITBOX Integration
==================

Spedn is available for NodeJS_ developers as an SDK extending capabilities of
`BITBOX SDK`_.
TypeScript_ type definitions are provided out of the box.

Installation
============

NodeJS **v11** or newer is required. You can also use **v10** but then `Worker Threads`_ feature
has to be explicitly enabled by ``--experimental-worker`` flag.

To install Spedn SDK in your JS project, type:

.. code-block:: bash

   npm i spedn

or

.. code-block:: bash

   yarn add spedn


Compiler service
================

Spedn compiler runs as a service in a worker thread that you can start, use and dispose with ``Spedn`` class.

.. code-block:: TypeScript

   import { Spedn } from "spedn";

   async function main() {

      const compiler = new Spedn();
      /* use compiler */
      compiler.dispose();

   }
   main();

Instead of manually disposing the service you can also use ``using`` function inspired by some languages,
which guarantees automatic disposal of a resource also in case of exceptions.

.. code-block:: TypeScript

   import { Spedn, using } from "spedn";

   async main() {

      await using(new Spedn(), async compiler => {
         /* use compiler */
      });

   }
   main();


Compiling contracts
===================

To compile a source file use ``compileFile`` method.
To compile source code in a string, use ``compileCode``.

.. code-block:: TypeScript

   const BlindEscrow = await compiler.compileFile("./BlindEscrow.spedn");

   const ExpiringTip = await compiler.compileCode(::`
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

The output of those methods is a JavaScript class representing a contract template.
Static field ``params`` describes what parameters are required to instantiate it.

.. code-block:: TypeScript

   console.log(ExpiringTip.params);
   // Object {alice: "Ripemd160", bob: "Ripemd160"}


Instantiating contracts
=======================

To instantiate the template, just create an object of the contract class, providing parameters values.
Parameters are passed as an object literal explicitly assigning values by names. Values of ``bool`` and ``int``
*Spedn* type can be passed as ordinary *JS* booleans and numbers. ``Time`` and ``TimeSpan`` are also passed as numbers
(see BIP65_ and BIP112_ for value interpretation details).
All the other types should be passed as *JS* ``Buffer``.

In case of ``ExpiringTip`` you'll need 2 public keys which you can generate with BITBOX.

.. code-block:: TypeScript

   import { BITBOX } from "bitbox-sdk";

   const bitbox = new BITBOX();
   const mnemonic = "draw parade crater busy book swim soldier tragic exit feel top civil";
   const wallet = bitbox.HDNode.fromSeed(bitbox.Mnemonic.toSeed(mnemonic));
   const alice = bitbox.HDNode.derivePath(wallet, "m/44'/145'/0'/0/0");
   const bob = bitbox.HDNode.derivePath(wallet, "m/44'/145'/1'/0/0");

   const tip = new ExpiringTip({
      alice: alice.getIdentifier(), // Ripemd160 hash of Alice's public key
      bob:   bob.getIdentifier()    // Ripemd160 hash of Bob's public key
   });

Once created, you can read the contract funding address and lookup for UTXOs (coins) that are locked in it.
Also, a field ``challengeSpecs`` contains definitions of challenges and their parameters.

.. code-block:: TypeScript

   console.log(tip.getAddress("mainnet"));
   // bitcoincash:pppvx30pcylxzhewr6puknpuvz7gjjtl4sdw4ezcnp

   const coins = await tip.findCoins("mainnet");
   // Array(2) [.....]

   console.log(tip.challengeSpecs);
   // Object {receive: Object, revoke: Object}
   console.log(tip.challengeSpecs.receive);
   // Object {sig: "Sig", pubKey: "PubKey"}

Spending coins
==============

To spend coins, use ``TxBuilder``. Provide tx inputs with ``from`` method and outputs with ``to`` method.
Optionally, set a timelock with ``withTimelock``.
To send the transaction to the network use ``broadcast`` method.
If you just want to build the transaction without broadcasting it, use ``build`` method.

``from`` method accept a single coin or an array of coins as a first parameter.
Because you can't (in most cases) sign the input without defining all the inputs and outputs first,
``from`` method does not simply accept scriptSig parameter. Instead, it accepts a ``SigningCallback`` function
and the actual signing is deferred to the moment of calling ``build``/``broadcast``.

``SigningCallback`` accepts 2 parameters. The first one is an object containing contract challenges.
The second one is a ``SigningContext`` which provides methods necessary for signing:

   * ``sign(keyPair, hashType)`` - generates a siggnature valid for ``OP_CHECKSIG``.
   * ``signData(keyPair, data)`` - generates a signature valid for ``OP_CHECKDATASIG``.
   * ``preimage(hashType)`` - generates the same preimage_ as one used by ``sign(keyPair, hashType)``
     (useful for ``OP_CHECKDATASIG`` covenants).

Note that methods accepting ``hashType`` always add ``SIGHASH_FORKID`` flag so you don't need to specify it
explicitly.

``to`` method accepts an address or a scriptPubKey buffer as its first argument and an amount (in satoshis)
as the second one. You can also omit the amount at a single output - in this case, ``TxBuilder`` will
treat this output as a change address and automatically calculate its amount choosing optimal transaction fee.

In the following example, all the previously found coins are spent using ``receive`` challenge but 5mBCH goes to
Bob's new address and the rest goes back to Alice.

.. code-block:: TypeScript

   import { TxBuilder, SigHash } from "spedn";

   const txid = await new TxBuilder("mainnet")
      .from(coins, (input, context) =>
         input.receive({
            sig: context.sign(bob.keyPair, SigHash.SIGHASH_ALL),
            pubKey: bob.getPublicKeyBuffer()
         })
      )
      .to("bitcoincash:qrc2jhalczuka8q3dvk0g8mnkqx79wxp9gvvqvg7qt", 500000)
      .to(alice.getAddress())
      .withTimelock(567654)
      .broadcast();


Spending ordinary P2PKH
-----------------------

Spedn SDK provides also a class ``P2PKH`` which is a representation of an ordinary Pay to Public Key Hash address.
You can instantiate it with a public key hash buffer or several factory methods:

.. code-block:: TypeScript

   import { P2PKH } from "spedn";

   let addr = new P2PKH(bob.getIdentifier());
   addr = P2PKH.fromKeyPair(bob.keyPair);
   addr = P2PKH.fromPubKey(bob.getPublicKeyBuffer());
   addr = P2PKH.fromAddress(bob.getAddress());
   // all the above are equivalent

P2PKH contracts can be spent just like any other contract - they have ``spend({sig, pubKey})`` challenge,
but you can also replace the whole signing callback with a convenient helper ``signWith(keyPair)``.
Let's modify the previous example to spend additional input.

.. code-block:: TypeScript

   import { signWith } from "spedn";

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


Spending generic P2SH
---------------------

Spedn SDK provides also a class ``GenericP2SH`` for interoperability with any Pay to Script Hash contract
created without Spedn. To work with that kind of contract, you just need to know its redeemScript and
what arguments it expects. The generated class will have a single challenge ``spend`` with parameter
requirements as specified in the constructor.

.. code-block:: TypeScript

   import { GenericP2SH } from "spedn";

   const contract = new GenericP2SH(redeemScriptBuffer, { sig: "Sig", someNumber: "int" });


.. _NodeJS: https://nodejs.org/
.. _BITBOX SDK: https://developer.bitcoin.com/bitbox
.. _TypeScript: https://www.typescriptlang.org/
.. _Worker Threads: https://nodejs.org/docs/latest-v12.x/api/worker_threads.html
.. _BIP65: https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki
.. _BIP112: https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki
.. _preimage: https://www.bitcoincash.org/spec/replay-protected-sighash.html#specification
