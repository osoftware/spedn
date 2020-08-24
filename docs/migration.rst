===============
Migration Guide
===============

Migrationg from v0.3 to v0.3.1
==============================

Javascript SDK in this release has been decoupled into several smaller components.
The Compiler stays in SDK library which moved to ``@spedn/sdk``. However, you won't need it anymore
if you just want to use already compiled contracts. In that case, install RTS (Runtime System) library with backend of choice:

* use ``@spedn/rts-bchjs`` if you want to build the app on top of `BCH-JS <https://bchjs.fullstack.cash/>`_.
* use ``@spedn/rts-bitbox`` if you want to build the app on top of `BITBOX-SDK <https://developer.bitcoin.com/bitbox>`_.

To use RTS, import apropriate adapter:

.. code-block:: TypeScript

   import { BchjsRts } from "@spedn/rts-bchjs";
   import BCHJS from "@chris.troutner/bch-js";

   const rts = new BchjsRts("mainnet")
   const testRts = new BchjsRts("testnet", new BCHJS({ restURL: "https://tapi.fullstack.cash/v3/" }));

To allow RTS to work without compiler, the Portable format has been introduced.
When using the CLI, you can request the compiler to output it with ``-f portable`` switch.
When using the Spedn service from SDK, this format will be returned by default.
To turn this JSON into contract classes as before, use ``rts.load(portable)``.

.. code-block:: TypeScript

   const mod = await using(new Spedn(), async compiler => await compiler.compileFile("./BlindEscrow.spedn"));
   fs.writeFileSync("./blind_escrow.json", JSON.stringify(mod));
   // ...  
   const { BlindEscrow } = rts.load(JSON.parse(fs.readFileSync("./blind_escrow.json");

You'll also need RTS to create TxBuilder:

.. code-block:: TypeScript

   import { TxBuilder } from "@spedn/rts";

   const builder = new TxBuilder(rts);

Static factory methods in P2PKH class can now be found in P2PKHFactory, that requires RTS:

.. code-block:: TypeScript

   import { P2PKHFactory } from "@spedn/rts";

   const factory = new P2PKHFactory(rts);
   const addr = factory.fromKeyPair(key);


Migrating from v0.1 to v0.2
===========================

There are several syntax changes that might cause your contract compiled for v0.1 version of Spedn
doesn't compile anymore. Here's how to fix it:

1. The ``bin`` type has been replced by ``[byte]``. Just replace all occurences.
   This will be good enough but consider being more strict by providing the exact size of the byte array,
   like ``[byte;5]``.

2. Tuple destructuring has a new syntax. Instead of ``bin [a, b]``, use ``([byte] a, [byte] b)``.
   As before, consider being more strict, ex. ``([byte;4] a, [byte;28] b)``.

3. With the November 2019 BCH protcol upgrade, ``OP_CHECKMULTISIG`` started to support Schnorr signatures
   but using this requires providing a checkbits argument instead of null dummy.
   Spedn 0.2 supports this mode exclusively so you'll have to add a checkbits argument.

   Code:

   .. code-block:: c

        challenge(Sig a, Sig b) {
            verify checkMultiSig([a, b], [k1, k2, k3]);
        }

   becomes:

   .. code-block:: c

        challenge([bit;3] checkbits, Sig a, Sig b) {
            verify checkMultiSig(checkbits, [a, b], [k1, k2, k3]);
        }

4. A single code file can now contain multiple contracts therefore the compiler in Spedn TypeScript SDK returns
   a new data structure called *module* instead of a single contract template.
   Instead of ``const MyContract = await compiler.compileFile("./MyContract.spedn");`` use
   ``const { MyContract } = await compiler.compileFile("./MyContract.spedn");``.
