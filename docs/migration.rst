===============
Migration Guide
===============

Migrating from v0.1 to v0.2
===========================

There are several syntax changes that might cause your contract compiled for v.0.1 version of Spedn
doesn't compile anymore. Here's how to fix it:

1. The ``bin`` type has been replced by ``[byte]``. Just replace all occurences.
   This will be good enough but consider being more strict by providing the exact size of the byte array,
   like ``[byte;5]``.

2. Tuple destructuring has a new syntax. instead of ``bin [a, b]``, use ``([byte] a, [byte] b)``.
   As before, consider being more strict, ex. ``([byte;4] a, [byte;28] b)``.

3. With the November 2019 BCH protcol upgrade, ``OP_CHECKMULTISIG`` started to support Schnorr signatures
   but using this requires providing a checkbits argument instead of null dummy.
   Spedn 0.2 supports this mode exclusively so you'll have to provide this:

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
