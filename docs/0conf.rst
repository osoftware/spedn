==================
Zero Conf Forfeits
==================

This example is based on /u/awemany's proposal for securing 0-conf transactions.
In addition to a regular payment output and a change output we create also a forfeit output.
The forfeit can be ordinarily spent by the customer which would be nonsensical if he also wanted to doublespend.
If the doublespend is actually attempted then the miner can spend the forfeit
by presenting a proof of that.

Read the details `here <https://gist.github.com/awemany/619a5722d129dec25abf5de211d971bd>`_
or watch a `presentation <https://www.youtube.com/watch?v=EsddVkR-MSs>`_.

.. code-block:: c

    contract Forfeit(
        Ripemd160 inputPKH,     // a public key hash used to redeem the input in the payment tx
        Ripemd160 customerPKH   // a public key hash to be used to redeem the forfeit
        ) {

        // This challenge is used by the customer to reclaim the forfeit.
        // Basically, a typical P2PKH.
        challenge ok(PubKey pubKey, Sig sig) {
            verify hash160(pubKey) == customerPKH;
            verify checkSig(sig, pubKey);
        }

        // This challenge can be used by a miner to claim the forfeit
        // if he can prove there was a doublespend attempt.
        challenge fraud(
            Sig paymentSig,         // A signature used in payment transaction
            bin paymentPayload,     // Signed data from the transaction
            Sig doublespendSig,     // Another signature taken from the doublespend attempt
            bin doublespendPayload, // Signed data from the doublespend
            PubKey pubKey           // Public Key matching both signatures
        ) {
            // If the provided PK matches the one from the payment input...
            if (hash160(pubKey) == inputPKH) {
                // verify the signature provided in that payment...
                verify checkDataSig(paymentSig, paymentPayload, pubKey);
                // and that there was seen some other transaction which also validly signed that input...
                verify checkDataSig(doublespendSig, doublespendPayload, pubKey);
            } else {
                // otherwise don't allow to spend it
                verify false;
            }
        }
    }
