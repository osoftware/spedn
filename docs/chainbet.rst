=================
ChainBet Protocol
=================

ChainBet is a proposed Bitcoin Cash protocol to enable on-chain betting.
You can read the details `here <https://github.com/fyookball/ChainBet/blob/master/PROTOCOL.md>`_.

The flow of the bet consists of several steps that can be expressed in Spedn.

Escrow Preparation
==================

Alice Escrow Address
--------------------

The main purpose of Alice's escrow address is to reveal Alice's Secret A when spent.
It will require both Alice and Bob's signature plus the secret.
By requiring the secret, it reveals it to Bob, thus fulfilling that part of the commitment scheme.

Alternatively, Alice can retrieve the funds unilaterally after 8 confirmations
in the situation when Bob abandonds the betting process.

.. code-block:: c

    contract ChainBetAliceEscrow(PubKey alicePK, PubKey bobPK, Ripemd160 commitment) {
        
        challenge cancel(Sig aliceSig) {
            verify checkSequence(8b);
            verify checkSig(aliceSig, alicePK);
        }

        challenge proceed(Sig aliceSig, Sig bobSig, bin secret) {
            verify hash160(secret) == commitment;
            verify checkMultiSig(0b11, [aliceSig, bobSig], [alicePK, bobPK]);
        }
    }


Bob Escrow Address
------------------

The main purpose of Bob's escrow address is to prevent Bob from double spending.
Once the funding transaction is created, Alice's secret will be revealed.
If Bob sees that he has a loss, he could theoretically attempt
to double spend his input to the funding transaction, thereby invalidating it.

By first moving the funds into escrow and requiring Alice's signature in addition to Bob's to spend,
Bob cannot on his own attempt a doublespend.

Of course, it is necessary for the transaction that funds the escrow account
to have at least 1 confirmation before the funding transaction is attempted,
because otherwise Bob could doublespend that, invalidating both itself and the child transaction
(the funding transaction).

Alternatively, Bob can also retrieve his own funds unilaterally after 8 confirmations
in the situation when Alice abandonds the betting process.

.. code-block:: c

    contract ChainBetBobEscrow(PubKey alicePK, PubKey bobPK) {
        
        challenge cancel(Sig bobSig) {
            verify checkSequence(8b);
            verify checkSig(bobSig, bobPK);
        }

        challenge proceed(Sig aliceSig, Sig bobSig) {
            verify checkMultiSig([aliceSig, bobSig], [alicePK, bobPK]);
        }
    }



Phase 5: Funding Transaction
============================

Alice should now have both of Bob's signatures, so she can spend from both escrow addresses
to create the (main) funding transaction.
lice should wait until both escrow transactions have at least one confirmation
before broadcasting the funding transaction.
Otherwise, she risks a double spend attack where Bob learns her secret, discovers he has lost the bet,
and then tries to double spend the input to the Bob escrow account.

Using a shorthand notation where Alice's Secret is "A" and the hash is "HASH_A",
and Bob's Secret is "B" and its hash is "HASH_B",
then we can say that the main P2SH address is a script that allows the funds to be spent if:

Alice can sign for her public key AND Hash(A)= HASH_A AND Hash(B)=HASH_B AND A+B is an odd number.

...or if Bob can sign for his public key AND Hash(A)= HASH_A AND Hash(B)=HASH_B AND A+B is an even number.

...or if Alice can sign for her public key and the transaction is more than 4 blocks old.

.. code-block:: c

    contract Bet(
        Ripemd160 aliceCommitment,
        Ripemd160 bobCommitment,
        PubKey alicePK,
        PubKey bobPK) {

        challenge odd([byte] aliceSecret, [byte] bobSecret, Sig aliceSig, bool cancel) {
            if (!cancel) {
                verify hash160(aliceSecret) == aliceCommitment;
                verify hash160(bobSecret) == bobCommitment;

                ([byte;4] a, _) = aliceSecret @ 4;
                ([byte;4] b, _) = bobSecret @ 4;
                verify (bin2num(a) + bin2num(b)) % 2 == 1;
            }
            else verify checkSequence(8b);

            verify checkSig(aliceSig, alicePK);
        }

        challenge even([byte] aliceSecret, [byte] bobSecret, Sig bobSig) {
            verify hash160(aliceSecret) == aliceCommitment;
            verify hash160(bobSecret) == bobCommitment;

            ([byte;4] a, _) = aliceSecret @ 4;
            ([byte;4] b, _) = bobSecret @ 4;
            verify (bin2num(a) + bin2num(b)) % 2 == 0;

            verify checkSig(bobSig, bobPK);
        }
    }
