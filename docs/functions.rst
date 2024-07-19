=========
Functions
=========

Math Functions
==============

* ``int abs(int a)``

  Returns an absolute value of the argument.

* ``int min(int a, int b)``

  Returns the smaller argument.

* ``int max(int a, int b)``

  Returns the larger argument.

* ``bool within(int x, int min, int max)``

  Returns ``true`` if ``x >= min && x < max``.


Hashing Functions
=================

* ``Ripemd160 ripemd160([byte] bytes)``

  Returns a RIPEMD-160 hash of the argument.

* ``Sha1 sha1([byte] bytes)``

  Returns a SHA-1 hash of the argument.

* ``Sha256 sha256([byte] bytes)``

  Returns a SHA-256 hash of the argument.

* ``Ripemd160 hash160([byte] bytes)``

  Returns RIPEMD-160 hash of SHA-256 hash of the argument.

* ``Sha256 hash256([byte] bytes)``

  Returns double SHA-256 hash of the argument.

Cryptographic Checks
====================

* ``bool checkSig(Sig sig, PubKey pk)``

  Validates a transaction signature ``sig`` againnst a public key ``pk``.

  * ``bool checkSigAdd(Sig sig, int num, PubKey pk)``

  Validates a transaction signature ``sig`` againnst a public key ``pk``.
  Returns ``num`` if ``sig`` is invalid or ``num+1`` if valid. 

  *Only available in BTC*

* ``bool checkMultiSig([bit;n] flags, [Sig;m] sigs, [PubKey;n] pks)``

  Validates the set of signatures against the set of public keys. 
  Flags indicate which keys will be used.

* ``bool checkDataSig(DataSig sig, [byte] msg, PubKey pk)``

  Validates a signature ``sig`` of an arbitrary message ``msg`` against a public key ``pk``.

  *Not available in BTC*

Timelock Checks
===============

* ``Verification checkLockTime(Time t)``

  Validates whether the spending transaction occurs after time ``t``,
  expressed as a block height or a timestamp.

* ``Verification checkSequence(TimeSpan duration)``

  Validates whether the spending transaction happens after ``duration``
  relative to the locking transaction,
  expressed as a number of blocks or number of 512 seconds-long periods.

Array Operations
================

* ``[byte] num2bin(int num, int size)``

  Converts a number ``num`` into a bytes list. Bytes remain little-endian and are padded with zeros up to ``size``.

  *Not available in BTC*

* ``[byte] Bytes(int num)``

  Reinterprets a number ``num`` as a bytes list without affecting the byte order or size.

* ``int bin2num([byte] data)``

  Converts a bytes list ``data`` to an integer. The list is treated as little-endian. The result is minimally encoded.

  *Not available in BTC*

* | ``[byte] reverseBytes([byte] data)``
  | ``[byte;n] reverseBytes([byte;n] data)``

  Returns an array with bytes in reverse order.

  *Not available in BTC*

  .. code-block:: c

      [byte;3] rev = reverseBytes(0xabcdef);
      // rev == 0xefcdab

* ``int size([byte] data)``

  Returns the length of ``data``.

* ``int checkSize([byte; x] data)``

  Returns true it the runtime size of the byte array matches the declared size ``x``.

* ``a fst((a, b) data)``

  Returns the first element of a tuple.

  .. code-block:: c

      [byte] left = fst(0xaabbccdd @ 2);
      // left == 0xaabb

* ``b snd((a, b) data)``

  Returns the second element of a tuple.

  .. code-block:: c

      [byte] right = snd(0xaabbccdd @ 2);
      // right == 0xccdd

* ``DataSig toDataSig(Sig data)``

  Converts a signature suitable for ``checkSig`` function (with a sighash flag)
  to a signature suitable for ``checkDataSig`` function (without a sighash flag).

  *Not available in BTC*

  .. code-block:: c

      verify checkSig(sig, pubKey);
      verify checkDataSig(toDataSig(sig), preimageHash, pubKey);

Covenant Introspection
======================

*These functions are only available in XEC, BCH and XPI.*

* ``TxState parse(Preimage p)``

  Returns a 10-tuple of preimage components.

* ``NVersion nVersion(Preimage p)``

  Returns nVersion of the transaction (4-byte little endian).

* ``Sha256 hashPrevouts(Preimage p)``

  Returns hashPrevouts.

* ``Sha256 hashSequence(Preimage p)``

  Returns hashSequence.

* ``Outpoint outpoint(Preimage p)``

  Returns outpoint (32-byte hash + 4-byte little endian).

* ``ScriptCode scriptCode(Preimage p)``

  Returns scriptCode of the input (serialized as scripts inside CTxOuts).

* ``Value value(Preimage p)``

  Returns value of the output spent by this input (8-byte little endian).

* ``NSequence nSequence(Preimage p)``

  Returns nSequence of the input (4-byte little endian).

* ``Sha256 hashOutputs(Preimage p)``

  Returns hashOutputs.

* ``NLocktime nLocktime(Preimage p)``

  Returns nLocktime of the transaction.

* ``Sighash sighash(Preimage p)``

  Returns sighash type of the signature (4-byte little endian).

Native Covenant Introspection
==============================

*These functions are only available in XEC, BCH and XPI.*

* ``int inputIndex()``

    Returns the index of the input being evaluated.

* ``ScriptCode activeBytecode()``

    Returns the bytecode currently being evaluated, beginning after the last executed ``OP_CODESEPARATOR``. 
    For Pay-to-Script-Hash (P2SH) evaluations, this is the redeem bytecode of the Unspent Transaction Output (UTXO) being spent.
    For all other evaluations, this is the locking bytecode of the UTXO being spent.

* ``int txVersion()``

    Returns the version of the current transaction.

* ``int txInputCount()``

    Returns the count of inputs in the current transaction.

* ``int txOutputCount()``

    Returns the count of outputs in the current transaction

* ``int txLockTime()``

    Returns the lock time of the current transaction.

* ``int utxoValue(int index)``

    Returns the value (in satoshis) of the Unspent Transaction Output (UTXO) spent by the input at ``index``.

* ``ScriptCode utxoBytecode(int index)``

    Returns the full locking bytecode of the Unspent Transaction Output (UTXO) spent by the input at ``index``.

* ``Sha256 outpointTxHash(int index)``

    Returns the outpoint transaction hash referenced by the input at ``index``.

* ``int outpointIndex(int index)``

    Returns the outpoint index referenced by the input at ``index``.

* ``ScriptCode inputBytecode(int index)``

    Returns the unlocking bytecode of the input at ``index``.

* ``int inputSequenceNumber(int index)``

    Returns the sequence number of the input at ``index``.

* ``int outputValue(int index)``

    Returns the value (in satoshis) of the output at ``index``.

* ``ScriptCode outputBytecode(int index)``

    Returns the locking bytecode of the output at ``index``.

CashTokens Introspection
========================

*These functions are only available in BCH.*

* ``TokenCategory utxoTokenCategory(int index)``

    Get the input at ``index``. 
    If the Unspent Transaction Output (UTXO) spent by that input includes no tokens, returns a ``0``. 
    If the UTXO does not include a non-fungible token with a capability, returns the UTXO’s token category, 
    otherwise, returns the concatenation of the token category and capability, where the mutable capability is represented by ``1`` 
    and the minting capability is represented by ``2``.

* ``[byte] utxoTokenCommitment(int index)``

    Get the input at ``index``. 
    If the UTXO spent by that input includes no tokens, returns an empty byte string.
    If the UTXO includes a non-fungible token, returns the token’s commitment.

* ``int utxoTokenAmount(int index)``

    Get the input at ``index``. 
    If the UTXO spent by that input includes no tokens, returns a ``0``. 
    If the UTXO includes fungible tokens, returns the amount of fungible tokens.

* ``TokenCategory outputTokenCategory(int index)``

    Get the output at ``index``. 
    If the output includes no tokens, returns a ``0``. 
    If the output does not include a non-fungible token with a capability, returns the output’s token category, 
    otherwise, returns the concatenation of the token category and capability, where the mutable capability is represented by ``1`` 
    and the minting capability is represented by ``2``.

* ``[byte] outputTokenCommitment(int index)``

    Get the output at ``index``. 
    If the output includes no tokens, returns an empty byte string. 
    If the output includes a non-fungible token, returns the token’s commitment.

* ``int outputTokenAmount(int index)``

    Get the output at ``index``. 
    If the output includes no tokens, returns a ``0``. 
    If the output includes fungible tokens, returns the amount of fungible tokens.


Type Constructors
=================

* ``PubKey PubKey([byte] data)``
* ``Ripemd160 Ripemd160([byte] data)``
* ``Sha1 Sha1([byte] data)``
* ``Sha256 Sha256([byte] data)``
* ``Sig Sig([byte] data)``
* ``DataSig DataSig([byte] data)``
* ``Time TimeStamp(int timestamp)``
* ``Time TimeStamp(int blockHeight)``
* ``TimeSpan Blocks(int number)``
* ``NVersion NVesrion([byte] data)``
* ``Sha256 HashPrevouts([byte] data)``
* ``Sha256 HashSequence([byte] data)``
* ``Outpoint Outpoint([byte] data)``
* ``ScriptCode ScriptCode([byte] data)``
* ``Value Value([byte] data)``
* ``NSequence NSequence([byte] data)``
* ``Sha256 HashOutputs([byte] data)``
* ``NLocktime NLocktime([byte] data)``
* ``Sighash Sighash([byte] data)``
* ``TokenCategory TokenCategory([byte] data)``
