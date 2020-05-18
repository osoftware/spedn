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

* ``bool checkMultiSig(List<Sig> sigs, List<PubKey> pks)``

  Validates the set of signatures against the set of public keys.

* ``bool checkDataSig(DataSig sig, [byte] msg, PubKey pk)``

  Validates a signature ``sig`` of an arbitrary message ``msg`` against a public key ``pk``.

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

* ``[byte] Bytes(int num)``

  Reinterprets a number ``num`` as a bytes list without affecting the byte order or size.

* ``int bin2num([byte] data)``

  Converts a bytes list ``data`` to an integer. The list is treated as little-endian. The result is minimally encoded.

* | ``[byte] reverseBytes([byte] data)``
  | ``[byte;n] reverseBytes([byte;n] data)``

  Returns an array with bytes in reverse order.

  .. code-block:: c

      [byte;3] rev = reverseBytes(0xabcdef);
      // rev == 0xefcdab

* ``int size([byte] data)``

  Returns the length of ``data``.

* ``int checkSize([byte; x] data)``

  Returns true it the runtime size of the byte array matches the declared size ``x``.

* ``bin fst([bin, bin] data)``

  Returns the first element of a tuple (result of ``@`` operator).

  .. code-block:: c

      [byte] left = fst(0xaabbccdd @ 2);
      // left == 0xaabb

* ``bin snd([bin, bin] data)``

  Returns the second element of a tuple (result of ``@`` operator).

  .. code-block:: c

      [byte] right = snd(0xaabbccdd @ 2);
      // right == 0xccdd

* ``DataSig toDataSig(Sig data)``

  Converts a signature suitable for ``checkSig`` function (with a sighash flag)
  to a signature suitable for ``checkDataSig`` function (without a sighash flag).

  .. code-block:: c

      verify checkSig(sig, pubKey);
      verify checkDataSig(toDataSig(sig), preimageHash, pubKey);

Covenant Introspection
======================

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


Type Constructors
=================

* ``PubKey PubKey(bin data)``
* ``Ripemd160 Ripemd160(bin data)``
* ``Sha1 Sha1(bin data)``
* ``Sha256 Sha256(bin data)``
* ``Sig Sig(bin data)``
* ``DataSig DataSig(bin data)``
* ``Time TimeStamp(int timestamp)``
* ``Time TimeStamp(int blockHeight)``
* ``TimeSpan Blocks(int number)``
