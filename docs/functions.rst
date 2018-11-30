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

* ``Ripemd160 ripemd160(bin bytes)``

  Returns a RIPEMD-160 hash of the argument.

* ``Sha1 sha1(bin bytes)``

  Returns a SHA-1 hash of the argument.

* ``Sha256 sha256(bin bytes)``

  Returns a SHA-256 hash of the argument.

* ``Ripemd160 hash160(bin bytes)``

  Returns RIPEMD-160 hash of SHA-256 hash of the argument.

* ``Ripemd160 hash160(bin bytes)``

  Returns double SHA-256 hash of the argument.

Cryptographic Checks
====================

* ``bool checkSig(Sig sig, PubKey pk)``

  Validates a transaction signature ``sig`` againnst a public key ``pk``.

* ``bool checkMultiSig(List<Sig> sigs, List<PubKey> pks)``

  Validates the set of signatures against the set of public keys.

* ``bool checkDataSig(Sig sig, bin msg, PubKey pk)``

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

* ``bin num2bin(int num, int size)``

  Converts a number ``num`` into a bytes array of size ``size``.

* ``int bin2num(bin data)``

  Converts a bytes array ``data`` to an integer. The array is treated as little-endian.

* ``int size(bin data)``

  Returns the length of ``data``.

Type Constructors
=================

* ``PubKey PubKey(bin data)``
* ``Ripemd160 Ripemd160(bin data)``
* ``Sha1 Sha1(bin data)``
* ``Sha256 Sha256(bin data)``
* ``Sig Sig(bin data)``
* ``Time TimeStamp(int timestamp)``
* ``TimeSpan Blocks(int number)``
