====================
Understanding Script
====================

Before developing contracts with Spedn it is worth understanding what they are compiled to
and how Bitcoin Cash transactions internally work.

There is no spoon...
====================

From a user perspective it's convenient to perceive a Bitcoin Cash address as a kind of account with a balance.
But this is just a nice abstraction over a mechanism that works in slightly more complicated way.

There is no account. Every transaction contains inputs and outputs. An output consists of an amount of bitcoins
and a script (often called ``scriptPubKey``) specifying some spending conditions for that amount.
An input is a reference to some output of a previous transaction and some script (called ``scriptSig``)
satisfying the spending condition from ``scriptPubKey``.
In a typical transaction, ``scriptPubKey`` contains a public key of a coin owner
and ``scriptSig`` contains a signature matching that public key - hence the names.
An output that is not yet referenced by any other transaction is called *Unspent Transaction Output* (UTXO).

A UTXO can be perceived as a lockbox containing a single coin.

An address is a user readable representation of a standard scriptPubKey.

Kinds of boxes
==============

You can spot two kinds of addresses in Bitcoin Cash:

Pay To Public Key Hash (P2PKH)
------------------------------

This is an "ordinary" address representing a very simple script that checks two condidtions:

* If the public key provided in scriptSig matches the hash in scriptPubKey when hashed with SHA-256 and then RIPEMD-160.
* If the signature provided in scriptSig is valid for that key.

Pay To Script Hash (P2SH)
-------------------------

This is a "smart contract" address.
Instead of public key hash it cointains a hash of an entire script that is called a ``redeem script``.
The scriptSig is supposed to provide the actual script that matches this hash and arguments to it.

Making fancy boxes
==================

All those scripts are bytecode that run in a stack machine.
A human readable representation (assembly language) of this bytecode is called... Script.
Script is a FORTH-like, stack oriented language containing numerous opcodes, some generic, some very Bitcoin-specific.
It intentionally lacks support for recursion what guarantees that all scripts finish
(and even do so in deterministic time).

Writing scripts in Script is quite hard.
This is why Spedn was created. It's a high level language that compiles to Script.
Contracts written in Spedn represent redeem scripts for P2SH addresses.
