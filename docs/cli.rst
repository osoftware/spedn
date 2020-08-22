======================
Command-line Interface
======================

The general syntax is::

    $ spedn COMMAND args

Compiling
=========

To compile a contract to opcodes, use::

    $ spedn compile -c MyContract.spedn

If the contract contains parameters, a template with placeholders will be generated.
To instantiate the contract with particular parameter values, provide them as ``key=value`` pairs after ``--``.
For example, assuming MyContract has ``alicePHK`` parameter of type ``Ripemd160`` and ``delay`` parameter of type ``TimeSpan``, you can use the following::

    $ spedn compile -c MyContract.spedn -- alicePKH=0xb08f0f859f53873e8f02f6c0a8290a53e76a2e0a delay=1d1h

To compile a contract to a hex representation, use::

    $ spedn compile -f hex -c MyContract.spedn -- alicePKH=0xb08f0f859f53873e8f02f6c0a8290a53e76a2e0a delay=1d1h

Note that in this case, the contract must be fully instantiated (all parameters values must be provided).


Portable
========

Spedn defines custom Script representation called Portable. It's a JSON-based format containing both opcodes and necessary reflection data
allowing it to be instantiated in runtime libraries.

To compile a contract to Spedn Portable format, use::

    $ spedn compile -f portable -c MyContract.spedn > MyCompiledContract.json
