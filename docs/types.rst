=====
Types
=====


Basic Types
===========

Basic types reflect types Script operates on.

* **bool** - a boolean value.
  Can be either ``true`` or ``false``.
  ``verify`` and ``if`` statements expect an expression returning this type.

* **int** - a 32-bit signed integer. Literals of this type can be specified in dec or hex.

    .. code-block:: c

        int a = -1234;
        int b = 0xff00i; // notice `i` suffix

* **bin** - an array of bytes. Literals of this type are specified in hex.

    .. code-block:: c

        bin arr = 0x11223344556677889900aabbccddeeff;


Domain-Spcecific Types
======================

To increase safety, Spedn introduces meaningful types that help with catching semantic errors at compile time.

Numeric types
-------------

These types add meaning to a raw ``int``.
They must be explicitly casted from ``int`` with a type constructor.
They cannot be casted back to ``int``.

* **Time** - represents an absolute time.
    Can be expressed as a Unix Timestamp or a Block Height and variously defined.

    .. code-block:: c

        Time x = `2018-10-13 21:37:00`; // defined with a time literal
        Time y = TimeStamp(1539466620); // conversion from `int` interpreted as Unix Timestamp

* **TimeSpan** - represents a relative time period. Can be expressed as a number of blocks or 512-seconds periods.

    .. code-block:: c

        TimeSpan x = 1d 2h 3m 4s; // Time units literal. Be awre that the number will be rounded down to full 512s periods
        TimeSpan y = 10b;         // Blocks literal.
        TimeSpan z = Blocks(10)   // Conversion from `int`


Binary types
------------

These types add meaning to a raw ``bin``.
They can be implicitly casted to ``bin``.
They must be explicitly casted from ``bin`` with a type constructor.

* **PubKey** - represents a public key.

    .. code-block:: c

        PubKey alice = PubKey(0x11223344556677889900aabbccddeeff);

* **Sig** - represents a tx signature (which can be checked with ``checkSig``).

    .. code-block:: c

        Sig alice = Sig(0x11223344556677889900aabbccddeeff);
        verify checkSig(alice, alicePubKey);

* **DataSig** - represents a data signature (which can be checked with ``checkDataSig``).

    .. code-block:: c

        DataSig alice = DataSig(0x11223344556677889900aabbccddee);
        verify checkDataSig(alice, preimageHash, alicePubKey);

* **Ripemd160** - represents a result of RIPEMD-160 hash.

    .. code-block:: c

        Ripemd160 h = hash160(pubKey);

* **Sha1** - represents a result of SHA-1 hash.

    .. code-block:: c

        Sha1 x = sha1(secret);

* **Sha256** - represents a result of SHA-256 hash.

    .. code-block:: c

        Sha256 x = hash256(secret);


Special types
-------------

These are types that can appear in expressions but you cannot define variables of them.

* **List** - can be only created as literals passed to functions that expect them,
    which is currently ``checkMultiSig`` only.

    .. code-block:: c

        verify checkMultiSig([sig1, sig2], [key1, key1]);

* **Verification** - almost like ``bool`` but the only thing you can do with it is to pass it to ``verify``.
    This is a return type of ``checkLockTime`` and ``checkSequence`` functions.:

    .. code-block:: c

        verify checkSequence(8b);
